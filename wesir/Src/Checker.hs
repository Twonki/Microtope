module Checker (healthcheck, routine) where 

import Database.HDBC   
import Database.HDBC.ODBC
import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt
import Data.Int

import Control.Monad(when)
import Data.ByteString.Char8 hiding (map,groupBy,filter,putStrLn)
import Data.List.GroupBy
import Data.List (sortBy)

import CommandLine(Arguments(..),isVerbose)

-- | This is the primary routine for the wesir application.
-- 
-- It does the following steps:
-- 1. Query the all unchecked results from the database
-- 2. Group entries into users and adjacent-pairs
-- 3. Check every entry-pair if they are valid (see method "valid" below)
-- 4. Mark all good entries as such in the database
-- 5. handle bad entries, delete the negative parts while keeping "ends"
-- 
-- For the exact behavior of "handling baddies" see the matching function. 
-- It needs to be done after checking the good ones. 
-- 
-- The routine expects the database to show only checked entries towards the user. 
-- It does therefore not try to alter all other types of database entries such as steps, coins and teams which occurred in an invalid time window. 
-- This behavior makes the approach more extensible and puts these responisbilities towards the database - where it is easier to change and adjust data. 
-- Otherwise, with every change of the datamodel, this file is likely to be changed too. With the chosen behaviour it is enough to change this only iff the login model is changed. 
routine :: Connection -> Arguments -> IO ()
routine conn args = do
    uncheckedResults <- quickQuery conn "SELECT * FROM audits WHERE controlled=False" []

    let 
        entries = catchValues uncheckedResults
        groupedEntries = groupEntries entries 
        goodies = mconcat $ map fst groupedEntries 
        baddies = mconcat $ map snd groupedEntries
    
    when (isVerbose args) (printGoodiesAndBaddies goodies baddies)

    putStrLn "\nMarking goodies checked..."
    goodEntriesChanged <- mapM (checkPair conn) goodies
    putStrLn "Number of entries marked checked:"
    print (sum goodEntriesChanged)
    commit conn

    putStrLn "\nDeleting baddies ..."
    badEntriesDeleted <- mapM (deleteEntry conn) baddies
    putStrLn "Number of entries deleted:"
    print (sum badEntriesDeleted)
    commit conn

    return ()
    
    where 
        printGoodiesAndBaddies :: [EntryPair] -> [Entry] -> IO ()
        printGoodiesAndBaddies goodies baddies = do
                putStrLn "These are good:\n"
                mapM_ putStrLn (map show goodies)
                putStrLn "These are bad: \n" 
                mapM_ putStrLn (map show baddies)

healthcheck :: Connection -> IO () 
healthcheck conn = do 
    [[bs]] <- quickQuery conn "SELECT * FROM health;" []
    let 
        status :: Txt.Text
        status = fromSql bs
    putStrLn ("Database Health: " ++ (Txt.unpack status)++"\n")


-- | This Datatype holds values regarding the logins and logouts and directly match the table entries. 
-- The only difference to a single row is that they do not have a checked-field (as we only get unchecked values), and the field "login or logout" from the database is represented as the type for better pattern match.
data Entry = Login Int32 Int32 LocalTime 
           | Logout Int32 Int32 LocalTime 
    deriving (Show,Eq)

getTime :: Entry -> LocalTime
getTime (Login _ _ t) = t 
getTime (Logout _ _ t) = t 

type EntryPair = (Entry,Entry)

-- | This function maps the SQL-Rows to Entries.
-- It has intentionally no pattern-match for invalid SQL-Rows, so any error arising will be passed towards the user.
catchValues :: [[SqlValue]] -> [Entry]
catchValues = map go
    where 
        go :: [SqlValue] -> Entry
        go [SqlInt32 aId,SqlInt32 uId,SqlByteString msg,SqlLocalTime t,_] 
            | msg == pack "login" = Login aId uId t
            | msg == pack "logout" = Logout aId uId t

-- | Takes a list of all unchecked entries 
-- Groups them by player
-- Sorts them by date 
-- Makes Entry touples 
-- Discards last entry if it was a login (read: the logout is still pending, player is playing)
groupEntries :: [Entry] -> [([EntryPair],[Entry])]
groupEntries entries = 
                let 
                    userEntries = groupBy sameUser entries
                    sortedUserEntries = map (sortBy (\a b -> compare (getTime a) (getTime b))) userEntries
                in map findValids sortedUserEntries

-- | This function groups the entries of a player into valid and invalid pairs. 
-- If it finds an anomaly (such as Login1 - Login2 - Logout1 - Logout2), it tries to find the best non-greedy match. 
-- That is the pair (Login2,Logout1) will be added to "goodies" and (Login1,Logout2) will be added to "baddies".
--
-- This function requires the Entries to be of the same player and sorted by date ascending - it does not check for these properties. 
findValids :: [Entry] -> ([EntryPair],[Entry])
findValids [] = ([],[])
findValids [(Login _ _ _)] = ([],[]) -- Last element is a login - maybe the player is playing. Do not add this to the baddies
findValids bads@[(Logout _ _ _)] = ([],bads) -- Last element is a logout - this is an error - add it to the baddies
findValids [lin@(Login _ _ _),lout@(Logout _ _ _)] = ([(lin,lout)],[]) -- Trivial case - there is a valid pair of entries and we make a new accumulator value.
findValids [bad,l@(Login _ _ _)] = ([],[bad]) -- All other 2 element lists which and with a login are only bads
findValids bads@[b1,b2] = ([],bads)
-- After checking for trivial cases, we need to check for exactly 3 elements
findValids [a,b,c] 
    | valid (a,b) = addToGoodies (a,b) $ findValids [c]
    | valid (b,c) = addToBaddies a $ addToGoodies (b,c) $ findValids []
    | otherwise   = addToBaddies a $ addToBaddies b $ addToBaddies c $ findValids []

-- most general check for 4 or more elements
findValids (a:b:c:d:others) 
    | valid (a,b) = addToGoodies (a,b) $ findValids (c:d:others)
    | valid (b,c) = addToBaddies a $ addToGoodies (b,c) $ findValids (d:others)
    | valid (c,d) = addToBaddies a $ addToBaddies b $ addToGoodies (c,d) $ findValids others
    | otherwise   = addToBaddies a $ addToBaddies b $ addToBaddies c $ findValids (d:others)

-- | Takes a accumulator of goodies and baddies, and adds another entry to the baddies
addToBaddies :: Entry -> ([EntryPair],[Entry]) -> ([EntryPair],[Entry])
addToBaddies bad (goodies,baddies) = (goodies,bad:baddies)
-- | Takes a accumulator of goodies and baddies, and adds another entryPair to the goodies
addToGoodies :: EntryPair -> ([EntryPair],[Entry]) -> ([EntryPair],[Entry])
addToGoodies good (goodies,baddies) = (good:goodies,baddies)


--findValids _ = undefined 
-- | Markes an entry as "controlled", that is, everything is ok and every login as a matching logout. 
checkEntry :: Connection -> Entry -> IO Integer
checkEntry conn (Login id _ _)  = run conn "UPDATE audits SET controlled=True WHERE audit_id =?;" [SqlInt32 id]
checkEntry conn (Logout id _ _) = run conn "UPDATE audits SET controlled=True WHERE audit_id =?;" [SqlInt32 id]

-- | Applies "checkEntry" to both entries of a login/logout pair
-- This is used to mark good pairs
checkPair :: Connection -> EntryPair -> IO Integer
checkPair conn (a,b) = do 
    i1 <- checkEntry conn a 
    i2 <- checkEntry conn b
    return $ i1 + i2

-- | Deletes and entry from the table. It is used for invalid or bad entries.
-- The database uses versioning - therefore it is possible to view and retrieve entries which where deleted.
deleteEntry :: Connection -> Entry -> IO Integer
deleteEntry conn (Login id _ _)  = run conn "DELETE FROM audits WHERE audit_id =?;" [SqlInt32 id]
deleteEntry conn (Logout id _ _) = run conn "DELETE FROM audits WHERE audit_id =?;" [SqlInt32 id]

-- | Utility function to compare users of two entries.
sameUser :: Entry -> Entry -> Bool
sameUser (Login _ u1 _) (Logout _ u2 _) = u1 == u2 
sameUser (Login _ u1 _) (Login _ u2 _) = u1 == u2
sameUser (Logout _ u1 _) (Logout _ u2 _) = u1 == u2 
sameUser (Logout _ u1 _) (Login _ u2 _) = u1 == u2

-- | Checks an entrypair whether they are login+logout, have the same user, and the logout date is after the login date. 
-- Everything that does not match this pattern is considered invalid.
valid :: EntryPair -> Bool 
valid (Login _ u1 d1 , Logout _ u2 d2 ) = u1 == u2 && d2 > d1 
valid _ = False