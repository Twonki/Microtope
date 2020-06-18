module Checker (healthcheck, routine) where 

import Database.HDBC   
import Database.HDBC.ODBC
import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt
import Data.Int
import Data.ByteString.Char8 hiding (map,groupBy,filter,putStrLn)
import Data.List.GroupBy
import Data.List (sortBy)

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
routine :: Connection -> IO ()
routine conn = do
    uncheckedResults <- quickQuery conn "SELECT * FROM audits WHERE controlled=False" []

    let 
        entries = catchValues uncheckedResults
        groupedEntries = groupEntries entries
        goodies = filter valid (mconcat groupedEntries)
        baddies = filter (not . valid) (mconcat groupedEntries)

    putStrLn $ "These are good: \n" ++ show goodies
    putStrLn $ "These are bad: \n" ++ show baddies

    putStrLn "\nMarking goodies checked..."
    goodEntriesChanged <- mapM (checkPair conn) goodies

    putStrLn "Number of entries marked checked:"
    print (sum goodEntriesChanged)

    commit conn

    -- TODO: Behavior for baddies
    -- Delete everything in the range?

    return ()

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
groupEntries :: [Entry] -> [[EntryPair]]
groupEntries es = let 
                    es' = groupBy sameUser es
                    es'' = map (sortBy (\a b -> compare (getTime a) (getTime b))) es'
                  in go <$> es'' 
                  where 
                      go :: [Entry] -> [EntryPair]
                      go (a:b:r) = (a,b) : go r
                      -- This either happens if the list is empty, or if there is only one login "pending"
                      go _ = []

-- | This function groups the entries of a player into valid and invalid pairs. 
-- If it finds an anomaly (such as Login1 - Login2 - Logout1 - Logout2), it tries to find the best non-greedy match. 
-- That is the pair (Login2,Logout1) will be added to "goodies" and (Login1,Logout2) will be added to "baddies".
--
-- This function requires the Entries to be of the same player and sorted by date ascending - it does not check for these properties. 
findValids :: [Entry] -> [EntryPair]
findValids [] = []
findValids [lin@(Login _ _ _),lout@(Logout _ _ _)] = [(lin,lout)]
findValids (a:b:c:d:others) 
    if valid (a,b) -- First case - The first two Elements are already perfect and valid. Use these as the first 
    then (a,b) : findValids (c:d:others)
    else 
        if valid (c,d) -- Second case - the third and fourth entry 
        then (c,d) : findValids (a:b:others)
        else undefined 
--findValids _ = undefined 
findValids [l] = [] -- Last element, this can only be case if the last entry is a login, as logout is earlier pattern-matched
 
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

-- | Applies "deleteEntry" to both entries of a login/logout pair
-- This is used to delete bad pairs and invalid logins 
deletePair :: Connection -> EntryPair -> IO Integer
deletePair conn (a,b) = do 
    i1 <- deleteEntry conn a 
    i2 <- deleteEntry conn b
    return $ i1 + i2

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