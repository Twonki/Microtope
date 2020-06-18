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

routine :: Connection -> IO ()
routine conn = do
    uncheckedResults <- quickQuery conn "SELECT * FROM audits WHERE controlled=False" []

    let 
        entries = catchValues uncheckedResults
        groupedEntries = groupEntries entries
        goodies = filter valid (mconcat groupedEntries)
        baddies = filter (not . valid) (mconcat groupedEntries)
    --TODO:
    -- Parse Values in my Datatype
    -- find entry-pairs (group by user, sort by time, fold with (,) )
    -- Sort entries in valid and invalid
    -- (Delete invalid time-entries?)
    -- update entries to be checked
    

    putStrLn $ "These are good: \n" ++ show goodies
    putStrLn $ "These are bad: \n" ++ show baddies

    putStrLn "\nMarking goodies checked..."
    goodEntriesChanged <- mapM (markPairChecked conn) goodies

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


-- | Markes an entry as "controlled", that is, everything is ok and every login as a matching logout. 
markEntryChecked :: Connection -> Entry -> IO Integer
markEntryChecked conn (Login id _ _)  = run conn "UPDATE audits SET controlled=True WHERE audit_id =?;" [SqlInt32 id]
markEntryChecked conn (Logout id _ _) = run conn "UPDATE audits SET controlled=True WHERE audit_id =?;" [SqlInt32 id]

-- | Applies "markEntryChecked" to both entries of a login/logout pair 
markPairChecked :: Connection -> EntryPair -> IO Integer
markPairChecked conn (a,b) = do 
    i1 <- markEntryChecked conn a 
    i2 <- markEntryChecked conn b
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