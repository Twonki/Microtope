module Checker where 

import Database.HDBC   
import Database.HDBC.ODBC
import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt
import Data.Int
import Data.ByteString.Char8 hiding (map,groupBy,filter,putStrLn)
import Data.List.GroupBy
import Data.List (sortBy)

routine :: Connection -> [[SqlValue]] -> IO ()
routine conn rs = do
    let 
        entries = catchValues rs
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




data Entry = Login Int32 Int32 LocalTime 
           | Logout Int32 Int32 LocalTime 
    deriving (Show,Eq)

getTime :: Entry -> LocalTime
getTime (Login _ _ t) = t 
getTime (Logout _ _ t) = t 

type EntryPair = (Entry,Entry)

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



markEntryChecked :: Connection -> Entry -> IO Integer
markEntryChecked conn (Login id _ _)  = run conn "UPDATE audits SET controlled=True WHERE audit_id =?;" [(SqlInt32 id)]
markEntryChecked conn (Logout id _ _) = run conn "UPDATE audits SET controlled=True WHERE audit_id =?;" [(SqlInt32 id)]

markPairChecked :: Connection -> EntryPair -> IO Integer
markPairChecked conn (a,b) = do 
    i1 <- markEntryChecked conn a 
    i2 <- markEntryChecked conn b
    return $ i1 + i2

sameUser :: Entry -> Entry -> Bool
sameUser (Login _ u1 _) (Logout _ u2 _) = u1 == u2 
sameUser (Login _ u1 _) (Login _ u2 _) = u1 == u2
sameUser (Logout _ u1 _) (Logout _ u2 _) = u1 == u2 
sameUser (Logout _ u1 _) (Login _ u2 _) = u1 == u2

valid :: EntryPair -> Bool 
valid (Login _ u1 d1 , Logout _ u2 d2 ) = u1 == u2 && d2 > d1 
valid _ = False