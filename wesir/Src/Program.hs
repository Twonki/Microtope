module Main where
   
import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt

import Control.Monad(when)
import Options.Applicative
import Data.Semigroup ((<>))

import CommandLine(opts,Arguments(..),ConnectionString,ConnectionProperties(..),printArgs) 
import Checker

import Database.HDBC.ODBC
import Database.HDBC

main :: IO ()
main = do 
    args <- execParser opts
    printArgs args
    conn <- getConnection args

    ((bs@(_):[]):[]) <- quickQuery conn "SELECT * FROM health;" []
    let 
        status :: Txt.Text
        status = fromSql bs
    putStrLn ("Database Health: " ++ (Txt.unpack status)++"\n")

    entries <- quickQuery conn "SELECT * FROM audits WHERE controlled=False" []
    routine conn entries

    disconnect conn
    putStrLn "\nbye, have a great time!"

getConnection :: Arguments -> IO Connection
getConnection (ArgumentsConnectionString c v) = connectODBC (Txt.unpack c)
getConnection (ArgumentsConnectionProps (Connection h p u pw d) v) = 
    let cs = "DRIVER={MariaDB};SERVER="++h++";PORT="++p++";USER="++u++";PASSWORD="++pw++";DATABASE="++d
    in connectODBC cs 