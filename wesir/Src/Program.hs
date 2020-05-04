module Main where
   
import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt

import Control.Monad(when)
import Options.Applicative
import Data.Semigroup ((<>))

import CommandLine(opts,Arguments(..),ConnectionString,ConnectionProperties(..),printArgs) 

import Database.HDBC.ODBC
import Database.HDBC

main :: IO ()
main = do 
    args <- execParser opts
    printArgs args
    conn <- getConnection args
    putStrLn "\n\nbye, have a great time!"

getConnection :: Arguments -> IO Connection
getConnection (ArgumentsConnectionString c v) = connectODBC (Txt.unpack c)
getConnection (ArgumentsConnectionProps (Connection h p u pw) v) = 
    let cs = "DRIVER={MariaDB};SERVER="++h++";PORT="++p++";USER="++u++";PASSWORD="++pw
    in connectODBC cs 