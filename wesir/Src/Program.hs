module Main where
   
import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt

import Control.Monad(when)
import Options.Applicative
import Data.Semigroup ((<>))

import CommandLine(opts,Arguments(..),ConnectionString,ConnectionProperties,printArgs) 

import Database.HDBC.ODBC
import Database.HDBC

main :: IO ()
main = do 
    args <- execParser opts
    printArgs args
    conn <- connectODBC "DRIVER={MariaDB}"
    print "kek"
