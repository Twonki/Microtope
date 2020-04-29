module Main where

import Data.Time 
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt

import Control.Monad(when)

import Options.Applicative
import Data.Semigroup ((<>))

-- The way to parse arguments is taken from 
-- https://hackage.haskell.org/package/optparse-applicative
-- Which is a proper library 
-- This is just a variation of the example given, so you might be enlightend visiting their documentation

data Arguments = Arguments
  { connectionstring :: String
  , host :: String  
  , port :: String  
  , user :: String
  , password :: String
  , verbose :: Bool
  }

args' :: Parser Arguments
args' = Arguments
    <$> strOption
        ( long "connectionstring"
        <> short 'c'
        <> metavar "STRING"
        <> help "Connectionstring to the mariadb database - if this option is given, the host and port are ignored" )
        -- <> value "error"
    <*> strOption
        ( long "host"
        <> short 'h'
        <> help "address of the database to connect to"
        <> showDefault
        <> value "127.0.0.1"
        <> metavar "STRING" )    
    <*> strOption
        ( long "port"
        <> short 'p'
        <> help "port of the database to connect to"
        <> showDefault
        <> value "jaccard"
        <> metavar "STRING" )    
    <*> strOption
        ( long "user"
        <> short 'u'
        <> help "The username that will connect to the database"
        <> showDefault
        <> value "wesir"
        <> metavar "STRING" )    
    <*> strOption
        ( long "password"
        -- <> short "pw"
        <> help "The password to use to connect to the database"
        <> showDefault
        <> value "wesir"
        <> metavar "STRING" )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "whether to print debug information" )



main :: IO ()
main = printArgs =<< execParser opts 
    where 
        opts = info (args' <**> helper) (fullDesc<> progDesc "TODO: add after-usage-description"<> header "TODO: Add Toplevel description")


printArgs :: Arguments -> IO ()
printArgs args = do 
    printTimestamp "Hello"

printTimestamp :: String -> IO ()
printTimestamp comment = do 
    t <- getCurrentTime
    putStrLn $ comment ++ " at"
    print t
    return ()