module CommandLine where 

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


{--
Arguments can either be the ConnectionString + Verbose
or they can be a quadruple of host,port,user and password.

This is reflected in the following datatypes and is used to build two parsers for command arguments.
--}
type ConnectionString = Txt.Text 
data ConnectionProperties = Connection {
    host :: String  
  , port :: String  
  , user :: String
  , password :: String
  , database :: String
}

data Arguments = 
    ArgumentsConnectionString ConnectionString Bool 
    | ArgumentsConnectionProps ConnectionProperties Bool 

{--
The argument parser first tries to read the connectionStringInput, and otherwise tries to read the connectionPropsInput.

The ConnectionPropsInput is always filled with default variables.
--}
args' :: Parser Arguments 
args' = connectionStringInput <|> connectionPropsInput

connectionPropsInput :: Parser Arguments
connectionPropsInput = ArgumentsConnectionProps <$>
    (Connection
        <$> strOption
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
            <> value "3306"
            <> metavar "STRING" )    
        <*> strOption
            ( long "user"
            <> short 'u'
            <> help "The username that will connect to the database"
            <> showDefault
            <> value "auditor"
            <> metavar "STRING" )    
        <*> strOption
            ( long "password"
            <> help "The password to use to connect to the database"
            <> value "ARGU5"
            <> metavar "STRING" )
        <*> strOption
            ( long "database"
            <> short 'd'
            <> help "the database in the db server to connect to"
            <> value "microtope"
            <> metavar "STRING" )
    )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "whether to print debug information" )


connectionStringInput :: Parser Arguments
connectionStringInput = ArgumentsConnectionString 
    <$> strOption
        ( long "connectionstring"
        <> short 'c'
        <> metavar "STRING"
        <> help "Connectionstring to the mariadb database - if this option is given, the host and port are ignored" )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "whether to print debug information" )

printArgs :: Arguments -> IO ()
printArgs (ArgumentsConnectionString constr v) = 
    printTimestamp "Using ConnectionString"
printArgs (ArgumentsConnectionProps (Connection h p u pw d) v) = 
    printTimestamp "Using ConnectionProps"


-- Little Helper to print statements such as "Connection done at 13:15"
printTimestamp :: String -> IO ()
printTimestamp comment = do 
    t <- getCurrentTime
    putStrLn $ comment ++ " at"
    print t
    return ()

opts = info (args' <**> helper) (fullDesc<> progDesc "TODO: add after-usage-description"<> header "TODO: Add Toplevel description")