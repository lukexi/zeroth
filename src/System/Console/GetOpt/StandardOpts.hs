{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Console.GetOpt.StandardOpts where

import Control.Applicative    ( (<$>) )
import Distribution.Text      ( display )
import Distribution.Version   ( Version )
import System.Console.GetOpt  ( ArgDescr (..), OptDescr (..), usageInfo )

data StandardFlag = HelpFlag | VersionFlag

standardOptions :: [OptDescr StandardFlag]
standardOptions = [ Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"
                  , Option "v" ["version"] (NoArg VersionFlag) "Show the version number of this program"
                  ]

stdOpts :: (StandardFlag -> a) -> [OptDescr a]
stdOpts f = (f <$>) <$> standardOptions

printHelp :: String -> [OptDescr a] -> IO ()
printHelp progName = putStrLn . usageInfo ("Usage:\n\t" ++ progName ++ " [FLAGS]\n")

printVersion :: String -> Version -> IO ()
printVersion name version = putStrLn $ name ++ ' ' : display version
