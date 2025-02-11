module Main
  ( main
  )
where

import Relude

import Blammo.Logging.Simple
import Data.Version (showVersion)
import Options.Applicative
import Paths_whitespace qualified as Pkg
import Whitespace

main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper) fullDesc

  runSimpleLoggingT $ do
    if opts.showVersion
      then putStrLn $ "whitespace " <> showVersion Pkg.version
      else formatPaths opts.formatOptions

data Options = Options
  { showVersion :: Bool
  , formatOptions :: FormatOptions
  }

options :: Parser Options
options =
  Options
    <$> switch (long "version" <> help "Show version")
    <*> formatOptions

formatOptions :: Parser FormatOptions
formatOptions =
  FormatOptions
    <$> (not <$> switch (long "no-remove-spaces" <> help "Don't remove trailing spaces"))
    <*> (not <$> switch (long "no-fix-newlines" <> help "Don't fix ending newlines"))
    <*> switch (long "strict" <> help "Abort on exceptions")
    <*> many (argument str (metavar "PATH" <> help "File to fix (inplace)"))
