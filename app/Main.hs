module Main
    ( main
    )
where

import RIO

import Options.Applicative
import Whitespace

main :: IO ()
main = do
    opts <- execParser $ info (options <**> helper) fullDesc
    runSimpleApp $ formatPaths opts

-- brittany-disable-next-binding

options :: Parser FormatOptions
options =
    FormatOptions
        <$> (not <$> switch (long "no-remove-spaces" <> help "Don't remove trailing spaces"))
        <*> (not <$> switch (long "no-fix-newlines" <> help "Don't fix ending newlines"))
        <*> switch (long "strict" <> help "Abort on exceptions")
        <*> many (argument str (metavar "PATH" <> help "File to fix (inplace)"))
