module Whitespace
    ( FormatOptions(..)
    , formatPaths
    , formatPath
    , format

    -- Exported for testing error-handling
    , UnableToFormat(..)
    )
where

import RIO

import RIO.Char (isSpace)
import qualified RIO.Text as T

data FormatOptions = FormatOptions
    { foSpaces :: Bool -- ^ Trim trailing whitespace from lines?
    , foNewlines :: Bool -- ^ Fix newlines at end of file?
    , foStrict :: Bool -- ^ Halt on errors reading files?
    , foPaths :: [FilePath] -- ^ Files to process
    }

formatPaths
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => FormatOptions
    -> m ()
formatPaths opts = for_ (foPaths opts) $ \path ->
    handleAny (handleErr (foStrict opts) path) $ formatPath opts path

data UnableToFormat
    = UnableToFormatCRLF
    | UnableToRead SomeException -- ^ Most likely non-Utf8
    deriving stock Show
    deriving anyclass Exception

formatPath :: MonadUnliftIO m => FormatOptions -> FilePath -> m ()
formatPath opts path = do
    content <- readFileUtf8 path `catchAny` (throwIO . UnableToRead)
    if isCRLF content
        then throwIO UnableToFormatCRLF
        else writeFileUtf8 path $ format opts content

isCRLF :: Text -> Bool
isCRLF = ("\r\n" `T.isInfixOf`)

format :: FormatOptions -> Text -> Text
format opts t
    | T.null t = t
    | otherwise = onOpt foNewlines newlines $ onOpt foSpaces spaces t
    where onOpt attr f = bool id f $ attr opts

-- | Ensure a single trailing newline
newlines :: Text -> Text
newlines = (<> "\n") . T.dropWhileEnd (== '\n')

-- | Trim whitespace from the end of all lines
spaces :: Text -> Text
spaces = eachLine $ T.dropWhileEnd isSpace

eachLine :: (Text -> Text) -> Text -> Text
eachLine f = T.unlines . map f . T.lines

handleErr
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Display ex)
    => Bool
    -> FilePath
    -> ex
    -> m ()
handleErr strict path ex
    | strict
    = do
        logError
            $ "Exception processing "
            <> fromString path
            <> ":"
            <> display ex
            <> ", aborting (disable strict mode to ignore)"
        exitWith $ ExitFailure 1
    | otherwise
    = logWarn $ "Exception processing " <> fromString path <> ":" <> display ex
