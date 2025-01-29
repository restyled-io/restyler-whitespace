module Whitespace
  ( FormatOptions (..)
  , formatPaths
  , formatPath
  , format
  -- Exported for testing error-handling
  , UnableToFormat (..)
  )
where

import Relude

import Blammo.Logging
import Blammo.Logging.ThreadContext
import Data.Char (isSpace)
import Data.Text qualified as T
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (handleAny, throwIO)

data FormatOptions = FormatOptions
  { spaces :: Bool
  -- ^ Trim trailing whitespace from lines?
  , newlines :: Bool
  -- ^ Fix newlines at end of file?
  , strict :: Bool
  -- ^ Halt on errors reading files?
  , paths :: [FilePath]
  -- ^ Files to process
  }

formatPaths
  :: (MonadLogger m, MonadMask m, MonadUnliftIO m)
  => FormatOptions
  -> m ()
formatPaths opts = for_ opts.paths $ \path ->
  handleAny (handleErr opts.strict path) $ formatPath opts path

data UnableToFormat
  = UnableToFormatCRLF
  | UnableToRead UnicodeException
  deriving stock (Show)
  deriving anyclass (Exception)

formatPath :: MonadUnliftIO m => FormatOptions -> FilePath -> m ()
formatPath opts path = do
  result <- decodeUtf8Strict <$> readFileBS path
  content <- either (throwIO . UnableToRead) pure result
  if isCRLF content
    then throwIO UnableToFormatCRLF
    else writeFileText path $ format opts content

isCRLF :: Text -> Bool
isCRLF = ("\r\n" `T.isInfixOf`)

format :: FormatOptions -> Text -> Text
format opts t
  | T.null t = t
  | otherwise = onOpt (.newlines) newlines $ onOpt (.spaces) spaces t
 where
  onOpt attr f = bool id f $ attr opts

-- | Ensure a single trailing newline
newlines :: Text -> Text
newlines = (<> "\n") . T.dropWhileEnd (== '\n')

-- | Trim whitespace from the end of all lines
spaces :: Text -> Text
spaces = eachLine $ T.dropWhileEnd isSpace

eachLine :: (Text -> Text) -> Text -> Text
eachLine f = T.unlines . map f . T.lines

handleErr
  :: (Exception ex, MonadIO m, MonadLogger m, MonadMask m)
  => Bool
  -> FilePath
  -> ex
  -> m ()
handleErr strict path ex = withThreadContext ctx $ do
  if strict
    then do
      logError "Exception processing file, aborting (disable strict mode to ignore)"
      exitFailure
    else logWarn "Exception processing file"
 where
  ctx =
    [ "path" .= path
    , "exception" .= displayException ex
    ]
