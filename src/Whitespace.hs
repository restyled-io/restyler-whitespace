module Whitespace
    ( FormatOptions(..)
    , formatPaths
    , format
    )
where

import RIO

import qualified Data.ByteString.Char8 as C8
import Data.Char (isSpace)
import qualified RIO.ByteString as BS

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

data UnableToFormat = UnableToFormatCRLF
    deriving stock Show
    deriving anyclass Exception

formatPath :: MonadUnliftIO m => FormatOptions -> FilePath -> m ()
formatPath opts path = do
    content <- BS.readFile path
    if isCRLF content
        then throwIO UnableToFormatCRLF
        else BS.writeFile path $ format opts content

isCRLF :: ByteString -> Bool
isCRLF = ("\r\n" `C8.isInfixOf`)

format :: FormatOptions -> ByteString -> ByteString
format opts bs
    | BS.null bs = bs
    | otherwise = onOpt foNewlines newlines $ onOpt foSpaces spaces bs
    where onOpt attr f = bool id f $ attr opts

-- | Ensure a single trailing newline
newlines :: ByteString -> ByteString
newlines = withEnd $ C8.cons '\n' . C8.dropWhile (== '\n')

-- | Trim whitespace from the end of all lines
spaces :: ByteString -> ByteString
spaces = eachLine $ withEnd $ C8.dropWhile isSpace

eachLine :: (ByteString -> ByteString) -> ByteString -> ByteString
eachLine f = C8.unlines . map f . C8.lines

withEnd :: (ByteString -> ByteString) -> ByteString -> ByteString
withEnd f = C8.reverse . f . C8.reverse

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
