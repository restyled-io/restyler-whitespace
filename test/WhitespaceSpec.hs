{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module WhitespaceSpec
  ( spec
  )
where

import Relude

import Control.Monad.Logger (runNoLoggingT)
import Test.Hspec
import Whitespace

spec :: Spec
spec = do
  let opts =
        FormatOptions
          { spaces = True
          , newlines = True
          , strict = True -- Unused
          , paths = [] -- Unused
          }

  describe "formatPath" $ do
    it "doesn't incorrectly format non-utf8" $ do
      let path = "test/fixtures/non-utf8.yaml"

      runNoLoggingT (formatPath opts path) `shouldThrow` \case
        UnableToRead _ -> True
        _ -> False

  describe "format" $ do
    it "strips trailing whitespace from the given content" $ do
      let
        content =
          mconcat
            [ "line one  "
            , "\nline two "
            , "\n "
            , "\nline three \\" -- preserved
            , "\n"
            ]
        expected =
          mconcat
            [ "line one"
            , "\nline two"
            , "\n"
            , "\nline three \\"
            , "\n"
            ]

      format opts content `shouldBe` expected
      format opts {spaces = False} content `shouldBe` content

    it "strips extra newlines from the end of the content" $ do
      let
        content =
          mconcat
            [ "line one"
            , "\nline two"
            , "\n"
            , "\n"
            , "\nline three"
            , "\n"
            , "\n"
            , "\n"
            ]
        expected =
          mconcat
            ["line one", "\nline two", "\n\n", "\nline three", "\n"]

      format opts content `shouldBe` expected
      format opts {newlines = False} content `shouldBe` content

    it "does not affect completely empty files" $ do
      format opts "" `shouldBe` ""
