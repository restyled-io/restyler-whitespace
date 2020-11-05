{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module WhitespaceSpec
    ( spec
    )
where

import RIO

import Test.Hspec
import Whitespace

spec :: Spec
spec = do
    let
        opts = FormatOptions
            { foSpaces = True
            , foNewlines = True
            , foStrict = True -- Unused
            , foPaths = [] -- Unused
            }

    describe "formatPath" $ do
        it "doesn't incorrectly format non-utf8" $ do
            let path = "test/fixtures/non-utf8.yaml"

            runSimpleApp (formatPath opts path) `shouldThrow` \case
                UnableToRead _ -> True
                _ -> False

    describe "format" $ do
        it "strips trailing whitespace from the given content" $ do
            let content = mconcat
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
            format opts { foSpaces = False } content `shouldBe` content

        it "strips extra newlines from the end of the content" $ do
            let content =
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
                expected = mconcat
                    ["line one", "\nline two", "\n\n", "\nline three", "\n"]

            format opts content `shouldBe` expected
            format opts { foNewlines = False } content `shouldBe` content

        it "does not affect completely empty files" $ do
            format opts "" `shouldBe` ""
