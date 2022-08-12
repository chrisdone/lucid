{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Test suite for Lucid.

module Main where

import Lucid
import Lucid.Base

import Control.Applicative
import Control.Monad.State.Strict

import qualified Data.Text as T

import Test.HUnit
import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "text" testText
  describe "elements" testElements
  describe "attributes" testAttributes
  describe "special-elements" testSpecials
  describe "self-closing" testSelfClosing
  describe "commute" testCommute

(==?*) :: (Eq a, Show a) => a -> [a] -> Assertion
x ==?* xs | x `elem` xs = return ()
          | otherwise   = assertFailure $ show x ++ " is not equal to any of " ++ show xs

-- | Test text/unicode.
testText :: Spec
testText =
  do it "simple"
        (renderText "foo" `shouldBe`
         "foo")
     it "escaping"
        (renderText "'<>" `shouldBe`
         "&#39;&lt;&gt;")
     it "unicode"
        (renderText "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381" `shouldBe`
         "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381")

-- | Test basic elements and nesting.
testElements :: Spec
testElements =
  do it "simple"
        (renderText (p_ "foo") `shouldBe`
         "<p>foo</p>")
     it "escaping"
        (renderText (p_ "'<>") `shouldBe`
         "<p>&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText (p_ "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") `shouldBe`
         ("<p>fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText (p_ (p_ "Hello!")) `shouldBe`
         "<p><p>Hello!</p></p>")
     it "empty"
        (renderText (p_ (p_ "")) `shouldBe`
         "<p><p></p></p>")
     it "mixed"
        (renderText (p_ (style_ "")) `shouldBe`
         "<p><style></style></p>")
     it "no closing"
        (renderText (p_ (input_ [])) `shouldBe`
         "<p><input></p>")

-- | Test that attribute assigning works properly.
testAttributes :: Spec
testAttributes =
  do it "simple"
        (renderText (p_ [class_ "foo"] "foo") `shouldBe`
         "<p class=\"foo\">foo</p>")
     it "simple raw"
        (renderText (p_ [style_ "background-image: url('foo')"] "foo") `shouldBe`
         "<p style=\"background-image: url('foo')\">foo</p>")
     it "simple raw (href)"
        (renderText (p_ [onclick_ "window.location.href='asdf';"] "foo") `shouldBe`
         "<p onclick=\"window.location.href='asdf';\">foo</p>")
     it "duplicates (class)"
        (renderText (p_ [class_ "foo", class_ "bar"] "foo") `shouldBe`
         "<p class=\"foo bar\">foo</p>")
     it "duplicates (id)"
        (renderText (p_ [id_ "foo", id_ "bar"] "foo") `shouldBe`
         "<p id=\"foobar\">foo</p>")
     it "duplicates (style)"
        (renderText (p_ [style_ "foo", style_ "bar"] "foo") `shouldBe`
         "<p style=\"foo;bar\">foo</p>")
     it "escaping"
        (renderText (p_ [class_ "foo"] "'<>") `shouldBe`
         "<p class=\"foo\">&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText
           (p_ [class_ "foo"]
               "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") `shouldBe`
         ("<p class=\"foo\">fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText
           (p_ [class_ "foo"]
               (p_ "Hello!")) `shouldBe`
         "<p class=\"foo\"><p>Hello!</p></p>")
     it "empty"
        (renderText
           (p_ [class_ "foo"]
               (p_ "")) `shouldBe`
         "<p class=\"foo\"><p></p></p>")
     it "mixed" $
        renderText
           (p_ [class_ "foo",style_ "attrib"]
               (do style_ ""
                   style_ "")) ==?*
        [ "<p style=\"attrib\" class=\"foo\"><style></style><style></style></p>"
        , "<p class=\"foo\" style=\"attrib\"><style></style><style></style></p>"
        ]
     it "no closing"
        (renderText (p_ [class_ "foo"] (input_ [])) `shouldBe`
         "<p class=\"foo\"><input></p>")
     it "multiple" $
        renderText (p_ [class_ "foo",id_ "zot"] "foo") ==?*
        [ "<p id=\"zot\" class=\"foo\">foo</p>"
        , "<p class=\"foo\" id=\"zot\">foo</p>"
        ]
     it "encoded"
        (renderText (p_ [class_ "foo<>"] "foo") `shouldBe`
         "<p class=\"foo&lt;&gt;\">foo</p>")

-- | Test special elements that do something different to normal
-- elements.
testSpecials :: Spec
testSpecials =
  do it "script"
        (renderText (script_ "alert('Hello, World!')") `shouldBe`
         "<script>alert('Hello, World!')</script>")
     it "style"
        (renderText (style_ "body{background:url('Hello, World!')}") `shouldBe`
         "<style>body{background:url('Hello, World!')}</style>")

-- | Elements which do not contain children.
testSelfClosing :: Spec
testSelfClosing =
  do it "br" (renderText (br_ []) `shouldBe` "<br>")
     it "hr" (renderText (hr_ []) `shouldBe` "<hr>")
     it "input"
        (renderText (input_ []) `shouldBe`
         "<input>")
     it "input"
        (renderText (input_ [type_ "text"]) `shouldBe`
         "<input type=\"text\">")

testCommute :: Spec
testCommute = do
  it "commutes" $ do
    let stateAction :: HtmlT (Control.Monad.State.Strict.State [Integer]) ()
        stateAction = div_ [class_ "inside"] $ pure ()
        (fragment, _s) = runState (commuteHtmlT stateAction) [0]
    renderText (div_ [class_ "outside"] fragment) `shouldBe`
     "<div class=\"outside\"><div class=\"inside\"></div></div>"
