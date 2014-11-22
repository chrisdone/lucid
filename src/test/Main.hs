{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Test suite for ACE.

module Main where

import Lucid

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

testText :: Spec
testText =
  do it "simple"
        (renderText "foo" ==
         "foo")
     it "escaping"
        (renderText "'<>" ==
         "&#39;&lt;&gt;")
     it "unicode"
        (renderText "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381" ==
         "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381")

testElements :: Spec
testElements =
  do it "simple"
        (renderText (p_ "foo") ==
         "<p>foo</p>")
     it "escaping"
        (renderText (p_ "'<>") ==
         "<p>&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText (p_ "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") ==
         ("<p>fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText (p_ (p_ "Hello!")) ==
         "<p><p>Hello!</p></p>")
     it "empty"
        (renderText (p_ (p_ "")) ==
         "<p><p></p></p>")
     it "mixed"
        (renderText (p_ (style_ "")) ==
         "<p><style></style></p>")
     it "no closing"
        (renderText (p_ input_) ==
         "<p><input></p>")

testAttributes :: Spec
testAttributes =
  do it "simple"
        (renderText (with p_ [class_ "foo"] "foo") ==
         "<p class=\"foo\">foo</p>")
     it "escaping"
        (renderText (with p_ [class_ "foo"] "'<>") ==
         "<p class=\"foo\">&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText
           (with p_
                 [class_ "foo"]
                 "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") ==
         ("<p class=\"foo\">fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText
           (with p_
                 [class_ "foo"]
                 (p_ "Hello!")) ==
         "<p class=\"foo\"><p>Hello!</p></p>")
     it "empty"
        (renderText
           (with p_
                 [class_ "foo"]
                 (p_ "")) ==
         "<p class=\"foo\"><p></p></p>")
     it "mixed"
        (renderText
           (with p_
                 [class_ "foo"]
                 (style_ "")) ==
         "<p class=\"foo\"><style></style></p>")
     it "no closing"
        (renderText (with p_ [class_ "foo"] input_) ==
         "<p class=\"foo\"><input></p>")
     it "multiple"
        (renderText (with p_ [class_ "foo",id_ "zot"] "foo") ==
         "<p id=\"zot\" class=\"foo\">foo</p>")
     it "encoded"
        (renderText (with p_ [class_ "foo<>"] "foo") ==
         "<p class=\"foo&lt;&gt;\">foo</p>")
     it "nesting attributes"
        (renderText (with (with p_ [class_ "foo"]) [class_ "bar"] "foo") ==
         "<p class=\"foobar\">foo</p>")
