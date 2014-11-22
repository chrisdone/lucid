{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Examples that should always compile.

module Lucid.Example where

import Lucid

foo :: Html ()
foo = p_ ""

demo :: Html ()
demo = with p_ [class_ "foo<>"] (p_ "foo { background: url('fi'); }")

mixed :: Html ()
mixed = with p_ [style_ "foo<>"] (style_ "foo { background: url('fi'); ")
