{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Variadic where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.String
import GHC.Exts

data Attrs = Attrs [(String,String)]
  deriving Eq

data H m a where
  -- Attributes
  AttrList :: [(String,String)] -> H m Attrs

  -- Waiting constructors
  WantsAttrsOnly :: String -> H m (Attrs -> H m ())
  WantsAttrsChildren :: String -> H m (Attrs -> (H m () -> H m ()))
  WantsChildren :: String -> [(String,String)] -> H m (H m () -> H m ())

  -- Completed elements
  HasAttrs :: String -> [(String,String)] -> H m ()
  HasAttrsChildren :: String -> [(String,String)] -> H m () -> H m ()
  --
  Elements :: [H m ()] -> H m ()
  Text :: String -> H m ()
  Return :: a -> H m a

deriving instance Show a => Show (H m a)
deriving instance Eq a => Eq (H m a)

type family MakeElement a b r where
  MakeElement Attrs Attrs (H m ()) = H m ()
  MakeElement Attrs Attrs (H m () -> H m ()) = H m () -> H m ()
  MakeElement (H m ()) () (H m ()) = H m ()
  MakeElement Attrs () (H m () -> H m ()) = H m ()

makeElement :: H m (a -> r) -> H m b -> MakeElement a b r
makeElement fun arg =
  case arg of
    AttrList as ->
      case fun of
        WantsAttrsOnly name -> HasAttrs name as
        WantsAttrsChildren name -> HasAttrsChildren name as
        --
        WantsChildren{} -> error "AttrList->WantsChildren: impossible case"
        Return{} -> error "AttrList->Return: impossible case"
    Text{} -> case fun of
                WantsChildren name as -> HasAttrsChildren name as arg
                WantsAttrsChildren name -> HasAttrsChildren name [] arg
                _ -> error "Text: imposible case"
    HasAttrs{} -> case fun of
                    WantsChildren name as -> HasAttrsChildren name as arg
                    WantsAttrsChildren name -> HasAttrsChildren name [] arg
                    _ -> error "HasAttrs: impossible case"
    Elements{} -> case fun of
                    WantsChildren name as -> HasAttrsChildren name as arg
                    WantsAttrsChildren name -> HasAttrsChildren name [] arg
                    _ -> error "Elements: impossible case"
    HasAttrsChildren{} -> case fun of
                            WantsChildren name as -> HasAttrsChildren name as arg
                            WantsAttrsChildren name -> HasAttrsChildren name [] arg
                            _ -> error "HasAttrsChildren: impossible case"
    --
    WantsAttrsOnly{} -> error "WantsAttrsOnly: impossible case"
    WantsAttrsChildren{} -> error "WantsAttrsChildren: impossible case"
    WantsChildren{} -> error "WantChildren: impossible case"
    Return{} -> error "Return: impossible case"

div_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
div_ = makeElement (WantsAttrsChildren "div")

span_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
span_ = makeElement (WantsAttrsChildren "span")

td_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
td_ = makeElement (WantsAttrsChildren "td")

table_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
table_ = makeElement (WantsAttrsChildren "table")

tr_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
tr_ = makeElement (WantsAttrsChildren "tr")

strong_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
strong_ = makeElement (WantsAttrsChildren "strong")

divdemo :: H m ()
divdemo = div_ (AttrList [("class","demo")]) (Text "Hello, World!")

divdemo' :: H m ()
divdemo' = div_ (Text "Hello, World!")

divdemo'' :: H m ()
divdemo'' = div_ (div_ (Text "Hello, world!"))

divdemo''' :: H m ()
divdemo''' = div_ (AttrList [("id","foo")])
                  (div_ (Elements [div_ (AttrList [("class","demo")])
                                        (Text "Hello, world!")
                                  ,div_ (AttrList [("class","demo")])
                                        (Text "Hello, world!")]))

input_ :: H m b -> MakeElement Attrs b (H m ())
input_ = makeElement (WantsAttrsOnly "input")

br_ :: H m b -> MakeElement Attrs b (H m ())
br_ = makeElement (WantsAttrsOnly "br")

inputdemo :: H m ()
inputdemo = input_ (AttrList [("type","text")])

combined :: H m ()
combined = div_ (input_ (AttrList [("type","text")]))

-- overloaded versions

instance (a ~ ()) => IsString (H m a) where
  fromString = Text

instance (a ~ Attrs) => IsList (H m a) where
  type Item (H m a) = (String,String)
  fromList = AttrList
  toList (AttrList l) = l
  toList _ = error "H.toList: Invalid constructor"

instance (Monoid a) => Monoid (H m a) where
  mempty = Return mempty
  mappend a b =
    case a of
      Elements as ->
        case b of
          Elements bs -> Elements (as <> bs)
          Text{} -> Elements (as <> [b])
          Return{} -> a
          HasAttrs{} -> Elements (as <> [b])
          HasAttrsChildren{} -> Elements (as <> [b])
          _ -> error "inaccessible code, but GHC can't detect it"
      Text as ->
        case b of
          Elements bs -> Elements ([a] <> bs)
          Text bs -> Text (as <> bs)
          Return{} -> a
          HasAttrs{} -> Elements [a,b]
          HasAttrsChildren{} -> Elements [a,b]
          _ -> error "inaccessible code, but GHC can't detect it"
      Return{} -> b
      AttrList as ->
                case b of
                  AttrList bs -> AttrList (as ++ bs)
                  _ -> error "inaccessible code, but GHC cannot detect it"
      HasAttrs{} -> hasAttrs
      HasAttrsChildren{} -> hasAttrs
      --
      WantsAttrsOnly {} -> error ""
      WantsAttrsChildren {} -> error ""
      WantsChildren {} -> error ""

    where hasAttrs =
            case b of
              Elements bs -> Elements ([a] <> bs)
              Text{} -> Elements ([a,b])
              Return{} -> a
              HasAttrs{} -> Elements ([a,b])
              HasAttrsChildren{} -> Elements ([a,b])
              _ -> error "inaccessible code"

instance Monad (H m) where
  return = Return
  m >>= f =
    case m of
      Return a -> f a
      HasAttrs{} ->
        case f () of Elements{} -> m <> f ()
                     Return{} -> f ()
                     HasAttrsChildren{} -> m <> f ()
                     HasAttrs{} -> m <> f ()
                     Text{} -> m <> f ()
                     WantsAttrsChildren{} -> error ""
                     WantsAttrsOnly{} -> error ""
                     WantsChildren{} -> error ""
      HasAttrsChildren{} -> f ()
      Elements{} -> f ()
      Text{} -> f ()
      AttrList xs -> f (Attrs xs)
      WantsChildren name as ->
        f (\c -> HasAttrsChildren name as c)
      WantsAttrsOnly name ->
        f (\_ -> HasAttrs name [])
      WantsAttrsChildren name ->
        f (\_ cs -> HasAttrsChildren name [] cs)

instance Functor (H m) where
  fmap = liftM

instance Applicative (H m) where
  (<*>) = ap
  pure = return

id_ :: String -> (String, String)
id_ = ("id",)

type_ :: String -> (String, String)
type_ = ("type",)

class_ :: String -> (String, String)
class_ = ("class",)

style_ :: String -> (String, String)
style_ = ("style",)

style__ :: H m b -> MakeElement Attrs b (H m () -> H m ())
style__ = makeElement (WantsAttrsChildren "style")

-- class Mixed a r | r -> a where
--   mixed :: String -> a -> r

-- instance Mixed String (String,String) where
--   mixed key val = (key,val)

-- instance Mixed (H m b) (MakeElement Attrs b (H m () -> H m ())) where

divdemo_ :: H m ()
divdemo_ =
  div_ [class_ "demo"] "Hello, World!"

divdemo'_ :: H m ()
divdemo'_ = div_ "Hello, World!"

divdemo''_ :: H m ()
divdemo''_ =
  div_ (do div_ "Hello, world!"
           "Sup?"
           br_ []
           style__ "body{background:red}"
           table_ (do tr_ (do td_ "Some datum"
                              td_ "Another datum")
                      tr_ [class_ "alt",style_ "color:red"]
                          (do td_ "Some datum"
                              td_ "Another datum"))
           div_ (span_ [class_ "awesome"]
                       (strong_ "OK, go!")))

divdemo'''_ :: H m ()
divdemo'''_ =
  div_ [id_ "foo"]
       (div_ (div_ [class_ "demo"] "Hello, world!" <>
              div_ [class_ "potato"] "Hello, world!"))

inputdemo_ :: H m ()
inputdemo_ = input_ [type_ "text"]

combined_ :: H m ()
combined_ = div_ (input_ [type_ "text"])

test :: [H m ()]
test =
  [divdemo,divdemo',divdemo'',divdemo''',inputdemo,combined] ++
  [divdemo_,divdemo'_,divdemo''_,divdemo'''_,inputdemo_,combined_]

check :: Bool
check = test == [HasAttrsChildren "div" [("class","demo")] (Text "Hello, World!"),HasAttrsChildren "div" [] (Text "Hello, World!"),HasAttrsChildren "div" [] (HasAttrsChildren "div" [] (Text "Hello, world!")),HasAttrsChildren "div" [("id","foo")] (HasAttrsChildren "div" [] (Elements [HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!"),HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!")])),HasAttrs "input" [("type","text")],HasAttrsChildren "div" [] (HasAttrs "input" [("type","text")]),HasAttrsChildren "div" [("class","demo")] (Text "Hello, World!"),HasAttrsChildren "div" [] (Text "Hello, World!"),HasAttrsChildren "div" [] (HasAttrsChildren "div" [] (Text "Hello, world!")),HasAttrsChildren "div" [("id","foo")] (HasAttrsChildren "div" [] (Elements [HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!"),HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!")])),HasAttrs "input" [("type","text")],HasAttrsChildren "div" [] (HasAttrs "input" [("type","text")])]
