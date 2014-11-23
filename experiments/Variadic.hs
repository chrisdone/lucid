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

-- -- import Control.Applicative
-- -- import Control.Monad

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.String
import GHC.Exts


data Attrs = Attrs [(String,String)]
  deriving Eq

data H m a where
  -- Attributes
  AttrList :: [H m ()] -> H m Attrs

  -- Waiting constructors
  WantsAttrsOnly :: String -> ([H m ()] -> [H m ()]) -> H m (Attrs -> H m ())
  WantsAttrsChildren :: String -> ([H m ()] -> [H m ()]) -> H m (Attrs -> (H m () -> H m ()))
  WantsChildren :: String -> [H m ()] -> H m (H m () -> H m ())

  -- Completed elements
  HasAttrs :: String -> [H m ()] -> H m ()
  HasAttrsChildren :: String -> [H m ()] -> H m () -> H m ()
  --
  Elements :: [H m ()] -> H m ()
  Text :: String -> H m ()
  Return :: a -> H m a

renderH :: H m () -> String
renderH (Elements es) = mconcat (map renderH es)
renderH (Text t) = encode t
  where encode = id
renderH (Return a) = mempty
renderH (HasAttrs name attrs) =
  "<" <> name <>
  concatMap showAttr attrs <>
  ">"
  where showAttr (HasAttrsChildren t _ c) = " " <> t <> "=\"" <> renderH c <>
                                            "\""
        showAttr c = error ""
renderH (HasAttrsChildren name attrs cs) =
  "<" <> name <>
  concatMap showAttr attrs <>
  ">" <>
  renderH cs <>
  "</" <>
  name <>
  ">"
  where showAttr (HasAttrsChildren t _ c) = " " <> t <> "=\"" <> renderH c <>
                                            "\""
        showAttr c = error ""

-- deriving instance Show a => Show (H m a)
-- deriving instance Eq a => Eq (H m a)

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
        WantsAttrsOnly name cont -> HasAttrs name (cont as)
        WantsAttrsChildren name cont -> HasAttrsChildren name (cont as)
        --
        WantsChildren{} -> error "AttrList->WantsChildren: impossible case"
        Return{} -> error "AttrList->Return: impossible case"
    Text{} -> case fun of
                WantsChildren name as -> HasAttrsChildren name as arg
                WantsAttrsChildren name cont -> HasAttrsChildren name (cont []) arg
                _ -> error "Text: imposible case"
    HasAttrs{} -> case fun of
                    WantsChildren name as -> HasAttrsChildren name as arg
                    WantsAttrsChildren name cont -> HasAttrsChildren name (cont []) arg
                    _ -> error "HasAttrs: impossible case"
    Elements{} -> case fun of
                    WantsChildren name as -> HasAttrsChildren name as arg
                    WantsAttrsChildren name cont -> HasAttrsChildren name (cont []) arg
                    _ -> error "Elements: impossible case"
    HasAttrsChildren{} -> case fun of
                            WantsChildren name as -> HasAttrsChildren name as arg
                            WantsAttrsChildren name cont -> HasAttrsChildren name (cont []) arg
                            _ -> error "HasAttrsChildren: impossible case"
    --
    WantsAttrsOnly{} -> error "WantsAttrsOnly: impossible case"
    WantsAttrsChildren{} -> error "WantsAttrsChildren: impossible case"
    WantsChildren{} -> error "WantChildren: impossible case"
    Return{} -> error "Return: impossible case"

div_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
div_ = makeElement (WantsAttrsChildren "div" id)

span_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
span_ = makeElement (WantsAttrsChildren "span" id)

td_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
td_ = makeElement (WantsAttrsChildren "td" id)

table_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
table_ = makeElement (WantsAttrsChildren "table" id)

tr_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
tr_ = makeElement (WantsAttrsChildren "tr" id)

strong_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
strong_ = makeElement (WantsAttrsChildren "strong" id)

divdemo' :: H m ()
divdemo' = div_ (Text "Hello, World!")

divdemo'' :: H m ()
divdemo'' = div_ (div_ (Text "Hello, world!"))

input_ :: H m b -> MakeElement Attrs b (H m ())
input_ = makeElement (WantsAttrsOnly "input" id)

br_ :: H m b -> MakeElement Attrs b (H m ())
br_ = makeElement (WantsAttrsOnly "br" id)

container_ =
  makeElement
    (WantsAttrsChildren
       "div"
       (composingAttr class_
                      "container"
                      (\x y -> x <> " " <> y)))

composingAttr name value f =
  composingAttrs
    (\key x y ->
       if key == fst (toKeyValue (name value))
          then f x y
          else x)
    [name value]

composingAttrs :: (String -> String -> String -> String)
               -> [H m ()]
               -> [H m ()]
               -> [H m ()]
composingAttrs compose xs ys =
  map pack
      (M.toList (M.unionWithKey compose
                                (M.fromList (map toKeyValue xs))
                                (M.fromList (map toKeyValue ys))))
  where

toKeyValue :: H m () -> (String,String)
toKeyValue (HasAttrsChildren name _ (Text value)) =
  (name,value)
pack (name,value) =
  HasAttrsChildren name
                   []
                   (Text value)

instance (a ~ ()) => Show (H m a) where
  show = renderH

-- overloaded versions

instance (a ~ ()) => IsString (H m a) where
  fromString = Text

instance (a ~ Attrs) => IsList (H m a) where
  type Item (H m a) = H m ()
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

-- instance Monad (H m) where
  -- return = Return
  -- m >>= f =
  --   case m of
  --     Return a -> f a
  --     HasAttrs{} ->
  --       case f () of Elements{} -> m <> f ()
  --                    Return{} -> f ()
  --                    HasAttrsChildren{} -> m <> f ()
  --                    HasAttrs{} -> m <> f ()
  --                    Text{} -> m <> f ()
  --                    WantsAttrsChildren{} -> error ""
  --                    WantsAttrsOnly{} -> error ""
  --                    WantsChildren{} -> error ""
  --     HasAttrsChildren{} -> f ()
  --     Elements{} -> f ()
  --     Text{} -> f ()
  --     AttrList xs -> f (Attrs xs)
  --     WantsChildren name as ->
  --       f (\c -> HasAttrsChildren name as c)
  --     WantsAttrsOnly name ->
  --       f (\_ -> HasAttrs name [])
  --     WantsAttrsChildren name ->
  --       f (\_ cs -> HasAttrsChildren name [] cs)

-- instance Functor (H m) where
--   fmap = liftM

-- instance Applicative (H m) where
--   (<*>) = ap
--   pure = return

-- id_ :: String -> (String, String)
-- id_ = ("id",)

-- type_ :: String -> (String, String)
-- type_ = ("type",)

-- class_ :: String -> (String, String)
-- class_ = ("class",)

style_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
style_ = makeElement (WantsAttrsChildren "style" id)

class_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
class_ = makeElement (WantsAttrsChildren "class" id)

id_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
id_ = makeElement (WantsAttrsChildren "id" id)

type_ :: H m b -> MakeElement Attrs b (H m () -> H m ())
type_ = makeElement (WantsAttrsChildren "type" id)

divdemo_ :: H m ()
divdemo_ =
  div_ [class_ "demo"] "Hello, World!"

divdemo'_ :: H m ()
divdemo'_ = div_ "Hello, World!"

divdemo''_ :: H m ()
divdemo''_ =
  div_ (Elements [div_ "Hello, world!"
                 ,"Sup?"
                 ,br_ []
                 ,style_ "body{background:red}"
                 ,table_ (Elements [tr_ (Elements [td_ "Some datum"
                                                  ,td_ "Another datum"])
                                   ,tr_ [class_ "alt",style_ "color:red"]
                                        (Elements [td_ "Some datum"
                                                  ,td_ "Another datum"])])
                 ,div_ (span_ [class_ "awesome"]
                              (strong_ "OK, go!"))])

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
test =[divdemo_,divdemo'_,divdemo''_,divdemo'''_,inputdemo_,combined_]

-- check :: Bool
-- check = test == [HasAttrsChildren "div" [("class","demo")] (Text "Hello, World!"),HasAttrsChildren "div" [] (Text "Hello, World!"),HasAttrsChildren "div" [] (HasAttrsChildren "div" [] (Text "Hello, world!")),HasAttrsChildren "div" [("id","foo")] (HasAttrsChildren "div" [] (Elements [HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!"),HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!")])),HasAttrs "input" [("type","text")],HasAttrsChildren "div" [] (HasAttrs "input" [("type","text")]),HasAttrsChildren "div" [("class","demo")] (Text "Hello, World!"),HasAttrsChildren "div" [] (Text "Hello, World!"),HasAttrsChildren "div" [] (HasAttrsChildren "div" [] (Text "Hello, world!")),HasAttrsChildren "div" [("id","foo")] (HasAttrsChildren "div" [] (Elements [HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!"),HasAttrsChildren "div" [("class","demo")] (Text "Hello, world!")])),HasAttrs "input" [("type","text")],HasAttrsChildren "div" [] (HasAttrs "input" [("type","text")])]
