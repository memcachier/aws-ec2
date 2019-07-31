{-# LANGUAGE DeriveGeneric
           , LambdaCase
           , RecordWildCards
           , NamedFieldPuns
           , OverloadedStrings
           , MultiWayIf
           , TypeFamilies
           #-}

module Aws.Query.Types (
  Value(..)
, XMLValueOptions(..)
, toValue
, castValue
) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Vector as V
import qualified Data.List as L

import Text.XML (Element(..), Name(..), Node(..))

import Aws.Core (AsMemoryResponse, MemoryResponse(..))

data XMLValueOptions = XMLValueOptions
                     { arrayElementTag :: Text
                     }

instance AsMemoryResponse Value where
    type MemoryResponse Value = Value
    loadToMemory = return

-- import Debug.Trace (trace)
-- import Text.Show.Pretty (ppShow)
--
-- traceArg a = ppShow a `trace` a
traceArg :: a -> a
traceArg = id

castValue :: FromJSON a => Value -> Maybe a
castValue v = parseMaybe (const (parseJSON v)) v

toValue :: XMLValueOptions -> Node -> Value
toValue = value

value :: XMLValueOptions -> Node -> Value
value options (NodeElement e) = values options (elementNodes e)
value _       (NodeContent c) = String c
value _       _ = Null

values :: XMLValueOptions -> [Node] -> Value
values options elementNodes = uncurry (elementValues options) $ traceArg $ (elementKind options elementNodes, elementNodes)

data ElementKind = ObjectLike
                 | ArrayLike
                 | Other
                 deriving (Show)

elementKind :: XMLValueOptions -> [Node] -> ElementKind
elementKind XMLValueOptions{..} nodes
  | isXMLArray = ArrayLike
  | isObject = ObjectLike
  | otherwise = Other
  where
    filtered = filterNodes nodes
    elems = onlyElements nodes

    isObject = (not $ null elems) && length filtered == length elems
    isXMLArray = [arrayElementTag] == (L.nub $ fmap forceElementName elems)

elementValues :: XMLValueOptions -> ElementKind -> [Node] -> Value
elementValues options ObjectLike ns = object [(forceElementName n, values options $ filterNodes $ elementNodes $ unElement n) | n <- onlyElements ns]
elementValues options ArrayLike ns = array $ innerNodes ns
  where
    innerNodes :: [Node] -> [Value]
    innerNodes nodes = fmap (values options . filterNodes . elementNodes . unElement) $ onlyElements $ filterNodes $ nodes
elementValues options Other ns = arrayOrValue $ fmap (value options) ns
  where
    arrayOrValue (x:[]) = x
    arrayOrValue [] = Null
    arrayOrValue a = array a

forceElementName :: Node -> Text
forceElementName = nameLocalName . elementName . unElement

unElement :: Node -> Element
unElement (NodeElement e) = e
unElement (NodeInstruction _) = error "unElement required NodeElement but received NodeInstruction"
unElement (NodeContent _) = error "unElement required NodeElement but received NodeContent"
unElement (NodeComment _) = error "unElement required NodeElement but received NodeComment"

array :: [Value] -> Value
array = Array . V.fromList

onlyElements :: [Node] -> [Node]
onlyElements = filter $ \case
                        NodeElement _ -> True
                        _ -> False

filterNodes :: [Node] -> [Node]
filterNodes = filter $ \case
                          NodeContent s -> (T.strip s) /= T.empty
                          _ -> True
