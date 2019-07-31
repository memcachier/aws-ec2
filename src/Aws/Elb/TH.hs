{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           #-}

module Aws.Elb.TH (
  module Aws.Core
, module Aws.Elb.Core
, module Aws.Query
, Text
, FromJSON
, elbValueTransaction
, elbValueTransactionDef
) where

import Language.Haskell.TH.Lib    (DecsQ, conT, stringE)
import Language.Haskell.TH.Syntax (Name)

import Data.Text                  (Text)
import Data.Aeson.Types           (FromJSON(..))

import Aws.Core
import Aws.Query
import Aws.Query.TH
import Aws.Elb.Core

elbValueTransaction :: Name -> String -> DecsQ
elbValueTransaction ty tag = [d|
                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = QueryMetadata
                      responseConsumer _ _ = queryResponseConsumer $ valueConsumerOpt (XMLValueOptions "member") $(stringE tag) fromJSONConsumer

                  instance Transaction $(conT ty) Value
                  |]

elbValueTransactionDef :: Name -> Name -> String -> String -> DecsQ
elbValueTransactionDef ty cons tag filterKey = queryValueTransactionDef ty cons tag 'elbSignQuery 'defVersion "member" filterKey
