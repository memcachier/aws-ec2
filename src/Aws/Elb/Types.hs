{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RecordWildCards
           , LambdaCase
           , OverloadedStrings
           #-}

module Aws.Elb.Types where

import Data.Text (Text)

import Network.HTTP.Types as HTTP

import Aws.Query

enumerateInstanceIds :: [Text] -> HTTP.Query
enumerateInstanceIds = enumerateLists "Instances.member." . fmap unroll
  where
    unroll i = [("InstanceId", qArg i)]

