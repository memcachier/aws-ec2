{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Ec2.Commands.DescribeReservedInstances where

import Data.Aeson (Value (..), FromJSON, parseJSON)

import Aws.Ec2.TH

data DescribeReservedInstances = DescribeReservedInstances { dri_instanceIds :: [Text] } deriving (Show)

instance SignQuery DescribeReservedInstances where
    type ServiceConfiguration DescribeReservedInstances = EC2Configuration
    signQuery DescribeReservedInstances{..} = ec2SignQuery $
                                                [ ("Action", qArg "DescribeReservedInstances")
                                                , defVersion
                                                ] +++ enumerate "ReservedInstancesId" dri_instanceIds qArg

ec2ValueTransaction ''DescribeReservedInstances "reservedInstancesSet"

newtype DescribeReservedInstancesResponse =
  DescribeReservedInstancesResponse {dirReservedInstances :: [ReservedInstance]} deriving (Show)

instance FromJSON DescribeReservedInstancesResponse where
  parseJSON v = DescribeReservedInstancesResponse <$>
    (maybeToList <$> parseJSON v)

