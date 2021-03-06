{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.Ec2.Commands.CreateSecurityGroup where

import Data.Aeson (Value (..), FromJSON, (.:), parseJSON)
import Data.Aeson.Types (typeMismatch)

import Aws.Ec2.TH

data CreateSecurityGroup = CreateSecurityGroup
               { csec_name :: Text
               , csec_description :: Text
               , csec_vpcId :: Maybe Text
               } deriving (Show)

instance SignQuery CreateSecurityGroup where
    type ServiceConfiguration CreateSecurityGroup = EC2Configuration
    signQuery CreateSecurityGroup{..} = ec2SignQuery $
                                           [ ("GroupName", qArg csec_name)
                                           , ("GroupDescription", qArg csec_description)
                                           , ("Action", qArg "CreateSecurityGroup")
                                           , defVersion
                                           ] +++ optionalA "VpcId" csec_vpcId

ec2ValueTransaction ''CreateSecurityGroup "CreateSecurityGroupResponse"

data CreateSecurityGroupResponse =
  CreateSecurityGroupResponse
  { csgrRequestId :: Text
  , csgrReturn :: Text
  , csgrGroupId :: Text }
  deriving Show

instance FromJSON CreateSecurityGroupResponse where
  parseJSON (Object v) = CreateSecurityGroupResponse <$>
    v .: "requestId" <*>
    v .: "return" <*>
    v .: "groupId"
  parseJSON invalid = typeMismatch "CreateSecurityGroupResponse" invalid
