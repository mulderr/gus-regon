{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11OsPrawnaPkd where

import Data.Aeson (ToJSON) 
import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)


data Bir11OsPrawnaPkd = Bir11OsPrawnaPkd
  { praw_pkdKod :: Maybe Text
  , praw_pkdNazwa :: Maybe Text
  , praw_pkdPrzewazajace :: Maybe Text
  } deriving (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
