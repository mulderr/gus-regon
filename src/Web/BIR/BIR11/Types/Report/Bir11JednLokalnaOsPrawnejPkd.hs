{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsPrawnejPkd where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)


data Bir11JednLokalnaOsPrawnejPkd = Bir11JednLokalnaOsPrawnejPkd
  { lokpraw_pkdKod :: Maybe Text
  , lokpraw_pkdNazwa :: Maybe Text
  , lokpraw_pkdPrzewazajace :: Maybe Text
  } deriving (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
