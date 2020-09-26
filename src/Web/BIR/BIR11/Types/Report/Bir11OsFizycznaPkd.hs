{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11OsFizycznaPkd where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)
import Web.BIR.BIR11.Types (EntitySilo)


data Bir11OsFizycznaPkd = Bir11OsFizycznaPkd
  { fiz_pkd_Kod :: Maybe Text
  , fiz_pkd_Nazwa :: Maybe Text
  , fiz_pkd_Przewazajace :: Maybe Text
  , fiz_SilosID :: Maybe EntitySilo
  , fiz_Silos_Symbol :: Maybe Text
  , fiz_dataSkresleniaDzialalnosciZRegon :: Maybe Day
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
