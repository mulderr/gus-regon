{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11TypPodmiotu where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Types (EntityType)
import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)


newtype Bir11TypPodmiotu = Bir11TypPodmiotu { typ :: Maybe EntityType }
  deriving (Eq, Show, Generic)
  deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, ToJSON)
