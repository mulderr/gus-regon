{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11OsPrawnaSpCywilnaWspolnicy where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)
import Web.BIR.BIR11.Types (Regon9)


data Bir11OsPrawnaSpCywilnaWspolnicy = Bir11OsPrawnaSpCywilnaWspolnicy
  { wspolsc_regonWspolnikSpolki :: Maybe Regon9
  , wspolsc_imiePierwsze :: Maybe Text
  , wspolsc_imieDrugie :: Maybe Text
  , wspolsc_nazwisko :: Maybe Text
  , wspolsc_firmaNazwa :: Maybe Text
  } deriving (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
