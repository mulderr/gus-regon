{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDaneOgolne where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)
import Web.BIR.BIR11.Types (Regon9, Nip)


data Bir11OsFizycznaDaneOgolne = Bir11OsFizycznaDaneOgolne
  { fiz_regon9 :: Maybe Regon9
  , fiz_nip :: Maybe Nip
  , fiz_statusNip :: Maybe Text
  , fiz_nazwisko :: Maybe Text
  , fiz_imie1 :: Maybe Text
  , fiz_imie2 :: Maybe Text
  , fiz_dataWpisuPodmiotuDoRegon :: Maybe Day
  , fiz_dataZaistnieniaZmiany :: Maybe Day
  , fiz_dataSkresleniaPodmiotuZRegon :: Maybe Day
  , fiz_podstawowaFormaPrawna_Symbol :: Maybe Text
  , fiz_szczegolnaFormaPrawna_Symbol :: Maybe Text
  , fiz_formaFinansowania_Symbol :: Maybe Text
  , fiz_formaWlasnosci_Symbol :: Maybe Text
  , fiz_podstawowaFormaPrawna_Nazwa :: Maybe Text
  , fiz_szczegolnaFormaPrawna_Nazwa :: Maybe Text
  , fiz_formaFinansowania_Nazwa :: Maybe Text
  , fiz_formaWlasnosci_Nazwa :: Maybe Text
  , fiz_dzialalnoscCeidg :: Maybe Int
  , fiz_dzialalnoscRolnicza :: Maybe Int
  , fiz_dzialalnoscPozostala :: Maybe Int
  , fiz_dzialalnoscSkreslonaDo20141108 :: Maybe Int
  , fiz_liczbaJednLokalnych :: Maybe Int
  } deriving (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, ToJSON)
