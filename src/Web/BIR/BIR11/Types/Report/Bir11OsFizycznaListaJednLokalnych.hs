{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11OsFizycznaListaJednLokalnych where

import Prelude

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)
import Web.BIR.BIR11.Types (Regon14, EntitySilo)


data Bir11OsFizycznaListaJednLokalnych = Bir11OsFizycznaListaJednLokalnych
  { lokfiz_regon14 :: Maybe Regon14
  , lokfiz_nazwa :: Maybe Text
  , lokfiz_silosID :: Maybe EntitySilo
  , lokfiz_silos_Symbol :: Maybe Text
  , lokfiz_adSiedzKraj_Symbol :: Maybe Text
  , lokfiz_adSiedzWojewodztwo_Symbol :: Maybe Text
  , lokfiz_adSiedzPowiat_Symbol :: Maybe Text
  , lokfiz_adSiedzGmina_Symbol :: Maybe Text
  , lokfiz_adSiedzKodPocztowy :: Maybe Text
  , lokfiz_adSiedzMiejscowoscPoczty_Symbol :: Maybe Text
  , lokfiz_adSiedzMiejscowosc_Symbol :: Maybe Text
  , lokfiz_adSiedzUlica_Symbol :: Maybe Text
  , lokfiz_adSiedzNumerNieruchomosci :: Maybe Text
  , lokfiz_adSiedzNumerLokalu :: Maybe Text
  , lokfiz_adSiedzNietypoweMiejsceLokalizacji :: Maybe Text
  , lokfiz_adSiedzWojewodztwo_Nazwa :: Maybe Text
  , lokfiz_adSiedzPowiat_Nazwa :: Maybe Text
  , lokfiz_adSiedzGmina_Nazwa :: Maybe Text
  , lokfiz_adSiedzMiejscowosc_Nazwa :: Maybe Text
  , lokfiz_adSiedzMiejscowoscPoczty_Nazwa :: Maybe Text
  , lokfiz_adSiedzUlica_Nazwa :: Maybe Text
  , lokfiz_dataPowstania :: Maybe Day
  , lokfiz_dataRozpoczeciaDzialalnosci :: Maybe Day
  , lokfiz_dataWpisuDoRegon :: Maybe Day
  , lokfiz_dataZawieszeniaDzialalnosci :: Maybe Day
  , lokfiz_dataWznowieniaDzialalnosci :: Maybe Day
  , lokfiz_dataZakonczeniaDzialalnosci :: Maybe Day
  , lokfiz_dataSkresleniaZRegon :: Maybe Day
  } deriving (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
