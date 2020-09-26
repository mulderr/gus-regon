{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}

module Web.BIR.BIR11.Types.Report.Bir11OsPrawnaListaJednLokalnych where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)
import Web.BIR.BIR11.Types (Regon14)


data Bir11OsPrawnaListaJednLokalnych = Bir11OsPrawnaListaJednLokalnych
  { lokpraw_regon14 :: Maybe Regon14
  , lokpraw_nazwa :: Maybe Text
  , lokpraw_adSiedzKraj_Symbol :: Maybe Text
  , lokpraw_adSiedzWojewodztwo_Symbol :: Maybe Text
  , lokpraw_adSiedzPowiat_Symbol :: Maybe Text
  , lokpraw_adSiedzGmina_Symbol :: Maybe Text
  , lokpraw_adSiedzKodPocztowy :: Maybe Text
  , lokpraw_adSiedzMiejscowoscPoczty_Symbol :: Maybe Text
  , lokpraw_adSiedzMiejscowosc_Symbol :: Maybe Text
  , lokpraw_adSiedzUlica_Symbol :: Maybe Text
  , lokpraw_adSiedzNumerNieruchomosci :: Maybe Text
  , lokpraw_adSiedzNumerLokalu :: Maybe Text
  , lokpraw_adSiedzNietypoweMiejsceLokalizacji :: Maybe Text
  , lokpraw_adSiedzWojewodztwo_Nazwa :: Maybe Text
  , lokpraw_adSiedzPowiat_Nazwa :: Maybe Text
  , lokpraw_adSiedzGmina_Nazwa :: Maybe Text
  , lokpraw_adSiedzMiejscowosc_Nazwa :: Maybe Text
  , lokpraw_adSiedzMiejscowoscPoczty_Nazwa :: Maybe Text
  , lokpraw_adSiedzUlica_Nazwa :: Maybe Text
  , lokpraw_dataPowstania :: Maybe Day
  , lokpraw_dataRozpoczeciaDzialalnosci :: Maybe Day
  , lokpraw_dataWpisuDoRegon :: Maybe Day
  , lokpraw_dataZawieszeniaDzialalnosci :: Maybe Day
  , lokpraw_dataWznowieniaDzialalnosci :: Maybe Day
  , lokpraw_dataZakonczeniaDzialalnosci :: Maybe Day
  , lokpraw_dataSkresleniaZRegon :: Maybe Day
  } deriving (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
