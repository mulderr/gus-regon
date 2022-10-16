module Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsPrawnej where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)
import Web.BIR.BIR11.Types (Regon14)


data Bir11JednLokalnaOsPrawnej = Bir11JednLokalnaOsPrawnej
  { lokpraw_regon14 :: Maybe Regon14
  , lokpraw_nazwa :: Maybe Text
  , lokpraw_numerWrejestrzeEwidencji :: Maybe Text
  , lokpraw_dataWpisuDoRejestruEwidencji :: Maybe Day
  , lokpraw_dataPowstania :: Maybe Day
  , lokpraw_dataRozpoczeciaDzialalnosci :: Maybe Day
  , lokpraw_dataWpisuDoRegon :: Maybe Day
  , lokpraw_dataZawieszeniaDzialalnosci :: Maybe Day
  , lokpraw_dataWznowieniaDzialalnosci :: Maybe Day
  , lokpraw_dataZaistnieniaZmiany :: Maybe Day
  , lokpraw_dataZakonczeniaDzialalnosci :: Maybe Day
  , lokpraw_dataSkresleniaZRegon :: Maybe Day
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
  , lokpraw_adSiedzKraj_Nazwa :: Maybe Text
  , lokpraw_adSiedzWojewodztwo_Nazwa :: Maybe Text
  , lokpraw_adSiedzPowiat_Nazwa :: Maybe Text
  , lokpraw_adSiedzGmina_Nazwa :: Maybe Text
  , lokpraw_adSiedzMiejscowosc_Nazwa :: Maybe Text
  , lokpraw_adSiedzMiejscowoscPoczty_Nazwa :: Maybe Text
  , lokpraw_adSiedzUlica_Nazwa :: Maybe Text
  , lokpraw_formaFinansowania_Symbol :: Maybe Text
  , lokpraw_organRejestrowy_Symbol :: Maybe Text
  , lokpraw_rodzajRejestruEwidencji_Symbol :: Maybe Text
  , lokpraw_formaFinansowania_Nazwa :: Maybe Text
  , lokpraw_organRejestrowy_Nazwa :: Maybe Text
  , lokpraw_rodzajRejestruEwidencji_Nazwa :: Maybe Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, ToJSON)
