module Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsFizycznej where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day)
import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)
import Web.BIR.BIR11.Types (EntitySilo, Regon14)


data Bir11JednLokalnaOsFizycznej = Bir11JednLokalnaOsFizycznej
  { lokfiz_regon14 :: Maybe Regon14
  , lokfiz_nazwa :: Maybe Text
  , lokfiz_silosID :: Maybe EntitySilo
  , lokfiz_silos_Nazwa :: Maybe Text
  , lokfiz_dataPowstania :: Maybe Day
  , lokfiz_dataRozpoczeciaDzialalnosci :: Maybe Day
  , lokfiz_dataWpisuDoRegon :: Maybe Day
  , lokfiz_dataZawieszeniaDzialalnosci :: Maybe Day
  , lokfiz_dataWznowieniaDzialalnosci :: Maybe Day
  , lokfiz_dataZaistnieniaZmiany :: Maybe Day
  , lokfiz_dataZakonczeniaDzialalnosci :: Maybe Day
  , lokfiz_dataSkresleniaZRegon :: Maybe Day
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
  , lokfiz_adSiedzKraj_Nazwa :: Maybe Text
  , lokfiz_adSiedzWojewodztwo_Nazwa :: Maybe Text
  , lokfiz_adSiedzPowiat_Nazwa :: Maybe Text
  , lokfiz_adSiedzGmina_Nazwa :: Maybe Text
  , lokfiz_adSiedzMiejscowosc_Nazwa :: Maybe Text
  , lokfiz_adSiedzMiejscowoscPoczty_Nazwa :: Maybe Text
  , lokfiz_adSiedzUlica_Nazwa :: Maybe Text
  , lokfiz_FormaFinansowania_Nazwa :: Maybe Text
  , lokfiz_FormaFinansowania_Symbol :: Maybe Text
  , lokfiz_dataWpisuDoRejestruEwidencji :: Maybe Day
  , lokfiz_numerwRejestrzeEwidencji :: Maybe Text
  , lokfiz_OrganRejestrowy_Symbol :: Maybe Text
  , lokfiz_RodzajRejestru_Symbol :: Maybe Text
  , lokfiz_OrganRejestrowy_Nazwa :: Maybe Text
  , lokfiz_RodzajRejestru_Nazwa :: Maybe Text
  , lokfizC_NiePodjetoDzialalnosci :: Maybe Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, ToJSON)
