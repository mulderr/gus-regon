module Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDzialalnoscCeidg where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)
import Web.BIR.BIR11.Types (Regon9)


data Bir11OsFizycznaDzialalnoscCeidg = Bir11OsFizycznaDzialalnoscCeidg
  { fiz_regon9 :: Maybe Regon9
  , fiz_nazwa :: Maybe Text
  , fiz_nazwaSkrocona :: Maybe Text
  , fiz_dataPowstania :: Maybe Day
  , fiz_dataRozpoczeciaDzialalnosci :: Maybe Day
  , fiz_dataWpisuDzialalnosciDoRegon :: Maybe Day
  , fiz_dataZawieszeniaDzialalnosci :: Maybe Day
  , fiz_dataWznowieniaDzialalnosci :: Maybe Day
  , fiz_dataZaistnieniaZmianyDzialalnosci :: Maybe Day
  , fiz_dataZakonczeniaDzialalnosci :: Maybe Day
  , fiz_dataSkresleniaDzialalnosciZRegon :: Maybe Day
  , fiz_dataOrzeczeniaOUpadlosci :: Maybe Day
  , fiz_dataZakonczeniaPostepowaniaUpadlosciowego :: Maybe Day
  , fiz_adSiedzKraj_Symbol :: Maybe Text
  , fiz_adSiedzWojewodztwo_Symbol :: Maybe Text
  , fiz_adSiedzPowiat_Symbol :: Maybe Text
  , fiz_adSiedzGmina_Symbol :: Maybe Text
  , fiz_adSiedzKodPocztowy :: Maybe Text
  , fiz_adSiedzMiejscowoscPoczty_Symbol :: Maybe Text
  , fiz_adSiedzMiejscowosc_Symbol :: Maybe Text
  , fiz_adSiedzUlica_Symbol :: Maybe Text
  , fiz_adSiedzNumerNieruchomosci :: Maybe Text
  , fiz_adSiedzNumerLokalu :: Maybe Text
  , fiz_adSiedzNietypoweMiejsceLokalizacji :: Maybe Text
  , fiz_numerTelefonu :: Maybe Text
  , fiz_numerWewnetrznyTelefonu :: Maybe Text
  , fiz_numerFaksu :: Maybe Text
  , fiz_adresEmail :: Maybe Text
  , fiz_adresStronyinternetowej :: Maybe Text
  , fiz_adSiedzKraj_Nazwa :: Maybe Text
  , fiz_adSiedzWojewodztwo_Nazwa :: Maybe Text
  , fiz_adSiedzPowiat_Nazwa :: Maybe Text
  , fiz_adSiedzGmina_Nazwa :: Maybe Text
  , fiz_adSiedzMiejscowosc_Nazwa :: Maybe Text
  , fiz_adSiedzMiejscowoscPoczty_Nazwa :: Maybe Text
  , fiz_adSiedzUlica_Nazwa :: Maybe Text
  , fizC_dataWpisuDoRejestruEwidencji :: Maybe Day
  , fizC_dataSkresleniaZRejestruEwidencji :: Maybe Day
  , fizC_numerWRejestrzeEwidencji :: Maybe Text
  , fizC_OrganRejestrowy_Symbol :: Maybe Text
  , fizC_OrganRejestrowy_Nazwa :: Maybe Text
  , fizC_RodzajRejestru_Symbol :: Maybe Text
  , fizC_RodzajRejestru_Nazwa :: Maybe Text
  , fizC_NiePodjetoDzialalnosci :: Maybe Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, ToJSON)
