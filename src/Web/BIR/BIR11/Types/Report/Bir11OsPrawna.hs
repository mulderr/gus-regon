module Web.BIR.BIR11.Types.Report.Bir11OsPrawna where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)
import Web.BIR.BIR11.Types (Regon9, Nip)


data Bir11OsPrawna = Bir11OsPrawna
  { praw_regon9 :: Maybe Regon9
  , praw_nip :: Maybe Nip
  , praw_statusNip :: Maybe Text
  , praw_nazwa :: Maybe Text
  , praw_nazwaSkrocona :: Maybe Text
  , praw_numerWRejestrzeEwidencji :: Maybe Text
  , praw_dataWpisuDoRejestruEwidencji :: Maybe Day
  , praw_dataPowstania :: Maybe Day
  , praw_dataRozpoczeciaDzialalnosci :: Maybe Day
  , praw_dataWpisuDoRegon :: Maybe Day
  , praw_dataZawieszeniaDzialalnosci :: Maybe Day
  , praw_dataWznowieniaDzialalnosci :: Maybe Day
  , praw_dataZaistnieniaZmiany :: Maybe Day
  , praw_dataZakonczeniaDzialalnosci :: Maybe Day
  , praw_dataSkresleniaZRegon :: Maybe Day
  , praw_dataOrzeczeniaOUpadlosci :: Maybe Day
  , praw_dataZakonczeniaPostepowaniaUpadlosciowego :: Maybe Day
  , praw_adSiedzKraj_Symbol :: Maybe Text
  , praw_adSiedzWojewodztwo_Symbol :: Maybe Text
  , praw_adSiedzPowiat_Symbol :: Maybe Text
  , praw_adSiedzGmina_Symbol :: Maybe Text
  , praw_adSiedzKodPocztowy :: Maybe Text
  , praw_adSiedzMiejscowoscPoczty_Symbol :: Maybe Text
  , praw_adSiedzMiejscowosc_Symbol :: Maybe Text
  , praw_adSiedzUlica_Symbol :: Maybe Text
  , praw_adSiedzNumerNieruchomosci :: Maybe Text
  , praw_adSiedzNumerLokalu :: Maybe Text
  , praw_adSiedzNietypoweMiejsceLokalizacji :: Maybe Text
  , praw_numerTelefonu :: Maybe Text
  , praw_numerWewnetrznyTelefonu :: Maybe Text
  , praw_numerFaksu :: Maybe Text
  , praw_adresEmail :: Maybe Text
  , praw_adresStronyinternetowej :: Maybe Text
  , praw_adSiedzKraj_Nazwa :: Maybe Text
  , praw_adSiedzWojewodztwo_Nazwa :: Maybe Text
  , praw_adSiedzPowiat_Nazwa :: Maybe Text
  , praw_adSiedzGmina_Nazwa :: Maybe Text
  , praw_adSiedzMiejscowosc_Nazwa :: Maybe Text
  , praw_adSiedzMiejscowoscPoczty_Nazwa :: Maybe Text
  , praw_adSiedzUlica_Nazwa :: Maybe Text
  , praw_podstawowaFormaPrawna_Symbol :: Maybe Text
  , praw_szczegolnaFormaPrawna_Symbol :: Maybe Text
  , praw_formaFinansowania_Symbol :: Maybe Text
  , praw_formaWlasnosci_Symbol :: Maybe Text
  , praw_organZalozycielski_Symbol :: Maybe Text
  , praw_organRejestrowy_Symbol :: Maybe Text
  , praw_rodzajRejestruEwidencji_Symbol :: Maybe Text
  , praw_podstawowaFormaPrawna_Nazwa :: Maybe Text
  , praw_szczegolnaFormaPrawna_Nazwa :: Maybe Text
  , praw_formaFinansowania_Nazwa :: Maybe Text
  , praw_formaWlasnosci_Nazwa :: Maybe Text
  , praw_organZalozycielski_Nazwa :: Maybe Text
  , praw_organRejestrowy_Nazwa :: Maybe Text
  , praw_rodzajRejestruEwidencji_Nazwa :: Maybe Text
  , praw_liczbaJednLokalnych :: Maybe Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, ToJSON)
