module Main where
import Control.Lens
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Web.BIR.BIR11.Methods
import Web.BIR.BIR11.Types
import Web.BIR.BIR11.Types.Report
import Web.BIR.BIR11.Types.Report.Bir11OsPrawna
import Web.BIR.BIR11.Xml (Bir11FromXmlDoc)


url :: Text
url = "https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc"

key :: ApiKey
key = ApiKey "abcde12345abcde12345"


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Network tests"
  -- search
  [ testCase "search regon" testSearchRegon
  , testCase "search nip" testSearchNip
  , testCase "search krs" testSearchKrs
  , testCase "search wrong" testSearchWrong
  -- full report
  , testCase "fullReport OsPrawna" testReportOsPrawna
  -- get value
  , testCase "getValue StanDanych" testGetValueStanDanych
  , testCase "getValue StatusSesji" testGetValueStatusSesji
  , testCase "getValue StatusUslugi" testGetValueStatusUslugi
  ]

testSearch :: SearchParams -> IO (Either Bir11Error [SearchResult])
testSearch ps = do
  Right skey <- login url key
  res <- search url skey ps
  Right True <- logout url skey
  pure res

assertSearchEquals :: SearchParams -> [SearchResult] -> IO ()
assertSearchEquals ps expected = do
  xs <- testSearch ps
  xs @?= Right expected

testSearchRegon :: IO ()
testSearchRegon = do
  assertSearchEquals (SearchParamsRegon regon) [expected]
  where
    regon = Regon "000331501"
    expected = SearchResult
      { regon = Just regon
      , nip = Just $ Nip "5261040828"
      , statusNip = Nothing
      , nazwa = Just "GŁÓWNY URZĄD STATYSTYCZNY"
      , wojewodztwo = Just "MAZOWIECKIE"
      , powiat = Just "m. st. Warszawa"
      , gmina = Just "Śródmieście"
      , miejscowosc = Just "Warszawa"
      , kodPocztowy = Just "00-925"
      , ulica = Just "ul. Test-Krucza"
      , nrNieruchomosci = Just "208"
      , nrLokalu = Nothing
      , typ = Just EntityTypeP
      , silosId = Just EntitySilo6
      , dataZakonczeniaDzialalnosci = Nothing
      , miejscowoscPoczty = Just "Warszawa"
      }

testSearchNip :: IO ()
testSearchNip = do
  assertSearchEquals (SearchParamsNip nip) [expected]
  where
    nip = Nip "5220002529"
    expected = SearchResult
      { regon = Just $ Regon "000288975"
      , nip = Just nip
      , statusNip = Nothing
      , nazwa = Just "SAMODZIELNY PUBLICZNY CENTRALNY SZPITAL KLINICZNY W WARSZAWIE"
      , wojewodztwo = Just "MAZOWIECKIE"
      , powiat = Just "m. st. Warszawa"
      , gmina = Just "Ochota"
      , miejscowosc = Just "Warszawa"
      , kodPocztowy = Just "02-097"
      , ulica = Just "ul. Test-Krucza"
      , nrNieruchomosci = Just "1A"
      , nrLokalu = Nothing
      , typ = Just EntityTypeP
      , silosId = Just EntitySilo6
      , dataZakonczeniaDzialalnosci = Nothing
      , miejscowoscPoczty = Just "Warszawa"
      }

testSearchKrs :: IO ()
testSearchKrs = do
  assertSearchEquals (SearchParamsKrs krs) [expected]
  where
    krs = Krs "0000028860"
    expected = SearchResult
      { regon = Just $ Regon "610188201"
      , nip = Just $ Nip "7740001454"
      , statusNip = Nothing
      , nazwa = Just "POLSKI KONCERN NAFTOWY ORLEN SPÓŁKA AKCYJNA"
      , wojewodztwo = Just "MAZOWIECKIE"
      , powiat = Just "m. Płock"
      , gmina = Just "M. Płock"
      , miejscowosc = Just "Płock"
      , kodPocztowy = Just "09-411"
      , ulica = Just "ul. Test-Wilcza"
      , nrNieruchomosci = Just "7"
      , nrLokalu = Nothing
      , typ = Just EntityTypeP
      , silosId = Just EntitySilo6
      , dataZakonczeniaDzialalnosci = Nothing
      , miejscowoscPoczty = Just "Płock"
      }

testSearchWrong :: IO ()
testSearchWrong = do
  Left err <- testSearch (SearchParamsNip $ Nip "foobar")
  err @?= Bir11ApiError expected
  where
    expected = Bir11ApiErrorMsg
      { errorCode = "4"
      , errorMessagePl = "Nie znaleziono podmiotu dla podanych kryteriów wyszukiwania."
      , errorMessageEn = "No data found for the specified search criteria."
      }

testFullReport :: Bir11FromXmlDoc a => Bir11FullReport a -> IO (Either Bir11Error a)
testFullReport report = do
  Right skey <- login url key
  res <- fullReport url skey report
  Right True <- logout url skey
  pure res

testReportOsPrawna :: IO ()
testReportOsPrawna = do
  Right x <- testFullReport (Bir11FrOsPrawna $ Regon9 "610188201")
  x.praw_regon9 @?= Just (Regon9 "610188201")
  x.praw_nip @?= Just (Nip "7740001454")
  x.praw_statusNip @?= Nothing
  x.praw_nazwa @?= Just "POLSKI KONCERN NAFTOWY ORLEN SPÓŁKA AKCYJNA"
  x.praw_nazwaSkrocona @?= Just "PKN ORLEN S.A."
  x.praw_numerWRejestrzeEwidencji @?= Just "0000028860"
  x.praw_dataWpisuDoRejestruEwidencji @?= Just (isoDay "2008-07-25")
  x.praw_dataPowstania @?= Just (isoDay "1993-07-01")
  x.praw_dataRozpoczeciaDzialalnosci @?= Just (isoDay "1993-07-01")
  x.praw_dataWpisuDoRegon @?= Nothing
  x.praw_dataZawieszeniaDzialalnosci @?= Nothing
  x.praw_dataWznowieniaDzialalnosci @?= Nothing
  x.praw_dataZaistnieniaZmiany @?= Just (isoDay "2013-08-28")
  x.praw_dataZakonczeniaDzialalnosci @?= Nothing
  x.praw_dataSkresleniaZRegon @?= Nothing
  x.praw_dataOrzeczeniaOUpadlosci @?= Nothing
  x.praw_dataZakonczeniaPostepowaniaUpadlosciowego @?= Nothing
  x.praw_adSiedzKraj_Symbol @?= Just "PL"
  x.praw_adSiedzWojewodztwo_Symbol @?= Just "14"
  x.praw_adSiedzPowiat_Symbol @?= Just "62"
  x.praw_adSiedzGmina_Symbol @?= Just "011"
  x.praw_adSiedzKodPocztowy @?= Just "09411"
  x.praw_adSiedzMiejscowoscPoczty_Symbol @?= Just "0968687"
  x.praw_adSiedzMiejscowosc_Symbol @?= Just "0968687"
  x.praw_adSiedzUlica_Symbol @?= Just "24261"
  x.praw_adSiedzNumerNieruchomosci @?= Just "7"
  x.praw_adSiedzNumerLokalu @?= Nothing
  x.praw_adSiedzNietypoweMiejsceLokalizacji @?= Nothing
  x.praw_numerTelefonu @?= Just "243655405"
  x.praw_numerWewnetrznyTelefonu @?= Nothing
  x.praw_numerFaksu @?= Just "243653157"
  x.praw_adresEmail @?= Nothing
  x.praw_adresStronyinternetowej @?= Nothing
  x.praw_adSiedzKraj_Nazwa @?= Just "POLSKA"
  x.praw_adSiedzWojewodztwo_Nazwa @?= Just "MAZOWIECKIE"
  x.praw_adSiedzPowiat_Nazwa @?= Just "m. Płock"
  x.praw_adSiedzGmina_Nazwa @?= Just "M. Płock"
  x.praw_adSiedzMiejscowosc_Nazwa @?= Just "Płock"
  x.praw_adSiedzMiejscowoscPoczty_Nazwa @?= Just "Płock"
  x.praw_adSiedzUlica_Nazwa @?= Just "ul. Test-Wilcza"
  x.praw_podstawowaFormaPrawna_Symbol @?= Just "1"
  x.praw_szczegolnaFormaPrawna_Symbol @?= Just "16"
  x.praw_formaFinansowania_Symbol @?= Just "1"
  x.praw_formaWlasnosci_Symbol @?= Just "234"
  x.praw_organZalozycielski_Symbol @?= Just "017000000"
  x.praw_organRejestrowy_Symbol @?= Just "071010060"
  x.praw_rodzajRejestruEwidencji_Symbol @?= Just "138"
  x.praw_podstawowaFormaPrawna_Nazwa @?= Just "OSOBA PRAWNA"
  x.praw_szczegolnaFormaPrawna_Nazwa @?= Just "SPÓŁKI AKCYJNE"
  x.praw_formaFinansowania_Nazwa @?= Just "JEDNOSTKA SAMOFINANSUJĄCA NIE BĘDĄCA JEDNOSTKĄ BUDŻETOWĄ LUB SAMORZĄDOWYM ZAKŁADEM BUDŻETOWYM"
  x.praw_formaWlasnosci_Nazwa @?= Just "WŁASNOŚĆ MIESZANA MIĘDZY SEKTORAMI Z PRZEWAGĄ WŁASNOŚCI SEKTORA PRYWATNEGO, W TYM Z PRZEWAGĄ WŁASNOŚCI KRAJOWYCH OSÓB FIZYCZNYCH"
  x.praw_organZalozycielski_Nazwa @?= Just "MINISTER SKARBU PAŃSTWA"
  x.praw_organRejestrowy_Nazwa @?= Just "SĄD REJONOWY DLA M.ST.WARSZAWY W WARSZAWIE,XIV WYDZIAŁ GOSPODARCZY KRAJOWEGO REJESTRU SĄDOWEGO"
  x.praw_rodzajRejestruEwidencji_Nazwa @?= Just "REJESTR PRZEDSIĘBIORCÓW"
  x.praw_liczbaJednLokalnych @?= Just 0

isoDay :: String -> Day
isoDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"

testGetValue :: Bir11ParamName -> IO (Either Bir11Error Text)
testGetValue param = do
  Right skey <- login url key
  res <- getValue url skey param
  Right True <- logout url skey
  pure res

testGetValueStanDanych :: IO ()
testGetValueStanDanych = do
  Right s <- testGetValue StanDanych
  s @?= "19-10-2018"

testGetValueStatusSesji :: IO ()
testGetValueStatusSesji = do
  Right s <- testGetValue StatusSesji
  s @?= "1"

testGetValueStatusUslugi :: IO ()
testGetValueStatusUslugi = do
  Right s <- testGetValue StatusUslugi
  s @?= "1"
