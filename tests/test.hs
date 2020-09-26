{-# language OverloadedStrings #-}

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


url :: String
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
  assertSearchEquals (SearchParamsRegon regon_) [expected]
  where
    regon_ = Regon "000331501"
    expected = SearchResult
      { _regon = Just regon_
      , _nip = Just $ Nip "5261040828"
      , _statusNip = Nothing
      , _nazwa = Just "GŁÓWNY URZĄD STATYSTYCZNY"
      , _wojewodztwo = Just "MAZOWIECKIE"
      , _powiat = Just "m. st. Warszawa"
      , _gmina = Just "Śródmieście"
      , _miejscowosc = Just "Warszawa"
      , _kodPocztowy = Just "00-925"
      , _ulica = Just "ul. Test-Krucza"
      , _nrNieruchomosci = Just "208"
      , _nrLokalu = Nothing
      , _typ = Just EntityTypeP
      , _silosId = Just EntitySilo6
      , _dataZakonczeniaDzialalnosci = Nothing
      , _miejscowoscPoczty = Just "Warszawa"
      }

testSearchNip :: IO ()
testSearchNip = do
  assertSearchEquals (SearchParamsNip nip_) [expected]
  where
    nip_ = Nip "5220002529"
    expected = SearchResult
      { _regon = Just $ Regon "000288975"
      , _nip = Just nip_
      , _statusNip = Nothing
      , _nazwa = Just "SAMODZIELNY PUBLICZNY CENTRALNY SZPITAL KLINICZNY W WARSZAWIE"
      , _wojewodztwo = Just "MAZOWIECKIE"
      , _powiat = Just "m. st. Warszawa"
      , _gmina = Just "Ochota"
      , _miejscowosc = Just "Warszawa"
      , _kodPocztowy = Just "02-097"
      , _ulica = Just "ul. Test-Krucza"
      , _nrNieruchomosci = Just "1A"
      , _nrLokalu = Nothing
      , _typ = Just EntityTypeP
      , _silosId = Just EntitySilo6
      , _dataZakonczeniaDzialalnosci = Nothing
      , _miejscowoscPoczty = Just "Warszawa"
      }

testSearchKrs :: IO ()
testSearchKrs = do
  assertSearchEquals (SearchParamsKrs krs_) [expected]
  where
    krs_ = Krs "0000028860"
    expected = SearchResult
      { _regon = Just $ Regon "610188201"
      , _nip = Just $ Nip "7740001454"
      , _statusNip = Nothing
      , _nazwa = Just "POLSKI KONCERN NAFTOWY ORLEN SPÓŁKA AKCYJNA"
      , _wojewodztwo = Just "MAZOWIECKIE"
      , _powiat = Just "m. Płock"
      , _gmina = Just "M. Płock"
      , _miejscowosc = Just "Płock"
      , _kodPocztowy = Just "09-411"
      , _ulica = Just "ul. Test-Wilcza"
      , _nrNieruchomosci = Just "7"
      , _nrLokalu = Nothing
      , _typ = Just EntityTypeP
      , _silosId = Just EntitySilo6
      , _dataZakonczeniaDzialalnosci = Nothing
      , _miejscowoscPoczty = Just "Płock"
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
  praw_regon9 x @?= Just (Regon9 "610188201")
  praw_nip x @?= Just (Nip "7740001454")
  praw_statusNip x @?= Nothing
  praw_nazwa x @?= Just "POLSKI KONCERN NAFTOWY ORLEN SPÓŁKA AKCYJNA"
  praw_nazwaSkrocona x @?= Just "PKN ORLEN S.A."
  praw_numerWRejestrzeEwidencji x @?= Just "0000028860"
  praw_dataWpisuDoRejestruEwidencji x @?= Just (isoDay "2008-07-25")
  praw_dataPowstania x @?= Just (isoDay "1993-07-01")
  praw_dataRozpoczeciaDzialalnosci x @?= Just (isoDay "1993-07-01")
  praw_dataWpisuDoRegon x @?= Nothing
  praw_dataZawieszeniaDzialalnosci x @?= Nothing
  praw_dataWznowieniaDzialalnosci x @?= Nothing
  praw_dataZaistnieniaZmiany x @?= Just (isoDay "2013-08-28")
  praw_dataZakonczeniaDzialalnosci x @?= Nothing
  praw_dataSkresleniaZRegon x @?= Nothing
  praw_dataOrzeczeniaOUpadlosci x @?= Nothing
  praw_dataZakonczeniaPostepowaniaUpadlosciowego x @?= Nothing
  praw_adSiedzKraj_Symbol x @?= Just "PL"
  praw_adSiedzWojewodztwo_Symbol x @?= Just "14"
  praw_adSiedzPowiat_Symbol x @?= Just "62"
  praw_adSiedzGmina_Symbol x @?= Just "011"
  praw_adSiedzKodPocztowy x @?= Just "09411"
  praw_adSiedzMiejscowoscPoczty_Symbol x @?= Just "0968687"
  praw_adSiedzMiejscowosc_Symbol x @?= Just "0968687"
  praw_adSiedzUlica_Symbol x @?= Just "24261"
  praw_adSiedzNumerNieruchomosci x @?= Just "7"
  praw_adSiedzNumerLokalu x @?= Nothing
  praw_adSiedzNietypoweMiejsceLokalizacji x @?= Nothing
  praw_numerTelefonu x @?= Just "243655405"
  praw_numerWewnetrznyTelefonu x @?= Nothing
  praw_numerFaksu x @?= Just "243653157"
  praw_adresEmail x @?= Nothing
  praw_adresStronyinternetowej x @?= Nothing
  praw_adSiedzKraj_Nazwa x @?= Just "POLSKA"
  praw_adSiedzWojewodztwo_Nazwa x @?= Just "MAZOWIECKIE"
  praw_adSiedzPowiat_Nazwa x @?= Just "m. Płock"
  praw_adSiedzGmina_Nazwa x @?= Just "M. Płock"
  praw_adSiedzMiejscowosc_Nazwa x @?= Just "Płock"
  praw_adSiedzMiejscowoscPoczty_Nazwa x @?= Just "Płock"
  praw_adSiedzUlica_Nazwa x @?= Just "ul. Test-Wilcza"
  praw_podstawowaFormaPrawna_Symbol x @?= Just "1"
  praw_szczegolnaFormaPrawna_Symbol x @?= Just "16"
  praw_formaFinansowania_Symbol x @?= Just "1"
  praw_formaWlasnosci_Symbol x @?= Just "234"
  praw_organZalozycielski_Symbol x @?= Just "017000000"
  praw_organRejestrowy_Symbol x @?= Just "071010060"
  praw_rodzajRejestruEwidencji_Symbol x @?= Just "138"
  praw_podstawowaFormaPrawna_Nazwa x @?= Just "OSOBA PRAWNA"
  praw_szczegolnaFormaPrawna_Nazwa x @?= Just "SPÓŁKI AKCYJNE"
  praw_formaFinansowania_Nazwa x @?= Just "JEDNOSTKA SAMOFINANSUJĄCA NIE BĘDĄCA JEDNOSTKĄ BUDŻETOWĄ LUB SAMORZĄDOWYM ZAKŁADEM BUDŻETOWYM"
  praw_formaWlasnosci_Nazwa x @?= Just "WŁASNOŚĆ MIESZANA MIĘDZY SEKTORAMI Z PRZEWAGĄ WŁASNOŚCI SEKTORA PRYWATNEGO, W TYM Z PRZEWAGĄ WŁASNOŚCI KRAJOWYCH OSÓB FIZYCZNYCH"
  praw_organZalozycielski_Nazwa x @?= Just "MINISTER SKARBU PAŃSTWA"
  praw_organRejestrowy_Nazwa x @?= Just "SĄD REJONOWY DLA M.ST.WARSZAWY W WARSZAWIE,XIV WYDZIAŁ GOSPODARCZY KRAJOWEGO REJESTRU SĄDOWEGO"
  praw_rodzajRejestruEwidencji_Nazwa x @?= Just "REJESTR PRZEDSIĘBIORCÓW"
  praw_liczbaJednLokalnych x @?= Just 0

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
