module Web.BIR.BIR11.Soap where

import Data.Bifunctor (first)
import Data.Map qualified as Map
import Data.Text (Text)
import Text.XML
    ( Document(..),
      def,
      Element(Element),
      Name(Name),
      Prologue(Prologue) )
import Text.XML.Writer
    ( content, element, elementA, render, ToXML(..) )


import Web.BIR.BIR11.Types
import Web.BIR.BIR11.Types.Report ( Bir11FullReport, bir11FullReportToParams )


-- | Shorthand for qualified names
--
-- Note (#) would conflict with OverloadedLabels
(##) :: Text -> Text -> Name
(##) prefix localName = Name (prefix <> ":" <> localName) Nothing (Just prefix)

-- | Create a SOAP message
soapdoc :: (ToXML head, ToXML body)
  => [(Text, Text)] -- ^ extra namespaces to put in soap:Envelope
  -> head -- ^ soap:Header
  -> body -- ^ soap:Body
  -> Document
soapdoc extraNss h b =
  Document
    { documentPrologue = Prologue def def def
    , documentRoot = Element ("soap"##"Envelope") (Map.fromList nss) $ render $ do
        toXML h
        toXML b
    , documentEpilogue = def
    }
  where
    nss =
      ("xmlns"##"soap", "http://www.w3.org/2003/05/soap-envelope") : map (first ("xmlns"##)) extraNss

msgLogin :: Text -> ApiKey -> Document
msgLogin url apikey =
  soapdoc [("ns", "http://CIS/BIR/PUBL/2014/07")] h b
  where
    h = elementA ("soap"##"Header") [ ("xmlns"##"wsa", "http://www.w3.org/2005/08/addressing") ] $ do
      element ("wsa"##"Action") $ content "http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/Zaloguj"
      element ("wsa"##"To") $ content url
    
    b = element ("soap"##"Body") $ do
      element ("ns"##"Zaloguj") $ do
        element ("ns"##"pKluczUzytkownika") $ content apikey.unApiKey

-- <soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/PUBL/2014/07">
--   <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
--     <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To>
--     <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/Wyloguj</wsa:Action>
--   </soap:Header>
--   <soap:Body>
--     <ns:Wyloguj>
--       <ns:pIdentyfikatorSesji>xxxxxxxxxxxxxxxxxxxx</ns:pIdentyfikatorSesji>
--     </ns:Wyloguj>
--   </soap:Body>
-- </soap:Envelope>
msgLogout :: Text -> SessionKey -> Document
msgLogout url sessionkey =
  soapdoc [("ns", "http://CIS/BIR/PUBL/2014/07")] h b
  where
    h = elementA ("soap"##"Header") [ ("xmlns"##"wsa", "http://www.w3.org/2005/08/addressing") ] $ do
      element ("wsa"##"Action") $ content "http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/Wyloguj"
      element ("wsa"##"To") $ content url

    b = element ("soap"##"Body") $ do
      element ("ns"##"Wyloguj") $ do
        element ("ns"##"pIdentyfikatorSesji") $ content sessionkey.unSessionKey

-- | DaneSzukajPodmioty
--
-- <soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/PUBL/2014/07" xmlns:dat="http://CIS/BIR/PUBL/2014/07/DataContract">
--   <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
--     <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To>
--     <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DaneSzukajPodmioty</wsa:Action>
--   </soap:Header>
--   <soap:Body>
--     <ns:DaneSzukajPodmioty>
--       <ns:pParametryWyszukiwania>
--         <dat:Nip>5261040828</dat:Nip>
--       </ns:pParametryWyszukiwania>
--     </ns:DaneSzukajPodmioty>
--   </soap:Body>
msgSearch :: Text -> SearchParams -> Document
msgSearch url ps =
  soapdoc [ ("ns", "http://CIS/BIR/PUBL/2014/07")
          , ("dat", "http://CIS/BIR/PUBL/2014/07/DataContract")
          ] h b
  where
    h = elementA ("soap"##"Header") [ ("xmlns"##"wsa", "http://www.w3.org/2005/08/addressing") ] $ do
      element ("wsa"##"Action") $ content "http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DaneSzukajPodmioty"
      element ("wsa"##"To") $ content url

    b = element ("soap"##"Body") $ do
      element ("ns"##"DaneSzukajPodmioty") $ do
        element ("ns"##"pParametryWyszukiwania") $ do
          element ("dat"##nm) $ content val

    (nm, val) = searchParamsToParams ps

-- | GetValue
--
-- <soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/2014/07">
--   <soap:Headerxmlns:wsa="http://www.w3.org/2005/08/addressing">
--     <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To>
--     <wsa:Action>http://CIS/BIR/2014/07/IUslugaBIR/GetValue</wsa:Action>
--   </soap:Header>
--   <soap:Body>
--     <ns:GetValue>
--       <ns:pNazwaParametru>StatusSesji</ns:pNazwaParametru>
--     </ns:GetValue>
--   </soap:Body>
-- </soap:Envelope>
msgGetValue :: Text -> Bir11ParamName -> Document
msgGetValue url pname = do
  soapdoc [("ns", "http://CIS/BIR/2014/07")] h b
  where
    h = elementA ("soap"##"Header") [ ("xmlns"##"wsa", "http://www.w3.org/2005/08/addressing") ] $ do
      element ("wsa"##"Action") $ content "http://CIS/BIR/2014/07/IUslugaBIR/GetValue"
      element ("wsa"##"To") $ content url

    b = element ("soap"##"Body") $ do
      element ("ns"##"GetValue") $ do
        element ("ns"##"pNazwaParametru") $ content $ paramNameToText pname


-- <soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:ns="http://CIS/BIR/PUBL/2014/07">
--   <soap:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
--     <wsa:To>https://wyszukiwarkaregontest.stat.gov.pl/wsBIR/UslugaBIRzewnPubl.svc</wsa:To>
--     <wsa:Action>http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DanePobierzPelnyRaport</wsa:Action>
--   </soap:Header>
--   <soap:Body>
--     <ns:DanePobierzPelnyRaport>
--       <ns:pRegon>000331501</ns:pRegon>
--       <ns:pNazwaRaportu>BIR11OsPrawna</ns:pNazwaRaportu>
--     </ns:DanePobierzPelnyRaport>
--   </soap:Body>
-- </soap:Envelope>
msgFullReport :: Text -> Bir11FullReport a -> Document
msgFullReport url report =
  soapdoc [("ns", "http://CIS/BIR/PUBL/2014/07")] h b
  where
    h = elementA ("soap"##"Header") [ ("xmlns"##"wsa", "http://www.w3.org/2005/08/addressing") ] $ do
      element ("wsa"##"Action") $ content "http://CIS/BIR/PUBL/2014/07/IUslugaBIRzewnPubl/DanePobierzPelnyRaport"
      element ("wsa"##"To") $ content url

    b = element ("soap"##"Body") $ do
      element ("ns"##"DanePobierzPelnyRaport") $ do
        element ("ns"##"pRegon") $ content reg
        element ("ns"##"pNazwaRaportu") $ content reportName

    (reportName, reg) = bir11FullReportToParams report
