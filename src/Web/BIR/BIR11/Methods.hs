-- | BIR 1.1 methods
module Web.BIR.BIR11.Methods where

import Control.Lens ((^..), (&), (.~), (^.))
import Data.Bifunctor (first)
import Data.CaseInsensitive (CI (..))
import Data.Text (Text)
import Network.Wreq
    ( Response,
      postWith,
      defaults,
      header,
      responseBody,
      responseHeader )
import Text.XML ( parseText, renderLBS, def, Document )
import Text.XML.Lens ( (...), named, root, text )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import Web.BIR.BIR11.Parse ( simpleParse )
import Web.BIR.BIR11.Soap ( msgLogin, msgLogout, msgSearch, msgGetValue, msgFullReport )
import Web.BIR.BIR11.Types
    ( Bir11ParamName,
      SearchParams,
      SessionKey(..),
      ApiKey,
      SearchResult,
      Bir11Error(..),
      Bir11ApiErrorMsg )
import Web.BIR.BIR11.Types.Report ( Bir11FullReport )
import Web.BIR.BIR11.Xml ( Bir11FromXmlDoc(..) )


-- | POST SOAP Document with Content-Type application/soap+xml
postSoap :: Maybe SessionKey -> String -> Document -> IO (Response BL.ByteString)
postSoap mkey url = postWith opts url . renderLBS def
  where
    baseOpts = defaults & header "Content-Type" .~ ["application/soap+xml; charset=utf-8"]
                        & header "Accept" .~ ["multipart/related; type=\"application/xop+xml\""]
    opts = case mkey of
      Nothing -> baseOpts
      Just key -> baseOpts & header "sid" .~ [TE.encodeUtf8 $ unSessionKey key]

-- | Read SOAP response
--
-- Expects multipart/related; type="application/xop+xml"
readSoap :: Response BL.ByteString -> Either String Document
readSoap res = do
  simpleParse (res ^. responseHeader "Content-Type") (BL.toStrict $ res ^. responseBody)

-- | Read BIR11 SOAP response
bir11ReadSoap
  :: Response BL.ByteString -- ^ raw response
  -> CI Text -- ^ response node name
  -> CI Text -- ^ result node name
  -> Either Bir11Error Text
bir11ReadSoap res responseName resultName = do
  doc <- first Bir11ProtocolError $ readSoap res
  case doc ^.. root . named "envelope" ... named "body" ... named responseName ... named resultName . text of
    [x] -> Right x
    _ -> Left $ Bir11ProtocolError "error: could not understand soap response"

-- | Read BIR11 SOAP response, return inner xml document
bir11ReadData
  :: Response BL.ByteString -- ^ raw response
  -> CI Text -- ^ response node name
  -> CI Text -- ^ result node name
  -> Either Bir11Error Document
bir11ReadData res responseName resultName =
  bir11ReadSoap res responseName resultName >>= toDoc
  where
    toDoc = 
      first (Bir11ProtocolError . show) . parseText def . TL.fromStrict

-- | BIR11 Zaloguj
--
-- Session ends either after calling logout or after 60m timeout or otherwise at 03:20 when the session purge job is activated.
-- Session status can be verified using getValue at any time.
login
  :: String -- ^ API URL
  -> ApiKey -- ^ KluczUzytkownika
  -> IO (Either Bir11Error SessionKey) -- ^ If successful 20 char session key (IdentyfikatorSesji)
login url apikey = do
  res <- postSoap Nothing url $ msgLogin (T.pack url) apikey
  pure $ case bir11ReadSoap res "zalogujresponse" "zalogujresult" of
    Right x | T.length x == 20 -> Right $ SessionKey x
    Right x -> Left $ Bir11ProtocolError $ "error: expected a session key with length 20 but got something of length: " <> show (T.length x)
    Left err -> Left err

-- | BIR11 DaneSzukajPodmioty
--
-- Search for entities matching given criteria.
search
  :: String -- ^ API URL
  -> SessionKey -- ^ IdentyfikatorSesji
  -> SearchParams -- ^ ParametryWyszukiwania
  -> IO (Either Bir11Error [SearchResult])
search url sessionkey searchParams = do
  res <- postSoap (Just sessionkey) url $ msgSearch (T.pack url) searchParams
  pure $ case bir11ReadData res "daneszukajpodmiotyresponse" "daneszukajpodmiotyresult" of
    Right d -> do
      case bir11FromXmlDoc @Bir11ApiErrorMsg d of
        Right err -> Left $ Bir11ApiError err
        Left _ -> first Bir11ProtocolError $ bir11FromXmlDoc d
    Left err ->
      Left err

-- | BIR11 DanePobierzPelnyRaport
--
-- Fetch info for a single entity.
fullReport :: forall a. (Bir11FromXmlDoc a)
  => String -- ^ API URL
  -> SessionKey -- ^ IdentyfikatorSesji
  -> Bir11FullReport a -- ^ NazwaRportu + REGON
  -> IO (Either Bir11Error a)
fullReport url sessionkey report = do
  res <- postSoap (Just sessionkey) url $ msgFullReport (T.pack url) report
  pure $ case bir11ReadData res "danepobierzpelnyraportresponse" "danepobierzpelnyraportresult" of
    Right d -> do
      case bir11FromXmlDoc @Bir11ApiErrorMsg d of
        Right err -> Left $ Bir11ApiError err
        Left _ -> first Bir11ProtocolError $ bir11FromXmlDoc d
    Left err ->
      Left err

{-
-- | BIR11 DanePobierzRaportZbiorczy
aggregateReport
  :: String -- ^ API URL
  -> SessionKey -- ^ IdentyfikatorSesji
  -> Text -- ^ DataRaportu, must be in range [today - 8d, today)
  -> Bir11AggregateReport -- ^ NazwaRaportu
  -> IO (Either Text [Regon])
aggregateReport _reportDate _reportName = error "TODO"
-}

-- | BIR11 GetValue
--
-- Returns diagnostic information about the service and current session.
getValue
  :: String -- ^ API URL
  -> SessionKey -- ^ IdentyfikatorSesji
  -> Bir11ParamName -- ^ NazwaParametru
  -> IO (Either Bir11Error Text)
getValue url sessionkey pname = do
  res <- postSoap (Just sessionkey) url $ msgGetValue (T.pack url) pname
  pure $ bir11ReadSoap res "getvalueresponse" "getvalueresult"

-- | BIR11 Wyloguj
logout
  :: String -- ^ API URL
  -> SessionKey -- ^ IdentyfikatorSesji
  -> IO (Either Bir11Error Bool) -- ^ True if successfull
logout url sessionkey = do
  res <- postSoap (Just sessionkey) url $ msgLogout (T.pack url) sessionkey
  pure $ case bir11ReadSoap res "wylogujresponse" "wylogujresult" of
    Right "true" -> Right True
    Right "false" -> Right False
    Right x -> Left $ Bir11ProtocolError $ "error: expected a boolean result but got: " <> T.unpack x
    Left err -> Left err
