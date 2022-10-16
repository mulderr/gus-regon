module Web.BIR.BIR11.Types.Error where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml, Bir11FromXmlDoc)


-- <dane>
--   <ErrorCode>4</ErrorCode>
--   <ErrorMessagePl>Nie znaleziono podmiotu dla podanych kryteriów wyszukiwania.</ErrorMessagePl>
--   <ErrorMessageEn>No data found for the specified search criteria.</ErrorMessageEn>
--   ...
-- </dane>
data Bir11ApiErrorMsg = Bir11ApiErrorMsg
  { errorCode :: Text
  , errorMessagePl :: Text
  , errorMessageEn :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, Bir11FromXmlDoc, FromJSON, ToJSON)

data Bir11Error
  = Bir11ProtocolError Text
  | Bir11ApiError Bir11ApiErrorMsg
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Exception)

