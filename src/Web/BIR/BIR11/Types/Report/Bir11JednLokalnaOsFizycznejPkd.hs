module Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsFizycznejPkd where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.Xml (Bir11FromXml)


data Bir11JednLokalnaOsFizycznejPkd = Bir11JednLokalnaOsFizycznejPkd
  { lokfiz_pkd_Kod :: Maybe Text
  , lokfiz_pkd_Nazwa :: Maybe Text
  , lokfiz_pkd_Przewazajace :: Maybe Text
  , lokfiz_Silos_Symbol :: Maybe Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, ToJSON)
