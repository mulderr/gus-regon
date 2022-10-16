module Web.BIR.BIR11.Types.Search where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Web.BIR.BIR11.Types.Common (Regon, Nip, EntityType, EntitySilo)
import Web.BIR.BIR11.Xml (Bir11FromXml)


-- Example:
-- <dane>
--   <Regon>000331501</Regon>
--   <Nip>5261040828</Nip>
--   <StatusNip />
--   <Nazwa>GŁÓWNY URZĄD STATYSTYCZNY</Nazwa>
--   <Wojewodztwo>MAZOWIECKIE</Wojewodztwo>
--   <Powiat>m. st. Warszawa</Powiat>
--   <Gmina>Śródmieście</Gmina>
--   <Miejscowosc>Warszawa</Miejscowosc>
--   <KodPocztowy>00-925</KodPocztowy>
--   <Ulica>ul. Test-Krucza</Ulica>
--   <NrNieruchomosci>208</NrNieruchomosci>
--   <NrLokalu />
--   <Typ>P</Typ>
--   <SilosID>6</SilosID>
--   <DataZakonczeniaDzialalnosci />
--   <MiejscowoscPoczty />
-- </dane>
data SearchResult = SearchResult
  { regon :: Maybe Regon
  , nip :: Maybe Nip
  , statusNip :: Maybe Text
  , nazwa :: Maybe Text
  , wojewodztwo :: Maybe Text
  , powiat :: Maybe Text
  , gmina :: Maybe Text
  , miejscowosc :: Maybe Text
  , kodPocztowy :: Maybe Text
  , ulica :: Maybe Text
  , nrNieruchomosci :: Maybe Text
  , nrLokalu :: Maybe Text
  , typ :: Maybe EntityType
  , silosId :: Maybe EntitySilo
  , dataZakonczeniaDzialalnosci :: Maybe Text
  , miejscowoscPoczty :: Maybe Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Bir11FromXml, FromJSON, ToJSON)
