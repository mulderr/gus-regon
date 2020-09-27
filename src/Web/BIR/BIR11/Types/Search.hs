{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language TemplateHaskell #-}

module Web.BIR.BIR11.Types.Search where

import Data.Text (Text)
import GHC.Generics (Generic)

import Web.BIR.BIR11.TH ( deriveAll )
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
  { _regon :: Maybe Regon
  , _nip :: Maybe Nip
  , _statusNip :: Maybe Text
  , _nazwa :: Maybe Text
  , _wojewodztwo :: Maybe Text
  , _powiat :: Maybe Text
  , _gmina :: Maybe Text
  , _miejscowosc :: Maybe Text
  , _kodPocztowy :: Maybe Text
  , _ulica :: Maybe Text
  , _nrNieruchomosci :: Maybe Text
  , _nrLokalu :: Maybe Text
  , _typ :: Maybe EntityType
  , _silosId :: Maybe EntitySilo
  , _dataZakonczeniaDzialalnosci :: Maybe Text
  , _miejscowoscPoczty :: Maybe Text
  } deriving (Eq, Show, Generic, Bir11FromXml)


$(deriveAll ''SearchResult)
