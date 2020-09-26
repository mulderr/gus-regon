{-# language DeriveGeneric #-}
{-# language DerivingVia #-}

module Web.BIR.BIR11.Types.Common where

import Control.Applicative ( Alternative((<|>)) ) 
import GHC.Generics ( Generic )
import Data.Text (Text)
import Data.Aeson (withText, withScientific, FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (parseFail)

import qualified Data.Text as T

import Web.BIR.BIR11.Xml


newtype ApiKey = ApiKey { unApiKey :: Text } deriving (Eq, Show)
newtype SessionKey = SessionKey { unSessionKey :: Text } deriving (Eq, Show)

newtype Regon = Regon { unRegon :: Text } deriving (Eq, Show, Generic) deriving (FromJSON, ToJSON) via Text
newtype Regon9 = Regon9 { unRegon9 :: Text } deriving (Eq, Show, Generic) deriving (FromJSON, ToJSON) via Text
newtype Regon14 = Regon14 { unRegon14 :: Text } deriving (Eq, Show, Generic) deriving (FromJSON, ToJSON) via Text
newtype Nip = Nip { unNip :: Text } deriving (Eq, Show) deriving (FromJSON, ToJSON) via Text
newtype Krs = Krs { unKrs :: Text } deriving (Eq, Show, Generic) deriving (FromJSON, ToJSON) via Text

toRegon9 :: Regon -> Maybe Regon9
toRegon9 (Regon r) =
  if T.length r >= 9
  then Just $ Regon9 $ T.take 9 r
  else Nothing

toRegon14 :: Regon -> Maybe Regon14
toRegon14 (Regon r) =
  if T.length r == 14
  then Just $ Regon14 r
  else Nothing

fromRegon9 :: Regon9 -> Regon
fromRegon9 = Regon . unRegon9

fromRegon14 :: Regon14 -> Regon
fromRegon14 = Regon . unRegon14


-- | ParametryWyszukiwania
data SearchParams
  -- single identifier 
  = SearchParamsRegon Regon
  | SearchParamsNip Nip
  | SearchParamsKrs Krs
  -- multiple identifiers (up to 20)
  | SearchParamsRegon9Multi [Regon9]
  | SearchParamsRegon14Multi [Regon14]
  | SearchParamsNipMulti [Nip]
  | SearchParamsKrsMulti [Krs] 
  deriving (Eq, Show)

searchParamsToParams :: SearchParams -> (Text, Text)
searchParamsToParams = \case
  SearchParamsRegon v -> ("Regon", unRegon v)
  SearchParamsNip v -> ("Nip", unNip v)
  SearchParamsKrs v -> ("Krs", unKrs v)
  SearchParamsRegon9Multi vs -> ("Regony9zn", T.intercalate "," $ map unRegon9 vs)
  SearchParamsRegon14Multi vs -> ("Regony14zn", T.intercalate "," $ map unRegon14 vs)
  SearchParamsNipMulti vs -> ("Nipy", T.intercalate "," $ map unNip vs)
  SearchParamsKrsMulti vs -> ("Krsy", T.intercalate "," $ map unKrs vs)

data EntityType
  = EntityTypeP
  | EntityTypeF
  | EntityTypeLP
  | EntityTypeLF
  deriving (Eq, Show, Generic)

instance ToJSON EntityType where
  toJSON = \case
    EntityTypeP -> "P"
    EntityTypeF -> "F"
    EntityTypeLP -> "LP"
    EntityTypeLF -> "LF"

instance FromJSON EntityType where
  parseJSON = withText "EntityType" $ \case
    "P" -> pure EntityTypeP
    "F" -> pure EntityTypeF
    "LP" -> pure EntityTypeLP
    "LF" -> pure EntityTypeLF
    x -> fail $ "error: unexpected value for EntityType: " <> T.unpack x

entityTypeFromText :: Text -> Maybe EntityType
entityTypeFromText = \case
  "P" -> Just EntityTypeP
  "F" -> Just EntityTypeF
  "LP" -> Just EntityTypeLP
  "LF" -> Just EntityTypeLF
  _ -> Nothing

data EntitySilo
  = EntitySilo1 -- F | LF
  | EntitySilo2 -- F | LF
  | EntitySilo3 -- F | LF
  | EntitySilo4 -- F | LF
  | EntitySilo6 -- P | LP
  deriving (Eq, Show)

instance ToJSON EntitySilo where
  toJSON = \case
    EntitySilo1 -> Number 1
    EntitySilo2 -> Number 2
    EntitySilo3 -> Number 3
    EntitySilo4 -> Number 4
    EntitySilo6 -> Number 6

instance FromJSON EntitySilo where
  parseJSON = withScientific "EntitySilo" $ \case
    1 -> pure EntitySilo1
    2 -> pure EntitySilo2
    3 -> pure EntitySilo3
    4 -> pure EntitySilo4
    6 -> pure EntitySilo6
    _ -> parseFail "EntitySilo: invalid numeric value"

entitySiloFromText :: Text -> Maybe EntitySilo
entitySiloFromText = \case
  "1" -> Just EntitySilo1
  "2" -> Just EntitySilo2
  "3" -> Just EntitySilo3
  "4" -> Just EntitySilo4
  "6" -> Just EntitySilo6
  _ -> Nothing

data Bir11AggregateReport
  = Bir11NowePodmiotyPrawneOrazDzialalnoscOsFizycznych
  | Bir11AktualizowanePodmiotyPrawneOrazDzialnoscOsFizycznych
  | Bir11SkreslonePodmiotyPrawneOrazDzialalnoscOsFizycznych
  | Bir11NoweJednostkiLokalne
  | Bir11AktualizowaneJednostkiLokalne
  | Bir11JednostkiLokalneSkreslone
  deriving (Eq, Show)

data Bir11ParamName
  = StanDanych
  | KomunikatKod
  | KomunikatTresc
  | StatusSesji
  | StatusUslugi
  | KomunikatUslugi
  deriving (Eq, Show)

paramNameToText :: Bir11ParamName -> Text
paramNameToText = T.pack . show


instance Bir11FromText Regon where
  bir11FromText x =
        fromRegon9 <$> bir11FromText @Regon9 x
    <|> fromRegon14 <$> bir11FromText @Regon14 x

instance Bir11FromText Regon9 where
  bir11FromText x | T.length x == 9 = Right $ Regon9 x
  bir11FromText x = Left $ "failed to read Regon9 from: " <> T.unpack x

instance Bir11FromText Regon14 where
  bir11FromText x | T.length x == 14 = Right $ Regon14 x
  bir11FromText x = Left $ "failed to read Regon14 from: " <> T.unpack x

instance Bir11FromText Nip where
  bir11FromText x | T.length x == 10 = Right $ Nip x
  bir11FromText x = Left $ "failed to read Nip from: " <> T.unpack x

instance Bir11FromText Krs where
  bir11FromText x | T.length x == 10 = Right $ Krs x
  bir11FromText x = Left $ "failed to read Krs from: " <> T.unpack x

instance Bir11FromText EntityType where
  bir11FromText x = case entityTypeFromText x of
    Just t -> Right t
    Nothing -> Left $ "failed to read EntityType form: " <> T.unpack x

instance Bir11FromText EntitySilo where
  bir11FromText x = case entitySiloFromText x of
    Just s -> Right s
    Nothing -> Left $ "failed to read EntitySilo from: " <> T.unpack x

