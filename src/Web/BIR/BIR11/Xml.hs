module Web.BIR.BIR11.Xml where

import Control.Lens ( (^.), (^..) )
import Data.CaseInsensitive qualified as CI
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Only ( Only(Only, fromOnly) )
import Text.Read (readMaybe)
import Text.XML.Lens ( Document, Element, (...), named, root, text )
import GHC.Generics



-- | BIR11 results that can be deocoded from inner xml
class Bir11FromXmlDoc a where
  bir11FromXmlDoc :: Document -> Either Text a

  default bir11FromXmlDoc :: (Bir11FromXml a) => Document -> Either Text a
  bir11FromXmlDoc = fmap fromOnly . bir11FromXmlDoc

instance Bir11FromXml a => Bir11FromXmlDoc [a] where
  bir11FromXmlDoc d =
    let xs = d ^.. root . named "root" ... named "dane"
    in mapM bir11FromXml xs

instance Bir11FromXml a => Bir11FromXmlDoc (Only a) where
  bir11FromXmlDoc d =
    let xs = d ^.. root . named "root" ... named "dane"
    in case xs of
      [x] -> Only <$> bir11FromXml x
      _ -> Left "error: expected exactly one element"


-- | BIR11 result types that can me decoded from Text.Xml.Element
--
-- The Element is expected to be a "dane" node of inner xml from the BIR 1.1 SOAP response.
--
-- For Generic types this defaults to using GBir11FromXml.
class Bir11FromXml a where
  bir11FromXml :: Element -> Either Text a

  default bir11FromXml :: (Generic a, GBir11FromXml (Rep a)) => Element -> Either Text a
  bir11FromXml = fmap GHC.Generics.to . gbir11FromXml


-- | Types that can be decoded from Text in context of BIR11 responses
class Bir11FromText a where
  bir11FromText :: Text -> Either Text a

instance Bir11FromText Text where
  bir11FromText = Right

instance Bir11FromText Int where
  bir11FromText x =
    case readMaybe $ T.unpack x of
      Just n -> Right n
      Nothing -> Left $ "failed to read Int from: " <> x

instance Bir11FromText Bool where
  bir11FromText = \case
    "true" -> Right True
    "false" -> Right False
    x -> Left $ "failed to read Bool from: " <> x

instance Bir11FromText Day where
  bir11FromText x =
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" $ T.unpack x of
      Just d -> Right d
      Nothing -> Left $ "failed to read Day from: " <> x

-- Empty text is mapped to Nothing but otherwise parse errors are carried through
instance (Bir11FromText a) => Bir11FromText (Maybe a) where
  bir11FromText "" = Right Nothing
  bir11FromText x =
    case bir11FromText x of
      Right a -> Right $ Just a
      Left err -> Left $ "failed to read Maybe a: " <> err


-- | Generic version of Bir11FromXml
class GBir11FromXml f where
  gbir11FromXml :: Element -> Either Text (f a)


instance GBir11FromXml U1 where
  gbir11FromXml _ = Right U1

instance (Bir11FromXml c) => GBir11FromXml (K1 i c) where
  gbir11FromXml = fmap K1 . bir11FromXml

instance (GBir11FromXml f) => GBir11FromXml (M1 D c f) where
  gbir11FromXml = fmap M1 . gbir11FromXml

instance (GBir11FromXml f) => GBir11FromXml (M1 C c f) where
  gbir11FromXml = fmap M1 . gbir11FromXml

instance (Bir11FromText a, Selector s) => GBir11FromXml (M1 S s (K1 i a)) where
  gbir11FromXml = fmap M1 . parse
    where
      -- Some lens heuristics here, if the name starts with an underscore it will be stripped before proceeding
      sname = T.pack $ selName (undefined :: M1 _i s _f _p)

      parse e =
        case e ^.. named "dane" ... named (CI.mk sname) of
          [] -> Left $ "decoding failure, property not found: " <> sname
          [x] -> K1 <$> bir11FromText (x ^. text)
          _ -> Left $ "decoding failure, multiple nodes matched property with maxOccurs=1: " <> sname

instance (GBir11FromXml a, GBir11FromXml b) => GBir11FromXml (a :*: b) where
  gbir11FromXml x =
    (:*:) <$> gbir11FromXml x
          <*> gbir11FromXml x
