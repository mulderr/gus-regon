{-# language ConstraintKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language DeriveAnyClass #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}

module Web.BIR.BIR11.Methods.Lifted where

import Control.Lens.TH
import Control.Monad.Except (MonadIO(..), MonadError(throwError), liftEither)
import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Web.BIR.BIR11.Methods as M
import Web.BIR.BIR11.Types
import Web.BIR.BIR11.Types.Report
import Web.BIR.BIR11.Types.Report.Bir11OsPrawna ( Bir11OsPrawna )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDaneOgolne ( Bir11OsFizycznaDaneOgolne )
import Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsFizycznej ( Bir11JednLokalnaOsFizycznej )
import Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsPrawnej ( Bir11JednLokalnaOsPrawnej )
import Web.BIR.BIR11.Xml ( Bir11FromXmlDoc )


data DetailedReport
  = DetailedReportP Bir11OsPrawna
  | DetailedReportF Bir11OsFizycznaDaneOgolne
  | DetailedReportLP Bir11JednLokalnaOsPrawnej
  | DetailedReportLF Bir11JednLokalnaOsFizycznej
  deriving (Eq, Show, Generic, ToJSON)

data DetailedResult = DetailedResult
  { _detailedResultSearch :: SearchResult
  , _detailedResultReport :: DetailedReport
  } deriving (Eq, Show, Generic)


class HasBirState m where
  getBirApiUrl :: m String
  getBirApiKey :: m ApiKey
  getBirSessionKey :: m SessionKey
  putBirSessionKey :: SessionKey -> m ()

type MonadBir m = (MonadIO m, MonadError Bir11Error m, HasBirState m)


login :: MonadBir m => m ()
login = do
  url <- getBirApiUrl
  key <- getBirApiKey
  skey <- liftEither =<< liftIO (M.login url key)
  putBirSessionKey skey

search :: MonadBir m => SearchParams -> m [SearchResult]
search params = do
  url <- getBirApiUrl
  skey <- getBirSessionKey
  liftEither =<< liftIO (M.search url skey params)

fullReport :: (MonadBir m, Bir11FromXmlDoc a) => Bir11FullReport a -> m a
fullReport report = do
  url <- getBirApiUrl
  skey <- getBirSessionKey
  liftEither =<< liftIO (M.fullReport url skey report)

getValue :: MonadBir m => Bir11ParamName -> m Text
getValue param = do
  url <- getBirApiUrl
  skey <- getBirSessionKey
  liftEither =<< liftIO (M.getValue url skey param)

logout :: MonadBir m => m Bool
logout = do
  url <- getBirApiUrl
  skey <- getBirSessionKey
  liftEither =<< liftIO (M.logout url skey)

searchDetailed :: forall m. (MonadBir m) => SearchParams -> m (Maybe DetailedResult)
searchDetailed params = do
  rs <- search params
  case rs of
    [] -> pure Nothing
    (x:_) -> do
      case _typ x of
        Just EntityTypeF -> do
          reg <- getRegon9 x
          r <- fullReport (Bir11FrOsFizycznaDaneOgolne reg)
          pure $ Just $ DetailedResult x $ DetailedReportF r
        Just EntityTypeP -> do
          reg <- getRegon9 x
          r <- fullReport (Bir11FrOsPrawna reg)
          pure $ Just $ DetailedResult x $ DetailedReportP r
        Just EntityTypeLF -> do
          reg <- getRegon14 x
          r <- fullReport (Bir11FrJednLokalnaOsFizycznej reg)
          pure $ Just $ DetailedResult x $ DetailedReportLF r
        Just EntityTypeLP -> do
          reg <- getRegon14 x
          r <- fullReport (Bir11FrJednLokalnaOsPrawnej reg)
          pure $ Just $ DetailedResult x $ DetailedReportLP r
        Nothing -> throwError $ Bir11ProtocolError "Entity type is missing"
  where
    getRegon9 = getRegonWith toRegon9
    getRegon14 = getRegonWith toRegon14

    getRegonWith :: (Regon -> Maybe a) -> SearchResult -> m a
    getRegonWith f x =
      case _regon x >>= f of
        Nothing -> throwError $ Bir11ProtocolError "Unexpected Regon value: failed to convert value to Regon9 or Regon14"
        Just r -> pure r

$(makeLenses ''DetailedResult)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 15} ''DetailedResult)
