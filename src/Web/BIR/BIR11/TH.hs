module Web.BIR.BIR11.TH where

import Control.Lens.TH ( makeLenses )
import Data.Aeson.TH ( defaultOptions, Options(fieldLabelModifier), deriveToJSON )
import Data.Char (toLower)
import Language.Haskell.TH.Syntax ( Q, Dec, Name, nameBase )


lowerCaseFirst :: String -> String
lowerCaseFirst = \case
  x:xs -> toLower x : xs 
  "" -> ""

deriveAll' :: Int -> Name -> Q [Dec]
deriveAll' prefixLen name = do
  ls <- makeLenses name
  js <- deriveToJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop prefixLen} name
  pure $ ls ++ js

deriveAllPrefixed :: Name -> Q [Dec]
deriveAllPrefixed name = do
  deriveAll' prefixLen name
  where
    nm = nameBase name
    prefixLen = length nm + 1 -- for leading underscore

deriveAll :: Name -> Q [Dec]
deriveAll = deriveAll' 1 -- for leading underscore
