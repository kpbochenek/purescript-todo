module LocalStore(putL, getLL, delL) where

import Control.Monad ((<$>))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Effect (Effect)
  
foreign import putL :: String -> String -> Effect Unit

foreign import getL :: String -> Effect (Nullable String)

foreign import delL :: String -> Effect Unit

getLL :: String -> Effect (Maybe String)
getLL key = toMaybe <$> (getL key)