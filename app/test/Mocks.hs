
module Mocks where

import           Control.Concurrent.MVar
import           GHC.Exts              (Any)
import           Unsafe.Coerce         (unsafeCoerce)
import           Data.Aeson            (ToJSON, FromJSON, Value, toJSON)

mkMocks :: (ToJSON a, FromJSON a) => [a] -> IO (MVar [Value])
mkMocks as = newMVar $ map toJSON as
