
module Mocks where

import           Control.Concurrent.MVar
import           GHC.Exts              (Any)
import           Unsafe.Coerce         (unsafeCoerce)

mkMocks :: [a] -> IO (MVar [Any])
mkMocks as = newMVar $ map unsafeCoerce as
