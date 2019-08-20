{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}

module Runtime.Interpreter where

import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.UUID          (toString)
import           Data.Maybe         (isJust)
import qualified Data.IntMap as MArr
import           Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import           Data.UUID.V4       (nextRandom)
import           Data.Aeson         (ToJSON, FromJSON, encode, decode)
import           Data.Proxy         (Proxy(..))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           Runtime.Types
import           Playback.Types
import           Playback.Machine
import           Playback.Entries
import           Language

-- interpretFlowFTest :: FlowF a -> IO a
-- interpretFlowFTest (GenerateGUID next) = pure $ next "111"
-- interpretFlowFTest (RunIO ioAct next)  = error "IO not supported in tests."
-- interpretFlowFTest (LogInfo msg next)  = pure $ next ()


withRunMode
  :: RRItem rrItem
  => MockedResult rrItem native
  => RunMode
  -> (native -> rrItem)
  -> IO native
  -> IO native
withRunMode RegularMode _ act = act
withRunMode (RecordingMode recorderRt) mkRRItem act
  = record recorderRt Proxy mkRRItem act
withRunMode (ReplayingMode playerRt) mkRRItem act
  = replay playerRt mkRRItem act


interpretFlowF :: Runtime -> FlowF a -> IO a
interpretFlowF rt (GenerateGUID next) =
  next <$> withRunMode (runMode rt) mkGenerateGUIDEntry
    (toString <$> nextRandom)
interpretFlowF rt (RunIO ioAct next) =
  next <$> withRunMode (runMode rt) mkRunIOEntry ioAct
interpretFlowF rt (LogInfo msg next) =
  next <$> withRunMode (runMode rt)
    (mkLogInfoEntry msg)
    (putStrLn msg)

runFlow :: Runtime -> Flow a -> IO a
runFlow rt = foldFree (interpretFlowF rt)
