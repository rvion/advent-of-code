module Demo where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromMaybe)
import Partial.Unsafe (unsafeCrashWith)

main = log "ok"

-- unsafeFromMaybe1 mba = fromMaybe (unsafeCrashWith "error 1") mba
-- unsafeFromMaybe2 :: forall t1. Maybe t1 -> t1
unsafeFromMaybe2     = fromMaybe (unsafeCrashWith "error 2")
