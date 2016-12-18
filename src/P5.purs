module P5 where

import Prelude
import Control.Coroutine (Consumer, Producer, Process, await, emit, runProcess, ($$), ($~))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Monad.Writer (lift)
import Control.Parallel (class Parallel)
import Data.String (charAt, singleton, take)
import Util (md5, unsafeFromMaybe)

input :: String
input = "ojvtpuvg"

test :: Eff _ Unit
test = void $ launchAff do
  log "start"
  solve1 input
  log "done"

solve1 str = runProcess (nats $$ (check str))

check :: forall a eff.
  String  ->
  Consumer Int  _ a
check str = forever do
  i <- await
  let hash = md5 (str <> show i)
  when (i `mod` 1000000 == 0) $ lift (logShow i)
  when (valid hash) $ lift (sol hash # singleton # log)
  where
    valid :: String -> Boolean
    valid hash = (take 5 hash) == "00000"

    sol :: String -> Char
    sol hash = charAt 5 hash # unsafeFromMaybe

nats :: forall m. (Monad m) => Producer Int m Unit
nats = go 0
  where
    go i = do
      emit i
      go (i + 1)
