module Demo where

import Prelude
import Control.Coroutine (await, emit, runProcess, ($$))
import Control.Monad.Cont (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Parallel (class Parallel)

-- nats :: forall m. (Monad m) => Producer Int m Unit
nats = go 0 where go i = do emit i *> go (i + 1)

-- printer :: forall a. Consumer String (Eff _) Unit
printer = forever do
  s <- await
  when (s `mod` 100 == 0) $ lift (liftEff (logShow s))
  pure unit

-- $$ => imply a Parrallel constraint
-- main :: Eff _ Unit
main :: forall t38 t39 t42 t47.
  ( MonadRec t39
  , Parallel t42 t39
  , MonadEff
      ( console :: CONSOLE
      | t47
      )
      t39
  ) => t39 t38
main = runProcess (nats $$ printer)
