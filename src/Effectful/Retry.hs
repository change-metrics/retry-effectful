{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | This module provides a Retry effect to adapt
 the @<https://hackage.haskell.org/package/retry retry>@ library for
 the @<https://hackage.haskell.org/package/effectful effectful>@ ecosystem.
-}
module Effectful.Retry (
    -- * Effect
    Retry,
    runRetry,

    -- * Types and Operations
    Control.Retry.RetryPolicyM (..),
    Control.Retry.retryPolicy,
    Control.Retry.retryPolicyDefault,
    Control.Retry.RetryAction (..),
    Control.Retry.toRetryAction,
    Control.Retry.RetryStatus (..),
    Control.Retry.defaultRetryStatus,
    Effectful.Retry.applyPolicy,
    Effectful.Retry.applyAndDelay,

    -- * Applying Retry Policies
    Effectful.Retry.retrying,
    Effectful.Retry.retryingDynamic,
    Effectful.Retry.recovering,
    Effectful.Retry.recoveringDynamic,
    Effectful.Retry.stepping,
    Effectful.Retry.recoverAll,
    Effectful.Retry.skipAsyncExceptions,
    Effectful.Retry.logRetries,
    Control.Retry.defaultLogMsg,
    -- Effectful.Retry.retryOnError,

    -- * Resumable variants
    Effectful.Retry.resumeRetrying,
    Effectful.Retry.resumeRetryingDynamic,
    Effectful.Retry.resumeRecovering,
    Effectful.Retry.resumeRecoveringDynamic,
    Effectful.Retry.resumeRecoverAll,

    -- * Retry Policies
    Control.Retry.constantDelay,
    Control.Retry.exponentialBackoff,
    Control.Retry.fullJitterBackoff,
    Control.Retry.fibonacciBackoff,
    Control.Retry.limitRetries,

    -- * Policy Transformers
    Control.Retry.limitRetriesByDelay,
    Control.Retry.limitRetriesByCumulativeDelay,
    Control.Retry.capDelay,

    -- * Development Helpers
    Control.Retry.simulatePolicy,
    Control.Retry.simulatePolicyPP,
) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Env (Env)

import Control.Exception (AsyncException, SomeAsyncException)
import Control.Monad.Catch
import Control.Retry

-- | An empty effect to provide the retry requirements, e.g. MonadIO and MonadMask
data Retry :: Effect

type instance DispatchOf Retry = 'Static 'WithSideEffects
data instance StaticRep Retry = Retry

-- | Peel the 'Retry' effect.
runRetry :: IOE :> es => Eff (Retry : es) a -> Eff es a
runRetry = evalStaticRep Retry

-- effect helper
policyIO :: Env es -> RetryPolicyM (Eff es) -> RetryPolicyM IO
policyIO env (RetryPolicyM policy) = RetryPolicyM $ \s -> unEff (policy s) env

-- effect helper
handlerIO :: Env es -> Handler (Eff es) a -> Handler IO a
handlerIO env (Handler handler) = Handler $ \e -> unEff (handler e) env

-- wrappers
applyPolicy :: Retry :> es => RetryPolicyM (Eff es) -> RetryStatus -> Eff es (Maybe RetryStatus)
applyPolicy policy status = unsafeEff $ \env -> Control.Retry.applyPolicy (policyIO env policy) status

applyAndDelay :: Retry :> es => RetryPolicyM (Eff es) -> RetryStatus -> Eff es (Maybe RetryStatus)
applyAndDelay policy status = unsafeEff $ \env -> Control.Retry.applyAndDelay (policyIO env policy) status

retrying ::
    Retry :> es =>
    RetryPolicyM (Eff es) ->
    (RetryStatus -> b -> Eff es Bool) ->
    (RetryStatus -> Eff es b) ->
    Eff es b
retrying = Effectful.Retry.resumeRetrying defaultRetryStatus

retryingDynamic ::
    Retry :> es =>
    RetryPolicyM (Eff es) ->
    (RetryStatus -> b -> Eff es RetryAction) ->
    (RetryStatus -> Eff es b) ->
    Eff es b
retryingDynamic = Effectful.Retry.resumeRetryingDynamic defaultRetryStatus

resumeRetrying ::
    Retry :> es =>
    RetryStatus ->
    RetryPolicyM (Eff es) ->
    (RetryStatus -> b -> Eff es Bool) ->
    (RetryStatus -> Eff es b) ->
    Eff es b
resumeRetrying retryStatus policy chk =
    Effectful.Retry.resumeRetryingDynamic
        retryStatus
        policy
        (\rs -> fmap toRetryAction . chk rs)

resumeRetryingDynamic ::
    Retry :> es =>
    RetryStatus ->
    RetryPolicyM (Eff es) ->
    (RetryStatus -> b -> Eff es RetryAction) ->
    (RetryStatus -> Eff es b) ->
    Eff es b
resumeRetryingDynamic retryStatus policy chk f =
    unsafeEff $ \env ->
        Control.Retry.resumeRetryingDynamic
            retryStatus
            (policyIO env policy)
            (\rs b -> unEff (chk rs b) env)
            (\rs -> unEff (f rs) env)

recovering ::
    Retry :> es =>
    RetryPolicyM (Eff es) ->
    [RetryStatus -> Handler (Eff es) Bool] ->
    (RetryStatus -> Eff es a) ->
    Eff es a
recovering = Effectful.Retry.resumeRecovering defaultRetryStatus

resumeRecovering ::
    Retry :> es =>
    RetryStatus ->
    RetryPolicyM (Eff es) ->
    [RetryStatus -> Handler (Eff es) Bool] ->
    (RetryStatus -> Eff es a) ->
    Eff es a
resumeRecovering retryStatus policy hs =
    Effectful.Retry.resumeRecoveringDynamic retryStatus policy hs'
  where
    hs' = map (fmap toRetryAction .) hs

recoveringDynamic ::
    Retry :> es =>
    RetryPolicyM (Eff es) ->
    [RetryStatus -> Handler (Eff es) RetryAction] ->
    (RetryStatus -> Eff es a) ->
    Eff es a
recoveringDynamic = Effectful.Retry.resumeRecoveringDynamic defaultRetryStatus

resumeRecoveringDynamic ::
    Retry :> es =>
    RetryStatus ->
    RetryPolicyM (Eff es) ->
    [RetryStatus -> Handler (Eff es) RetryAction] ->
    (RetryStatus -> Eff es a) ->
    Eff es a
resumeRecoveringDynamic retryStatus policy hs f =
    unsafeEff $ \env ->
        Control.Retry.resumeRecoveringDynamic
            retryStatus
            (policyIO env policy)
            (fmap (handlerIO env) <$> hs)
            (\rs -> unEff (f rs) env)

stepping ::
    Retry :> es =>
    RetryPolicyM (Eff es) ->
    [RetryStatus -> Handler (Eff es) Bool] ->
    (RetryStatus -> Eff es ()) ->
    (RetryStatus -> Eff es a) ->
    RetryStatus ->
    Eff es (Maybe a)
stepping policy hs schedule f s =
    unsafeEff $ \env ->
        Control.Retry.stepping
            (policyIO env policy)
            (fmap (handlerIO env) <$> hs)
            (\rs -> unEff (schedule rs) env)
            (\rs -> unEff (f rs) env)
            s

recoverAll ::
    Retry :> es =>
    RetryPolicyM (Eff es) ->
    (RetryStatus -> Eff es a) ->
    Eff es a
recoverAll = Effectful.Retry.resumeRecoverAll defaultRetryStatus

resumeRecoverAll :: Retry :> es => RetryStatus -> RetryPolicyM (Eff es) -> (RetryStatus -> Eff es a) -> Eff es a
resumeRecoverAll retryStatus policy f =
    unsafeEff $ \env ->
        Control.Retry.resumeRecoverAll
            retryStatus
            (policyIO env policy)
            (\rs -> unEff (f rs) env)

skipAsyncExceptions ::
    Retry :> es =>
    [RetryStatus -> Handler (Eff es) Bool]
skipAsyncExceptions = [asyncH, someAsyncH]
  where
    asyncH _ = Handler $ \(_ :: AsyncException) -> return False
    someAsyncH _ = Handler $ \(_ :: SomeAsyncException) -> return False

logRetries ::
    (Retry :> es, Exception e) =>
    (e -> Eff es Bool) ->
    (Bool -> e -> RetryStatus -> Eff es ()) ->
    RetryStatus ->
    Handler (Eff es) Bool
logRetries test reporter status = Handler $ \err -> do
    result <- test err
    reporter result err status
    return result

{- TODO
retryOnError
    :: (Error e :> es, Retry :> es)
    => RetryPolicyM (Eff es)
    -> (RetryStatus -> e -> Eff es Bool)
    -> (RetryStatus -> Eff es a)
    -> Eff es a
retryOnError policy chk f = go defaultRetryStatus
  where
    go stat = do
-}
