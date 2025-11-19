{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Effectful
import Effectful.Retry
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
        testGroup
            "retry-effectful"
            [ testCase "`retrying` works" $ do
                res <- runRetryEff testRetrying
                assertEqual "match" res 42
            ]

runRetryEff :: Eff '[Retry, IOE] a -> IO a
runRetryEff = runEff . runRetry

testRetrying :: (Retry :> es) => Eff es Int
testRetrying = do
    retrying (constantDelay 100) chk (const $ pure 42)
  where
    chk _ 42 = pure False
    chk _ _ = pure True
