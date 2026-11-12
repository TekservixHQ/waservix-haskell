{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (try)
import Control.Monad (unless)
import Network.WAservix
import System.Exit (exitFailure)

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
  pure ok

main :: IO ()
main = do
  r1 <- check "accepts the matching secret" (verifyWebhook "s3cret" "s3cret")
  r2 <- check "rejects a different secret" (not (verifyWebhook "wrong!" "s3cret"))
  r3 <- check "rejects an empty header" (not (verifyWebhook "" "s3cret"))
  c <- newClient "wpk_invalid_haskell_test"
  res <- try (getStatus c)
  r4 <- check "invalid key gives a 401 WAservixError" $ case res of
    Left e -> errorStatus e == 401
    Right _ -> False
  unless (and [r1, r2, r3, r4]) exitFailure
