{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import CachingReverseProxy (ProxyDest (..), runCRP)
import Options.Generic
import Relude

data CLIArgs w = CLIArgs
  { path :: w ::: FilePath <?> "Caching local directory",
    port :: w ::: Int <?> "Listening port number",
    destHost :: w ::: Text <?> "The remote hostname",
    destPort :: w ::: Int <?> "The remote port"
  }
  deriving stock (Generic)

instance ParseRecord (CLIArgs Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  args <- unwrapRecord "caching-reverse-proxy"
  runCRP
    (path args)
    (port args)
    (Just $ ProxyDest (encodeUtf8 . destHost $ args) (destPort args))
