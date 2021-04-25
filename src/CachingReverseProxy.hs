{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- A caching reverse proxy
module CachingReverseProxy
  ( runCRP,
    ProxyDest (..),
  )
where

import qualified Data.Binary.Builder as Binary
import qualified Data.ByteString as BS
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import Network.HTTP.Client (newManager, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Status (Status (..), status200)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.IO (hClose, openFile)

toFileName :: FilePath -> Wai.Request -> FilePath
toFileName base request =
  base <> decodeUtf8 (Wai.rawPathInfo request <> Wai.rawQueryString request) <> ".raw"

ensureFile :: FilePath -> IO Handle
ensureFile fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  openFile fp WriteMode

runCRP :: FilePath -> Port -> Maybe ProxyDest -> IO ()
runCRP cachePath port dest = do
  manager <- newManager tlsManagerSettings
  -- Run a warp service for the app
  putTextLn $ "Listing on port " <> show port
  Warp.run port (app manager)
  where
    -- The app is a proxy configured with 'handleRequest' and 'handleResponse'
    app manager = waiProxyToSettings handleRequest setting manager
    setting = defaultWaiProxySettings {wpsProcessBody = handleResponse}
    -- 'handleRequest' run when a client makes a request
    handleRequest :: Wai.Request -> IO WaiProxyResponse
    handleRequest request' = do
      let request = request' {Wai.requestHeaders = fixupHeaders (Wai.requestHeaders request')}
      putTextLn $ "Serving: " <> show request
      let fp = toFileName cachePath request
      cached <- doesFileExist fp
      pure $
        if cached
          then WPRResponse $ Wai.responseFile status200 [("Content-Type", "application/json")] fp Nothing
          else case dest of
            Just pd@(ProxyDest _ 443) -> WPRModifiedRequestSecure request pd
            Just pd -> WPRProxyDest pd
            _ -> error "No destination and no cache"
    fixupHeaders headers = map setHost $ filter keepHeader headers
    setHost ("Host", host) = case dest of
      Just (ProxyDest host' port') -> ("Host", host' <> ":" <> show port')
      Nothing -> ("Host", host)
    setHost x = x
    -- remove empty auth
    keepHeader ("Authorization", "token ") = False
    keepHeader _ = True
    -- 'handleResponse' run when the request hit the dest
    handleResponse request response = case responseStatus response of
      (Status code _message) | 200 <= code && code < 300 -> Just processConduit
      _ -> Nothing
      where
        fp = toFileName cachePath request
        processConduit :: MonadIO m => Conduit.ConduitT ByteString (Conduit.Flush Binary.Builder) m ()
        processConduit = do
          putTextLn $ "Caching response to " <> show fp
          --  TODO: figure out how to use Conduit.bracketP here
          handle <- liftIO $ ensureFile fp
          res <- Conduit.mapM (go handle)
          liftIO $ hClose handle
          pure res
        go :: MonadIO m => Handle -> ByteString -> m (Conduit.Flush Binary.Builder)
        go handle bs = do
          liftIO $ BS.hPut handle bs
          pure $ Conduit.Chunk (Binary.fromByteString bs)
