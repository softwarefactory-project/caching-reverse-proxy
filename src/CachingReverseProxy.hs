{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Disable deprecation because we need Wai.requestBody
{-# OPTIONS_GHC -Wno-deprecations #-}

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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import Data.Digest.Pure.SHA (sha1, showDigest)
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

toFileName :: FilePath -> Wai.Request -> Maybe LBS.ByteString -> FilePath
toFileName base request bodyM =
  base <> decodeUtf8 (Wai.rawPathInfo request <> Wai.rawQueryString request <> extra) <> ".raw"
  where
    extra = case bodyM of
      Just body -> mappend "-" . encodeUtf8 . showDigest . sha1 $ body
      Nothing -> ""

-- | Return a file path, with the request body if it is not too big
toFileNameWithBody :: FilePath -> Wai.Request -> IO (FilePath, Maybe LBS.ByteString)
toFileNameWithBody fp request = do
  bodyM <- case Wai.requestBodyLength request of
    Wai.KnownLength len | 0 < len && len < 1024 * 1024 * 10 -> Just <$> Wai.strictRequestBody request
    _ -> pure Nothing
  pure (toFileName fp request bodyM, bodyM)

ensureFile :: FilePath -> IO Handle
ensureFile fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  openFile fp WriteMode

writeInfo :: FilePath -> LBS.ByteString -> IO ()
writeInfo fp content = do
  cached <- doesFileExist fp
  unless cached $ do
    handle <- ensureFile fp
    LBS.hPut handle content
    hClose handle

-- | A helper function to create a static request body producer.
-- The http-client expects the requestBody IO to produce a mempty result when the body have been consumed
-- This implementation uses an IORef to store the current state so that:
-- - first IO produces the body
-- - next IO produces mempty
staticRequestBody :: LBS.ByteString -> IO (IO ByteString)
staticRequestBody body = do
  sendRef <- newIORef False
  pure (go sendRef)
  where
    go sendRef = do
      sent <- readIORef sendRef
      if sent
        then pure mempty
        else do
          writeIORef sendRef True
          pure $ toStrict body

runCRP :: FilePath -> Port -> Maybe ProxyDest -> IO ()
runCRP cachePath port dest = do
  manager <- newManager tlsManagerSettings
  -- TODO: handle concurrency.
  -- Here we are using a IORef to share the cache path between the request and response handler
  -- This is because the path is created using the body which is not available in the response handler
  -- because it has been already consumed.
  fpRef <- newIORef ""
  -- Run a warp service for the app
  putTextLn $ "Listing on port " <> show port
  Warp.run port (app fpRef manager)
  where
    -- The app is a proxy configured with 'handleRequest' and 'handleResponse'
    app fpRef manager = waiProxyToSettings (handleRequest fpRef) (setting fpRef) manager
    setting fpRef = defaultWaiProxySettings {wpsProcessBody = handleResponse fpRef}
    -- 'handleRequest' run when a client makes a request
    handleRequest :: IORef FilePath -> Wai.Request -> IO WaiProxyResponse
    handleRequest fpRef request' = do
      -- Get cache path file name
      (fp, bodyM) <- toFileNameWithBody cachePath request'
      -- Adapt the initial request
      reqBody <- case bodyM of
        Nothing -> pure (pure mempty)
        Just body -> staticRequestBody body
      let request =
            request'
              { Wai.requestHeaders = fixupHeaders (Wai.requestHeaders request'),
                Wai.requestBody = reqBody
              }
      putTextLn $ "Serving: " <> show request
      cached <- doesFileExist fp
      if cached
        then pure . WPRResponse $ Wai.responseFile status200 [("Content-Type", "application/json")] fp Nothing
        else do
          -- save file path ref for response handler
          writeIORef fpRef fp
          case bodyM of
            Just body -> writeInfo (fp <> "-body") body
            Nothing -> pure ()
          pure $ case dest of
            Just pd@(ProxyDest _ 443) -> WPRModifiedRequestSecure request pd
            Just pd -> WPRModifiedRequest request pd
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
    handleResponse fpRef _request response = case responseStatus response of
      (Status code _message) | 200 <= code && code < 300 -> Just processConduit
      _ -> Nothing
      where
        processConduit :: MonadIO m => Conduit.ConduitT ByteString (Conduit.Flush Binary.Builder) m ()
        processConduit = do
          --  TODO: figure out how to use Conduit.bracketP here
          fp <- readIORef fpRef
          putTextLn $ "Caching response to " <> show fp
          handle <- liftIO $ ensureFile fp
          res <- Conduit.mapM (go handle)
          liftIO $ hClose handle
          pure res
        go :: MonadIO m => Handle -> ByteString -> m (Conduit.Flush Binary.Builder)
        go handle bs = do
          liftIO $ BS.hPut handle bs
          pure $ Conduit.Chunk (Binary.fromByteString bs)
