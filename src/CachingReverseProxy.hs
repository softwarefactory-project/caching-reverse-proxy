{- |
Copyright: (c) 2021 Tristan de Cacqueray
SPDX-License-Identifier: NONE
Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>

A caching reverse proxy
-}

module CachingReverseProxy
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
