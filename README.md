# caching-reverse-proxy

[![Hackage](https://img.shields.io/hackage/v/caching-reverse-proxy.svg?logo=haskell)](https://hackage.haskell.org/package/caching-reverse-proxy)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

A caching reverse proxy to help client development.

## Example

```ShellSession
$ caching-reverse-proxy --path "/srv/bugzilla.redhat.com" --port 8080 --dest-host "bugzilla.redhat.com" --dest-port 443
Listing on port 8080
-- in another shell, run: curl localhost:8080/rest/bug/42042
Serving: Request {requestMethod = "GET", httpVersion = HTTP/1.1, rawPathInfo = "/rest/bug/42042", ... }
Caching response to "/srv/bugzilla.redhat.com/rest/bug/42042.raw"
```

Subsequent requests may be served directly from the cache.

## Contribute

To work on this project you need a Haskell toolchain, for example on fedora:

```
$ sudo dnf install -y ghc cabal-install && cabal update
```

Run the cli:

```
$ cabal run caching-reverse-proxy -- --help
```
