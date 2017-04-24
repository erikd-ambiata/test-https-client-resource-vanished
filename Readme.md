# test-http-client-tls-resource-vanished

This is a repo contains a trivially simple HTTPS server and a http-client-tls
client that tries to show up a bug..

Building and running it is as simple as:

```
cabal sandbox init
cabal install --dependencies-only
cabal build
dist/build/test-http-https-server/test-http-https-server
```

