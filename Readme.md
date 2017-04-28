# test-https-client-resource-vanished

This is a repo contains a trivially simple HTTPS server and a http-client-tls
client that tries to show up a bug.

The repo contains a `cabal.config` file specifying the versions of all the
dependencies so it should be trivial to reproduce this setup with either
stack or mafia. I can reproduce this with both GHC 8.0.1 and 8.0.2. I didn't
try earlier versions of GHC because I didn't think it relevant.

## The Problem
The problem this code is trying to reproduce is exceptions from the `tls`
library bubbling up to the top level and killing the process.

Reproducing this probably requires Linux because it uses the `timeout` program
from the GNU `coreutils` package.

It also requires running the server and client (both in this repo) on different
physical machines. Running both on localhost *does not work*. In my testing I
was running this on two AWS instances in different availability zones to ensure
that I wasn't just getting two VMs on the same hardware.

## Reproduction
One the first machine:
```
git clone https://github.com/erikd/test-https-client-resource-vanished
cd test-https-client-resource-vanished
cabal sandbox init
cabal install --dependencies-only
cabal build
scripts/https-server-runner.sh
```
This runs a trivial `warp-tls` server that serves up an 11Meg text file
regardless of what is requested. It also uses GNU `coreutils` to kill the
process every second and restart it (this is to emultate a broken network).

On the second machine:
```
git clone https://github.com/erikd/test-https-client-resource-vanished
cd test-https-client-resource-vanished
cabal sandbox init
cabal install --dependencies-only
cabal build
time dist/build/https-client/https-client <IP address of the first machine>
```

After anything up to 5 minutes you should see the second machine do this:
```
...5H...5...5...5H...5H...5H...5H...5...5H...5H...5...5...5...5...5...5...5...5.
..5...5...5...5...5...5...5...5...5...5H...5...5...5...5...5...5...5...5...5...5
...https-client: Error_Packet "partial packet: expecting 16408 bytes, got: 9688"
```
