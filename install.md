# Introduction

`buildsome` is a Haskell build system. To install it, first install
these prerequisites:

# Prerequisites

## The Haskell "Stack" tool

The Haskell tool
[Stack](https://github.com/commercialhaskell/stack#how-to-install) is
an alternative to the older cabal-install tool, and is the easiest way
to build and install buildsome.

## leveldb

Install the leveldb development packages.

| OS     | install command                           |
| ------ | ----------------------------------------- |
| Ubuntu | ```sudo apt-get install libleveldb-dev``` |
| Centos | ```sudo yum install leveldb-devel```      |

# Installation

Run the following commands:

```
git clone https://github.com/buildsome/buildsome
cd buildsome
stack install
```

On NixOS use the following command:

```
nix-shell -p stack --run 'stack install --nix-packages leveldb'
```

## `$PATH` variable

Stack installs buildsome at `$HOME/.local/bin`, so make sure
`$HOME/.local/bin` is in your `$PATH`.

## The `.stack-work` directory

Stack creates a local `.stack-work` directory where intermediate build
outputs are created, serving as a build sandbox.

Due to an [open
issue](https://github.com/commercialhaskell/stack/issues/848) with the
Stack build tool, the 'stack install' command installs a `buildsome`
executable that refers to the `.stack-work` sandbox in which
`buildsome` was built, so it must not be deleted or moved.
