# granule-compiler-llvm

LLVM compiler for Granule. This is a prototype which compiles on the functional core of Granule and does not yet support custom ADTs and GADTs.

# Installation

Installation requires [Stack](https://docs.haskellstack.org/en/stable/README/) and LLVM 12.

Build and install using Stack. The following will install the `grc` binary:

```
stack install :grc
```

## Installing LLVM 12

LLVM 12 is available on Mac via Homebrew (until July 2025). It is available on _some_ Linux distributions via apt using the default repositories or through the [LLVM apt repository](https://apt.llvm.org/).

```bash
# mac
brew install llvm@12

# linux
apt install llvm-12
```
