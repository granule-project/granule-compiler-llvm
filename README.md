# granule-compiler-llvm

LLVM compiler for Granule. This is a prototype which compiles on the functional core of Granule and does not yet support custom ADTs and GADTs.

# Installation

Installation requires [Stack](https://docs.haskellstack.org/en/stable/README/) and [LLVM 12](https://releases.llvm.org/12.0.0/docs/index.html).

Build and install using Stack. The following will install the `grlc` binary:

```
stack install
```

## Installing LLVM 12

LLVM 12 is available on _some_ macOS versions via Homebrew and _some_ Linux distributions via their default apt repositories or the [LLVM apt repository](https://apt.llvm.org/).

```bash
# mac
brew install llvm@12

# linux
sudo apt install llvm-12
```

If you are not able to install through a package manager, or if you encounter any unexpected issues during building this project (especially `llvm-config` errors on macOS), which you are not able to resolve, you might need to build LLVM from source.

## Building LLVM 12

These are the steps I took to build LLVM 12 on an M1 MacBook running Sequoia 15.1. The process is very similar on Linux, minus the macOS specifics.

1. Install build dependencies:

```bash
brew install cmake ninja
```

2. Get the source, create a build directory:

```bash
git clone https://github.com/llvm/llvm-project -b release/12.x --single-branch --depth 1
cd llvm-project/llvm
mkdir build && cd build
```

3. Build. Choose a sensible installation path. The dylib flags are necessary, the rest are for build speed:

```
# setup
cmake -Wno-dev -G "Ninja" .. \
-DCMAKE_BUILD_TYPE=Release \
-DCMAKE_INSTALL_PREFIX=/usr/local/llvm-12 \
-DLLVM_TARGETS_TO_BUILD="AArch64" \
-DLLVM_INCLUDE_TOOLS=ON \
-DLLVM_INCLUDE_EXAMPLES=OFF \
-DLLVM_INCLUDE_TESTS=OFF \
-DLLVM_INCLUDE_BENCHMARKS=OFF \
-DLLVM_ENABLE_BINDINGS=OFF \
-DLLVM_BUILD_LLVM_DYLIB=ON \
-DLLVM_LINK_LLVM_DYLIB=ON

# build (take a break!)
ninja

# install
sudo ninja install
```

4. Clean up and permissions. The `-12` suffix is sometimes expected on the dylib. The `install_name_tool` usage is for [SIP](https://support.apple.com/en-gb/102149):

```
cd /usr/local/llvm-12/lib
sudo ln -s libLLVM.dylib libLLVM-12.dylib
sudo install_name_tool -id $PWD/libLTO.dylib libLTO.dylib
sudo install_name_tool -id $PWD/libLLVM.dylib libLLVM.dylib
sudo install_name_tool -change '@rpath/libLLVM.dylib' $PWD/libLLVM.dylib libLTO.dylib

```

5. Add the following to your .zshrc or equivalent:

```bash
export PATH="/usr/local/llvm-12/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/llvm-12/lib:$LD_LIBRARY_PATH"
```
