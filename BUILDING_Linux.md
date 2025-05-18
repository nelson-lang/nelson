# Building Nelson on Linux

This guide provides instructions on how to build Nelson on Linux.
For the most up-to-date information on dependencies and build instructions, refer to the [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) file or [nix.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/nix.yml) .

## Nix

Nelson provides a [Nix](https://nix.dev/manual/nix/2.17/command-ref/new-cli/nix3-develop) development environment for working on the project. This is the recommended setup for Linux and macOS users.

### [Install Nix](https://nixos.org/download/)

```bash
curl -L https://nixos.org/nix/install | sh -s -- --daemon
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.conf
```

restart your terminal

### Development Workflow

Once Nix is installed, enter the development environment with:

```bash
nix develop
```

Then, use the following commands:

`just config` — Configure the build environment

`just build` — Build the project

`just start` — Run Nelson

## NixOS

To build Nelson on NixOS, you can use the provided [default.nix](https://github.com/nelson-lang/nelson/blob/master/default.nix) file.

### Using the Latest Master Version

To fetch the latest master version and build Nelson:
replace sha256 by desired/latest sha256 commit.

### Building Nelson

To build Nelson using the default.nix file:

```bash
nix-build
```

### Evaluate Nelson

```bash
nix-shell
./result/bin/nelson
```

### Install in your environment:

```bash
nix-env -i ./result
```

## ArchLinux

Nelson is available as an ArchLinux package. You can install it using `paru`:

```bash
paru nelson-git
```

## You can also build nelson with a micromamba environment

After installing micromamba

```bash
micromamba env create -f environment-linux-dev.yml
micromamba activate nelson
```

see [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/mamba.yml) with micromamba job.

- CMake options:

```bash
cmake -LAH

// Build Nelson under LGPL v3.x only
LGPL_ONLY:BOOL=OFF

// Build mininal Nelson
MINIMAL_BUILD:BOOL=ON

// Enable AVX2 Instruction set (if available)
ENABLE_AVX2=OFF

// Disable ASSERT_FUNCTIONS module.
WITHOUT_ASSERT_FUNCTIONS_MODULE:BOOL=OFF

// Disable AUDIO module.
WITHOUT_AUDIO_MODULE:BOOL=OFF

// Disable CHARACTERS_ENCODING module.
WITHOUT_CHARACTERS_ENCODING_MODULE:BOOL=OFF

// Disable DATA_ANALYSIS module.
WITHOUT_DATA_ANALYSIS_MODULE:BOOL=OFF

// Disable DYNAMIC_LINK module.
WITHOUT_DYNAMIC_LINK_MODULE:BOOL=OFF

// Disable F2C module.
WITHOUT_F2C_MODULE:BOOL=OFF

// Disable FFTW module.
WITHOUT_FFTW_MODULE:BOOL=OFF

// Disable FILE_ARCHIVER module.
WITHOUT_FILE_ARCHIVER_MODULE:BOOL=OFF

// Disable GRAPHICS module.
WITHOUT_GRAPHICS_MODULE:BOOL=OFF

// Disable GUI module.
WITHOUT_GUI_MODULE:BOOL=OFF

// Disable HDF5 module.
WITHOUT_HDF5_MODULE:BOOL=OFF

// Disable HELP_BROWSER module.
WITHOUT_HELP_BROWSER_MODULE:BOOL=OFF

// Disable HELP_TOOLS module.
WITHOUT_HELP_TOOLS_MODULE:BOOL=OFF

// Disable IPC module.
WITHOUT_IPC_MODULE:BOOL=OFF

// Disable JSON module.
WITHOUT_JSON_MODULE:BOOL=OFF

// Disable LOCALIZATION module.
WITHOUT_LOCALIZATION_MODULE:BOOL=OFF

// Disable MATIO module.
WITHOUT_MATIO_MODULE:BOOL=OFF

// Disable MEX module.
WITHOUT_MEX_MODULE:BOOL=OFF

// Disable MPI module.
WITHOUT_MPI_MODULE:BOOL=OFF

// Disable NIG module.
WITHOUT_NIG_MODULE:BOOL=OFF

// Disable OpenMP
WITHOUT_OPENMP:BOOL=OFF

// Disable PARALLEL module.
WITHOUT_PARALLEL_MODULE:BOOL=OFF

// Disable POLYNOMIAL_FUNCTIONS module.
WITHOUT_POLYNOMIAL_FUNCTIONS_MODULE:BOOL=OFF

// Disable QML_ENGINE module.
WITHOUT_QML_ENGINE_MODULE:BOOL=OFF

// Disable RANDOM module.
WITHOUT_RANDOM_MODULE:BOOL=OFF

// Disable SIGNAL_PROCESSING module.
WITHOUT_SIGNAL_PROCESSING_MODULE:BOOL=OFF

// Disable SIO_CLIENT module.
WITHOUT_SIO_CLIENT_MODULE:BOOL=OFF

// Disable SLICOT module.
WITHOUT_SLICOT_MODULE:BOOL=OFF

// Disable SPECIAL_FUNCTIONS module.
WITHOUT_SPECIAL_FUNCTIONS_MODULE:BOOL=OFF

// Disable STATISTICS module.
WITHOUT_STATISTICS_MODULE:BOOL=OFF

// Disable TESTS_MANAGER module.
WITHOUT_TESTS_MANAGER_MODULE:BOOL=OFF

// Disable TEXT_COMPLETION module.
WITHOUT_TEXT_COMPLETION_MODULE:BOOL=OFF

// Disable TEXT_EDITOR module.
WITHOUT_TEXT_EDITOR_MODULE:BOOL=OFF

// Disable TRIGONOMETRIC_FUNCTIONS module.
WITHOUT_TRIGONOMETRIC_FUNCTIONS_MODULE:BOOL=OFF

// Disable VALIDATORS module.
WITHOUT_VALIDATORS_MODULE:BOOL=OFF

// Disable WEBTOOLS module.
WITHOUT_WEBTOOLS_MODULE:BOOL=OFF

```

Standard build using Just (>= 1.33):

```bash
cd nelson
just config
just build
just build_help
just minimal_tests
```

Minimal build:

```bash
cd nelson
just config -DCMAKE_BUILD_TYPE=Release -DMINIMAL_BUILD=ON -G "Unix Makefiles" .
just build
```

Some tips:

- On local build, you can build optimized version using AVX2 instruction set

```bash
cd nelson
just config -DCMAKE_BUILD_TYPE=Release -DENABLE_AVX2=ON -G "Unix Makefiles" .
just build
just build_help
```

- Build Nelson with clang-tidy fix

```bash
  cd nelson
  just config -DENABLE_CLANG_TIDY_FIX=ON -G "Unix Makefiles" .
  just build
```

- Update localization files if you modify it (optional, only for dev):

```bash
just update-localization
```

- launch Nelson:

```bash
cd nelson
just start
```

[Previous (Building)](BUILDING.md)
