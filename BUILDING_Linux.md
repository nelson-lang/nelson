### Linux

- You can also see [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) file to help you to see dependencies. This file is up-to-date about how to build Nelson on each platform.

- [ArchLinux package](https://aur.archlinux.org/packages/nelson-git)

```bash
paru nelson
```

- You can also build nelson with a micromamba environment

After installing micromamba

```bash
micromamba env create -f environment-linux-dev.yml
micromamba activate nelson
```

see [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) with micromamba job.

- CMake options:

```bash
cmake -LAH

// Build Nelson under LGPL v3.x only
LGPL_ONLY:BOOL=OFF

// Build mininal Nelson
MINIMAL_BUILD:BOOL=ON

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

Standard build:

```bash
cd nelson
cmake -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" .
cmake --build . -- -j $(nproc)
cmake --build . -- buildhelp
cmake --build . -- tests_minimal
```

Minimal build:

```bash
cd nelson
cmake -DCMAKE_BUILD_TYPE=Release -DMINIMAL_BUILD=ON -G "Unix Makefiles" .
cmake --build . -- -j $(nproc)
```

Some tips:

- On Fedora distribution: replaces jack-audio-connection-kit-devel package by pipewire-jack-audio-connection-kit-devel

- Build Nelson with clang-tidy fix

```bash
  cd nelson
  cmake -DENABLE_CLANG_TIDY_FIX=ON -G "Unix Makefiles" .
```

- Update localization files if you modify it (optional, only for dev):

```bash
make updatelocalization
```

- launch Nelson:

```bash
cd nelson
./bin/linux/nelson.sh
```

or

```bash
cd nelson
./bin/macOs/nelson.sh
```

[Previous (Building)](BUILDING.md)
