### Linux

- You can also see [ccpp.yml](https://github.com/Nelson-numerical-software/nelson/blob/master/.github/workflows/ccpp.yml) file to help you to see dependencies. This file is up-to-date about how to build Nelson on each platform.

CMake options:

```
-DLGPL_ONLY=ON LGP only (disable fftw and slicot modules)
-DWITHOUT_MEX_MODULE=ON disable mex module
-DWITHOUT_FFTW_MODULE=ON disable fftw module
-DWITHOUT_SLICOT_MODULE=ON disable slicot module

```

Standard build:

```bash
cd nelson
cmake -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" .
cmake --build . -- -j $(nproc)
cmake --build . -- buildhelp
cmake --build . -- tests_minimal
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
