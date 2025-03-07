# Building on macOS

To build this project on macOS, follow the steps below.

## Prerequisites

1. **Install Xcode**  
   Xcode is required for compiling code on macOS.

2. **Install Homebrew**  
   Homebrew is a package manager for macOS that simplifies the installation of software.

   ```bash
   ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
   brew doctor
   ```

3. **Install Micromamba**  
   Micromamba is used to manage environments for this project. Follow the [Micromamba installation guide](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html) for detailed instructions on macOS.

   Alternatively, install it with Homebrew:

   ```bash
   brew install micromamba
   ```

## Building the Project with micromamba

1. Navigate to the root directory of the project:

   ```bash
   cd /path/to/nelson/
   ```

2. Create and activate the environment:

   ```bash
   micromamba env create -f environment-macos-dev.yml
   micromamba activate nelson
   ```

3. Create a build directory and run CMake with the following options:

   ```bash
   mkdir -p build
   cd build
   cmake .. \
       -DCMAKE_BUILD_TYPE=Release \
       -DFORCE_LIBGFORTRAN_LINK=ON \
       -DWITHOUT_SLICOT_MODULE=ON \
       -DCMAKE_PREFIX_PATH=$CONDA_PREFIX \
       -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX \
       -DCMAKE_INSTALL_LIBDIR=lib
   ```

4. Build the project:

   ```bash
   cmake --build .
   cmake --build . --target buildhelp
   cmake --build . --target install
   ```

5. Run the application:

   ```bash
   nelson
   ```

## Additional Information

- See [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) for an up-to-date list of dependencies and build instructions across platforms.
- Refer to [Building with Linux](BUILDING_Linux.md) for further information on compilation options.
- By default on Apple Silicon, build use native optimized cpu instructions set.
  [Previous: Building](BUILDING.md)
