{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # System override for macOS
        effectiveSystem = if builtins.match ".*-darwin" system != null then "aarch64-darwin" else system;
        pkgs = import nixpkgs { system = effectiveSystem; };
        isDarwin = pkgs.stdenv.isDarwin;
      in
      {
        devShells.default =
          with pkgs;
          mkShell {
            name = "Default development shell";
            packages = [
              nixpkgs-fmt
              cmake
              pkg-config
              gnumake
              ninja
              just
              gdb
              git
              nodejs_20 
            ] ++ lib.optionals (!isDarwin) [
              xvfb-run
            ];

            nativeBuildInputs = with pkgs; [
              cmake
              pkg-config
              qt6.qtbase
              qt6.wrapQtAppsHook
              makeWrapper
            ];

            buildInputs = with pkgs; [
              qt6.full
              mpich
              fftw
              fftwFloat
              icu
              zlib
              boost
              openssl
              hdf5
              curl
              libgit2
              libsndfile
              eigen
              portaudio
              openblasCompat
              taglib
              matio
              giflib
              libtiff
              python313
            ] ++ lib.optionals isDarwin [
              llvmPackages.libcxx
              llvmPackages.openmp
              coreutils
              libiconv
              darwin.apple_sdk.frameworks.CoreAudio
              darwin.apple_sdk.frameworks.Accelerate
            ] ++ lib.optionals (!isDarwin) [
              alsa-oss
              alsa-lib
              libjack2
            ];

            shellHook = ''

              ${if isDarwin then "export DYLD_LIBRARY_PATH=" else "export LD_LIBRARY_PATH="}"${
                pkgs.lib.makeLibraryPath (
                  with pkgs;
                  [
                    zlib
                    fftw
                    fftwFloat
                    openblasCompat
                    icu
                    boost
                    openssl
                    mpi
                    hdf5
                    curl
                    libgit2
                    libsndfile
                    portaudio
                    eigen
                    taglib
                    matio
                    qt6.full
                    giflib
                    libtiff
                    python313
                  ] ++ lib.optionals isDarwin [
                    llvmPackages.openmp
                    darwin.apple_sdk.frameworks.CoreAudio
                    darwin.apple_sdk.frameworks.Accelerate
                  ] ++ lib.optionals (!isDarwin) [
                    pipewire.jack
                  ]
                )
              }:$${if isDarwin then "DYLD_LIBRARY_PATH" else "LD_LIBRARY_PATH"}"

              if [ -f package.json ] && [ ! -d node_modules ]; then
                echo "📦 Installing npm dependencies..."
                npm install
              fi

              # Export prefix variables for specific libraries
              export NIX_LIBTIFF_PREFIX="${pkgs.libtiff}"
              export NIX_GIFLIB_PREFIX="${pkgs.giflib}"
              export LC_ALL=C
              export QT_XCB_GL_INTEGRATION=none

              
              export NIX_ENFORCE_NO_NATIVE=0
              echo ""
              echo "⭐ Welcome to the Nelson development environment ⭐"
              echo ""
            '';
          };
      }
    );
}