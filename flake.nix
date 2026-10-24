# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
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
      in {
        devShells.default = with pkgs; let
          # the libraries CMake looks for, also exposed at run time through the library path below
          nelsonLibraries = [
            # GUI, graphics and help
            qt6.full
            # numerics
            mpich
            fftw
            fftwFloat
            openblasCompat
            eigen
            sundials
            highs
            qhull
            tbb_2022_0
            # data formats
            hdf5
            matio
            netcdf
            arrow-cpp
            libxml2
            libxslt
            # system libraries
            icu
            zlib
            openssl
            curl
            libgit2
            libffi
            # images
            giflib
            libtiff
            libjpeg_turbo
            libpng
            libwebp
            # audio
            portaudio
            libsndfile
            taglib
            # Python engine
            python313
          ] ++ lib.optionals isDarwin [
            llvmPackages.libcxx
            llvmPackages.openmp
            coreutils
            libiconv
          ] ++ lib.optionals (!isDarwin) [
            alsa-oss
            alsa-lib
            libjack2
            # WebUI loads GTK 3 and WebKitGTK at run time for the native WebView window
            gtk3
            webkitgtk_4_1
          ];
        in mkShell {
          name = "Default development shell";

          packages = [
            nixpkgs-fmt
            cmake
            pkg-config
            gnumake
            ninja
            just
            git
            rustc
            cargo
            # the CMake build compiles the web frontends with npm, the CI configures the Julia engine
            nodejs_22
            julia-bin
          ] ++ lib.optionals (!isDarwin) [
            xvfb-run
            gdb
          ]++ lib.optionals (isDarwin) [
            lldb
          ];

          nativeBuildInputs = [
            cmake
            pkg-config
            qt6.qtbase
            qt6.wrapQtAppsHook
            makeWrapper
          ];

          buildInputs = nelsonLibraries ++ [
            python313Packages.numpy
          ];

          # the libraries Nelson loads by name at run time (fftw, python, ...)
          # macOS uses the fallback path: DYLD_LIBRARY_PATH would shadow the libc++ of the Nix binaries (cargo dies with SIGBUS)
          shellHook = (if isDarwin then ''
            export DYLD_FALLBACK_LIBRARY_PATH="${
              pkgs.lib.makeLibraryPath nelsonLibraries
            }:/usr/local/lib:/usr/lib"
          '' else ''
            export LD_LIBRARY_PATH="${
              pkgs.lib.makeLibraryPath (nelsonLibraries ++ [ pipewire.jack ])
            }:$LD_LIBRARY_PATH"
          '') + ''
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
      });
}
# ==============================================================================
