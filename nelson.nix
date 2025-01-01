{
  stdenv, # Standard environment providing basic build tools and utilities
  lib,  # Nix standard library with helper functions and utilities
  fetchFromGitHub,  # Function to fetch source code from GitHub repositories
  cmake, # Build system generator
  pkg-config, # Package configuration tool
  zlib, # Compression library
  boost, # Boost libraries
  openssl, # SSL support
  mpi, # MPI support
  hdf5, # HDF5 support
  curl, # cURL support
  libgit2, # Git support
  libsndfile, # Sound file support
  eigen, # Eigen library
  portaudio, # PortAudio support
  lapack, # LAPACK support
  openblas, # OpenBLAS support
  libjack2, # JACK support
  taglib, # TagLib support
  alsa-oss, # ALSA support
  alsa-lib, # ALSA support
  matio, # MATIO support
  libcxx, # C++ standard library
  qt6, # Qt for GUI support
}:

stdenv.mkDerivation {
  buildInputs = [
    zlib # Compression library
    boost # Boost libraries
    openssl # SSL support
    mpi # MPI support
    hdf5 # HDF5 support
    curl # cURL support
    libgit2 # Git support
    libsndfile # Sound file support
    eigen # Eigen library
    portaudio # PortAudio support
    lapack # LAPACK support
    openblas # OpenBLAS support
    libjack2 # JACK support
    taglib # TagLib support
    alsa-oss # ALSA support
    alsa-lib # ALSA support
    matio # MATIO support
    libcxx # C++ standard library
    qt6.full # Qt for GUI support
    qt6.qtbase # Qt base required
  ];

  nativeBuildInputs = [
    cmake # Build system
    pkg-config # pkg-config for detecting dependencies
    qt6.wrapQtAppsHook # Wrapper for Qt applications
  ];

  name = "nelson";
  version = "master";

  # Source URL for the Nelson source code
  src = fetchFromGitHub {
    owner = "nelson-lang";
    repo = "nelson";
    rev = "master";
    sha256 = "sha256-9UYEL57yJeRR1lLD+o3R9cPKYLTqcPxwIAv8pf3Rf6Y="; # Replace with the actual checksum
  };

  cmakeFlags = [
    "-DLGPL_ONLY=ON"
  ];

  # Wrap the application with Qt environment variables
  postInstall = ''
    wrapQtApp $out/bin/nelson
  '';

  doCheck = false;

  # Enable parallel building
  enableParallelBuilding = true;

  meta = {
    description = "Nelson programming language";
    homepage = "https://github.com/nelson-lang/nelson";
    license = lib.licenses.lgpl3Plus;
    maintainers = with lib.maintainers; [ ];
    mainProgram = "nelson";
  };

}