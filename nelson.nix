{
  pkgs,
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
  qt5, # Qt for GUI support
}:

pkgs.stdenv.mkDerivation {
  buildInputs = [
    pkgs.zlib # Compression library
    pkgs.boost # Boost libraries
    pkgs.openssl # SSL support
    pkgs.mpi # MPI support
    pkgs.hdf5 # HDF5 support
    pkgs.curl # cURL support
    pkgs.libgit2 # Git support
    pkgs.libsndfile # Sound file support
    pkgs.eigen # Eigen library
    pkgs.portaudio # PortAudio support
    pkgs.lapack # LAPACK support
    pkgs.openblas # OpenBLAS support
    pkgs.libjack2 # JACK support
    pkgs.taglib # TagLib support
    pkgs.alsa-oss # ALSA support
    pkgs.alsa-lib # ALSA support
    pkgs.matio # MATIO support
    pkgs.libcxx # C++ standard library
    pkgs.qt5.full # Qt for GUI support
  ];

  nativeBuildInputs = [
    pkgs.cmake # Build system
    pkgs.gfortran # Fortran compiler
    pkgs.pkg-config # pkg-config for detecting dependencies
    pkgs.qt5.wrapQtAppsHook # Wrapper for Qt applications
  ];

  name = "nelson";
  version = "master";

  # Source URL for the Nelson source code
  src = pkgs.fetchFromGitHub {
    owner = "nelson-lang";
    repo = "nelson";
    rev = "master";
    sha256 = "sha256-b7L/ahq39eYDvFLW2BS9kYjS2eR2c+yPsMp0mBBJqLU="; # Replace with the actual checksum
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
    license = pkgs.lib.licenses.lgpl3Plus;
    maintainers = with pkgs.lib.maintainers; [ ];
    mainProgram = "nelson";
  };

}
