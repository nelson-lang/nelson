{
  pkgs ? import <nixpkgs> { },
}:

pkgs.stdenv.mkDerivation {
  name = "nelson";
  version = "master";

  # Source URL for the Nelson source code
  src = pkgs.fetchFromGitHub {
    owner = "nelson-lang";
    repo = "nelson";
    rev = "master";
    sha256 = "sha256-V6DERgNNv2dUal6lkbu/OtFpJoFKdMRW6HLOQJBZbWk="; # Replace with the actual checksum
  };

  buildInputs = [
    pkgs.zlib # Compression library
    pkgs.boost # Boost libraries
    pkgs.openssl # SSL support
    pkgs.mpi # MPI support
    pkgs.hdf5 # HDF5 support
    pkgs.curl # cURL support
    pkgs.libgit2 # Git support
    pkgs.libsndfile # Sound file support
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
    pkgs.qt5.wrapQtAppsHook # Wrapper for Qt applications
  ];

  nativeBuildInputs = [
    pkgs.cmake # Build system
    pkgs.gnumake # Make utility
    pkgs.gfortran # Fortran compiler (if needed for certain modules)
    pkgs.eigen # Eigen library
    pkgs.pkg-config # pkg-config for detecting dependencies
  ];

  cmakeFlags = [
    "-DLGPL_ONLY=ON"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DCMAKE_INSTALL_PREFIX=$out"
  ];

  # Wrap the application with Qt environment variables
  postInstall = ''
    wrapQtApp $out/bin/nelson
  '';

  doCheck = false;

  meta = {
    description = "Nelson programming language";
    homepage = "https://github.com/nelson-lang/nelson";
    license = pkgs.lib.licenses.lgpl3Plus;
    maintainers = with pkgs.lib.maintainers; [ ];
  };

}
