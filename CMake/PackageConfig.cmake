# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VENDOR "Allan CORNET")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "Nelson is an- matrix/array programming language providing a powerful open computing environment for engineering and scientific applications using modern C/C++ libraries (Boost, Eigen, …) and others state of art numerical libraries."
)
set(CPACK_PACKAGE_VERSION_MAJOR ${Nelson_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${Nelson_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${Nelson_VERSION_MAINTENANCE})
set(CPACK_PACKAGE_VERSION
    "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}.${Nelson_VERSION_BUILD}"
)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Nelson-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_CONTACT "nelson.numerical.computation@gmail.com")

if(UNIX)
  set(CPACK_GENERATOR "TGZ")
  if(APPLE)
    # later
  else(APPLE)
    find_file(DEBIAN_FOUND debian_version debconf.conf PATHS /etc)
    find_file(FEDORA_FOUND fedora-release PATHS /etc)
    find_file(REDHAT_FOUND redhat-release inittab.RH PATHS /etc)

    if(NOT ${CMAKE_VERSION} VERSION_LESS "3.20.0")
      if(DEBIAN_FOUND)
        set(CPACK_GENERATOR "DEB")
        set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Allan CORNET")
        set(CPACK_DEBIAN_PACKAGE_HOMEPAGE
            "https://nelson-lang.github.io/nelson-website/")
        set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
        # set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
        set(CPACK_DEBIAN_PACKAGE_RECOMMENDS "cmake")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS "fftw3")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libslicot0")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenblas0-openmp")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, hdf5-tools")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libasound2 (>= 1.0.16)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libboost-chrono1.74.0 (>= 1.74.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libboost-filesystem1.74.0 (>= 1.74.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libboost-iostreams1.74.0 (>= 1.74.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libboost-locale1.74.0 (>= 1.74.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libboost-serialization1.74.0 (>= 1.74.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libboost-thread1.74.0 (>= 1.74.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libc6 (>= 2.35)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libcurl4 (>= 7.16.2)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libffi8 (>= 3.4)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgcc-s1 (>= 4.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgit2-1.1 (>= 1.1.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgomp1 (>= 4.9)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libhdf5-103-1")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libicu70 (>= 70.1-1~)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libjack-jackd2-0 (>= 1.9.10+20150825) | libjack-0.125"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, liblapacke (>= 3.10.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libmatio11 (>= 1.5.15)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenblas0")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenmpi3 (>= 4.1.2)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libportaudio2 (>= 19+svn20101113)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6core6 (>= 6.2.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6gui6 (>= 6.1.2)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6help6 (>= 6.2.2)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6printsupport6 (>= 6.1.2)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6qml6 (>= 6.2.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6quick6 (>= 6.2.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6svg6 (>= 6.2.0)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6widgets6 (>= 6.1.2)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsndfile1 (>= 1.0.20)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libstdc++6 (>= 12)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtag1v5 (>= 1.9.1-2.2~)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libxml2 (>= 2.7.4)")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, zlib1g (>= 1:1.1.4)")

        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qt6-declarative-dev")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qt6-documentation-tools")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-templates")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-controls")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-window")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-dialogs")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtqml-workerscript")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-layouts")

      endif(DEBIAN_FOUND)
    endif()
  endif(APPLE)
else(UNIX)
  set(CPACK_GENERATOR "ZIP")
endif(UNIX)
# ==============================================================================
include(CPack)
# ==============================================================================
