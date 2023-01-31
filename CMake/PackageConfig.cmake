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
            "https://nelson-numerical-software.github.io/nelson-website/")
        set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
        set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
        set(CPACK_DEBIAN_PACKAGE_DEPENDS "fftw3")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS},  libslicot0")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS},  libopenblas0-openmp")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
            "${CPACK_DEBIAN_PACKAGE_DEPENDS},  hdf5-tools")
        set(CPACK_DEBIAN_PACKAGE_RECOMMENDS "cmake")
      endif(DEBIAN_FOUND)
    endif()
  endif(APPLE)
else(UNIX)
  set(CPACK_GENERATOR "ZIP")
endif(UNIX)
# ==============================================================================
include(CPack)
# ==============================================================================
