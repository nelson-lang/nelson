# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(ARPACK_FOUND FALSE)
set(ARPACK_LIBRARIES)
set(ARPACK_RUNTIME_LIBRARY)

if(WITHOUT_ARPACK)
  message(STATUS "ARPACK support disabled.")
  return()
endif()

if(WIN32)
  if(NELSON_WINDOWS_PLATFORM_NAME STREQUAL "ARM64")
    message(STATUS "ARPACK not available on Windows ARM64.")
    return()
  endif()

  set(_nelson_arpack_root "${NELSON_WINDOWS_THIRDPARTY_ROOT}/arpack-ng")
  find_library(ARPACK_LIBRARY
    NAMES
    libnlsarpack-ng
    libarpack
    PATHS
    "${_nelson_arpack_root}/lib"
    NO_DEFAULT_PATH
  )
  find_file(ARPACK_RUNTIME_LIBRARY
    NAMES
    libnlsarpack-ng.dll
    libarpack.dll
    PATHS
    "${_nelson_arpack_root}/bin"
    NO_DEFAULT_PATH
  )
else()
  find_library(ARPACK_LIBRARY
    NAMES
    arpack
    arpack-ng
    libarpack
    libarpack-ng
  )
endif()

if(ARPACK_LIBRARY)
  set(ARPACK_FOUND TRUE)
  set(ARPACK_LIBRARIES "${ARPACK_LIBRARY}")
  if(WIN32 AND ARPACK_RUNTIME_LIBRARY)
    add_library(ARPACK::ARPACK SHARED IMPORTED)
    set_target_properties(ARPACK::ARPACK
      PROPERTIES
        IMPORTED_IMPLIB "${ARPACK_LIBRARY}"
        IMPORTED_LOCATION "${ARPACK_RUNTIME_LIBRARY}"
    )
  else()
    add_library(ARPACK::ARPACK UNKNOWN IMPORTED)
    set_target_properties(ARPACK::ARPACK
      PROPERTIES IMPORTED_LOCATION "${ARPACK_LIBRARY}"
    )
  endif()
  message(STATUS "Found ARPACK library: ${ARPACK_LIBRARY}")
  if(ARPACK_RUNTIME_LIBRARY)
    message(STATUS "Found ARPACK runtime: ${ARPACK_RUNTIME_LIBRARY}")
  endif()
else()
  message(STATUS "ARPACK library not found.")
endif()
