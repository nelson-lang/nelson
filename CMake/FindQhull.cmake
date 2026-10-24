# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include(FindPackageHandleStandardArgs)
# ==============================================================================
set(_Qhull_HINTS)
if(DEFINED Qhull_ROOT)
  list(APPEND _Qhull_HINTS "${Qhull_ROOT}")
endif()
if(DEFINED NELSON_WINDOWS_THIRDPARTY_ROOT)
  list(APPEND _Qhull_HINTS "${NELSON_WINDOWS_THIRDPARTY_ROOT}/qhull")
endif()
if(DEFINED _THIRDPARTY_ROOT)
  list(APPEND _Qhull_HINTS "${_THIRDPARTY_ROOT}/qhull")
endif()
# ==============================================================================
find_path(Qhull_INCLUDE_DIR
  NAMES
  libqhull_r/libqhull_r.h
  libqhullcpp/Qhull.h
  HINTS
  ${_Qhull_HINTS}
  PATH_SUFFIXES
  include
)

find_library(Qhull_R_LIBRARY
  NAMES
  qhull_r
  libqhull_r
  HINTS
  ${_Qhull_HINTS}
  PATH_SUFFIXES
  lib
  lib64
)

find_library(Qhull_CPP_LIBRARY
  NAMES
  qhullcpp
  libqhullcpp
  HINTS
  ${_Qhull_HINTS}
  PATH_SUFFIXES
  lib
  lib64
)

find_file(Qhull_RUNTIME_LIBRARY
  NAMES
  qhull_r.dll
  libqhull_r.dll
  HINTS
  ${_Qhull_HINTS}
  PATH_SUFFIXES
  bin
)

find_package_handle_standard_args(
  Qhull
  REQUIRED_VARS
  Qhull_INCLUDE_DIR
  Qhull_R_LIBRARY
)

if(Qhull_FOUND)
  set(Qhull_INCLUDE_DIRS "${Qhull_INCLUDE_DIR}")
  set(Qhull_LIBRARIES "${Qhull_R_LIBRARY}")
  if(Qhull_CPP_LIBRARY)
    list(PREPEND Qhull_LIBRARIES "${Qhull_CPP_LIBRARY}")
  endif()

  if(NOT TARGET Qhull::qhull_r)
    add_library(Qhull::qhull_r UNKNOWN IMPORTED)
    set_target_properties(Qhull::qhull_r
      PROPERTIES
        IMPORTED_LOCATION "${Qhull_R_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${Qhull_INCLUDE_DIR}"
    )
    if(WIN32 AND Qhull_RUNTIME_LIBRARY)
      set_target_properties(Qhull::qhull_r
        PROPERTIES IMPORTED_IMPLIB "${Qhull_R_LIBRARY}"
      )
    endif()
  endif()

  if(Qhull_CPP_LIBRARY AND NOT TARGET Qhull::qhullcpp)
    add_library(Qhull::qhullcpp UNKNOWN IMPORTED)
    set_target_properties(Qhull::qhullcpp
      PROPERTIES
        IMPORTED_LOCATION "${Qhull_CPP_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${Qhull_INCLUDE_DIR}"
        INTERFACE_LINK_LIBRARIES Qhull::qhull_r
    )
  endif()
endif()
