# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_MAJOR})
  set(Nelson_VERSION_MAJOR $ENV{NELSON_VERSION_MAJOR})
else()
  set(Nelson_VERSION_MAJOR ${Nelson_VERSION_MAJOR_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{Nelson_VERSION_MINOR})
  set(Nelson_VERSION_MINOR $ENV{Nelson_VERSION_MINOR})
else()
  set(Nelson_VERSION_MINOR ${Nelson_VERSION_MINOR_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_MAINTENANCE})
  set(Nelson_VERSION_MAINTENANCE $ENV{NELSON_VERSION_MAINTENANCE})
else()
  set(Nelson_VERSION_MAINTENANCE ${Nelson_VERSION_MAINTENANCE_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_BUILD})
  set(Nelson_VERSION_BUILD $ENV{NELSON_VERSION_BUILD})
else()
  set(Nelson_VERSION_BUILD ${Nelson_VERSION_BUILD_DEFAULT})
endif()
# ==============================================================================
if(EXISTS ${CMAKE_SOURCE_DIR}/modules/commons/src/include/Nelson_VERSION.h)
else()
  configure_file(
    "${CMAKE_SOURCE_DIR}/modules/commons/src/include/Nelson_VERSION.h.in"
    "${CMAKE_SOURCE_DIR}/modules/commons/src/include/Nelson_VERSION.h")
endif()
# ==============================================================================
