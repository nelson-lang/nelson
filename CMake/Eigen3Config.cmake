# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  get_filename_component(
    MYEIGEN ${CMAKE_SOURCE_DIR}/../nelson-thirdparty-linux${BITNESS}/Eigen
    ABSOLUTE)
endif()
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  get_filename_component(
    MYEIGEN ${CMAKE_SOURCE_DIR}/../nelson-thirdparty-macosx/Eigen ABSOLUTE)
endif()
if(EXISTS ${MYEIGEN})
  set(EIGEN3_INCLUDE_DIR ${MYEIGEN})
else()
  find_package(Eigen3 3.3 REQUIRED)
endif()
# ==============================================================================
