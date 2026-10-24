# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nflow_blocks)
set(module_library_name nlsNflow_blocks)
# ==============================================================================
nelson_module_sources_from_glob(
  NFLOW_BLOCKS_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/math
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/source
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/continuous
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/discrete
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/logic
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/lookup
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/nonlinear
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/routing
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/matrix
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/sink
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/userdefined
)

add_library(${module_library_name} ${NFLOW_BLOCKS_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
)

target_link_libraries(${module_library_name} PRIVATE nlsCommons)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_module_data(${module_name} EXTRA_DIRS libraries examples)
