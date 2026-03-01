# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name elementary_mathematics)
set(module_library_name nlsElementary_mathematics)
# ==============================================================================
nelson_module_sources_from_glob(
  ELEMENTARY_MATHEMATICS_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)

add_library(${module_library_name} ${ELEMENTARY_MATHEMATICS_SRC})

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${OpenMP_CXX_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)

target_link_libraries(${module_library_name} PRIVATE ${OpenMP_CXX_LIBRARIES})
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
