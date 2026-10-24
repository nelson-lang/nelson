# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nelson_manager)
set(module_library_name nlsNelson_manager)
# ==============================================================================
nelson_module_sources_from_glob(
  NELSON_MANAGER_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${NELSON_MANAGER_SRC})

file(GLOB NELSON_MANAGER_PUBLIC_HEADER
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.h"
)
set_target_properties(${module_library_name}
  PROPERTIES PUBLIC_HEADER "${NELSON_MANAGER_PUBLIC_HEADER}"
)

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
)

target_link_libraries(${module_library_name} PRIVATE nlsCharacters_encoding)
# ==============================================================================
nelson_install_library(${module_library_name})
