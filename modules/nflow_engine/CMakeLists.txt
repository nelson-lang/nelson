# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nflow_engine)
set(module_library_name nlsNflow_engine)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  NFLOW_ENGINE_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${NFLOW_ENGINE_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_blocks/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsNflow_blocks
    nlsCommons
    nlsError_manager
    nlsI18n
    nlsNelson_manager
    nlsOs_functions
)
# ==============================================================================
# Builtin library (headless code-generation gateway: nflow_codegenerate)
# ==============================================================================
nelson_module_sources_from_glob(
  NFLOW_ENGINE_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${NFLOW_ENGINE_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_blocks/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../ode_solvers/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsNflow_blocks
    nlsOde_solvers
    nlsCommons
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsModules_manager
    nlsNelson_manager
    nlsTypes
    nlsCharacters_encoding
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.nflow"
  "*.csv"
)
