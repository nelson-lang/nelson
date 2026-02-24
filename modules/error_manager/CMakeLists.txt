# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name error_manager)
set(module_library_name nlsError_manager)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  ERROR_MANAGER_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)

add_library(${module_library_name} ${ERROR_MANAGER_SRC})

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../debugger/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include)

target_link_libraries(
  ${module_library_name}
  PRIVATE nlsCharacters_encoding
          nlsI18n
          nlsNelson_manager
          nlsCommons
          nlsDebugger)

file(
  GLOB
  ERROR_MANAGER_HEADER
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${ERROR_MANAGER_HEADER}")
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  ERROR_MANAGER_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

add_library(${module_library_builtin_name} ${ERROR_MANAGER_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../debugger/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
          nlsInterpreter
          nlsDebugger
          nlsTypes
          nlsCharacters_encoding
          nlsI18n)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# ==============================================================================
