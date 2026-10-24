# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name dynamic_link)
set(module_library_name nlsDynamic_link)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  DYNAMIC_LINK_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${DYNAMIC_LINK_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../handle/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
    ${LIBFFI_INCLUDE_DIR}
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsFiles_folders_functions
    nlsHandle
    nlsI18n
    nlsOs_functions
    nlsTypes
    ${LIBFFI_LIBRARY}
)
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  DYNAMIC_LINK_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${DYNAMIC_LINK_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../display_format/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../handle/src/include
    ${LIBFFI_INCLUDE_DIR}
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsCommons
    nlsDisplay_format
    nlsError_manager
    nlsHandle
    nlsI18n
    nlsInterpreter
    nlsTypes
    nlsCharacters_encoding
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.c"
  "*.cpp"
  "*.h"
  EXTRA_DIRS
  examples
)

include(GNUInstallDirs)
set(_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/resources
  DESTINATION "${_dest}"
  FILES_MATCHING PATTERN "*.txt"
)
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/resources
  DESTINATION "${_dest}"
  FILES_MATCHING PATTERN "*.json"
)
# ==============================================================================
# Nelson C++ SDK headers.  Parity with the Windows installer, where each SDK
# module's .iss ships src\include under the DynamicLink component.  On macOS the
# per-module CMake install does not ship headers, so dlgetnelsonincludes() found
# no <module>/src/include in the installed tree, returned empty, and
# test_dlgetnelsonincludes (and any builtin-gateway build) failed.  Ship just
# the SDK module set (~3 MB) to the same layout the function resolves:
# modules/<module>/src/include.
# ==============================================================================
set(_nelson_sdk_modules
  interpreter
  types
  error_manager
  i18n
  characters_encoding
  stream_manager
  overload
  commons
  nelson_manager
  display_format
  handle
  f2c
  validators
)
foreach(_sdk_module IN LISTS _nelson_sdk_modules)
  if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_sdk_module}/src/include")
    install(
      DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_sdk_module}/src/include
      DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${_sdk_module}/src"
      FILES_MATCHING
        PATTERN "*.h"
        PATTERN "*.hpp"
    )
  endif()
endforeach()
