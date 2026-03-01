# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name webtools)
set(module_library_name nlsWebtools)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(NOT WITH_LIBCURL AND NOT WITH_LIBGIT2)
  return()
endif()
# ==============================================================================
file(
  GLOB
  WEBTOOLS_SRC
  "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/*.c")
if(NOT WITH_LIBCURL)
  list(
    FILTER
    WEBTOOLS_SRC
    EXCLUDE
    REGEX
    "src/cpp/WebOptions\\.cpp")
  list(
    FILTER
    WEBTOOLS_SRC
    EXCLUDE
    REGEX
    "src/cpp/WebREST\\.cpp")
endif()
if(NOT WITH_LIBGIT2)
  list(
    FILTER
    WEBTOOLS_SRC
    EXCLUDE
    REGEX
    "src/cpp/ProcessEventsDynamic\\.cpp")
  list(
    FILTER
    WEBTOOLS_SRC
    EXCLUDE
    REGEX
    "src/cpp/Repository[A-Z].*\\.cpp")
  list(
    FILTER
    WEBTOOLS_SRC
    EXCLUDE
    REGEX
    "src/cpp/ResponseCodeToMessage\\.cpp")
endif()
list(
  FILTER
  WEBTOOLS_SRC
  EXCLUDE
  REGEX
  "dllMain\\.cpp")
# ==============================================================================
add_library(${module_library_name} ${WEBTOOLS_SRC})

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE $<$<BOOL:${WITH_LIBCURL}>:${CURL_INCLUDE_DIRS}>
          $<$<BOOL:${WITH_LIBGIT2}>:${LIBGIT2_INCLUDE_DIRS}>
          ${CMAKE_CURRENT_SOURCE_DIR}/src/c
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include)

target_link_libraries(
  ${module_library_name}
  PRIVATE nlsCharacters_encoding
          nlsCommons
          nlsError_manager
          nlsFiles_folders_functions
          nlsI18n
          nlsNelson_manager
          nlsTypes
          $<$<BOOL:${WITH_LIBCURL}>:${CURL_LIBRARIES}>
          $<$<BOOL:${WITH_LIBGIT2}>:${LIBGIT2_LIBRARIES}>)
# ==============================================================================
# Builtin library
# ==============================================================================
file(
  GLOB
  WEBTOOLS_BUILTIN
  "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
if(NOT WITH_LIBCURL)
  list(
    FILTER
    WEBTOOLS_BUILTIN
    EXCLUDE
    REGEX
    "builtin/cpp/webRESTBuiltin\\.cpp")
endif()
if(NOT WITH_LIBGIT2)
  list(
    FILTER
    WEBTOOLS_BUILTIN
    EXCLUDE
    REGEX
    "builtin/cpp/repoBuiltin\\.cpp")
endif()
list(
  FILTER
  WEBTOOLS_BUILTIN
  EXCLUDE
  REGEX
  "dllMain\\.cpp")

add_library(${module_library_builtin_name} ${WEBTOOLS_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
          nlsError_manager
          nlsI18n
          nlsInterpreter
          nlsTypes)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.json"
  EXTRA_DIR_PATTERNS
  "functions/@weboptions:*.m"
  "examples:*.m"
  "examples/ollama:*.m"
  "examples/ollama:*.md")
# ==============================================================================
