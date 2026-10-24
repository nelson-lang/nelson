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
# The WebAssembly build has neither library: webREST is served by the browser
# network stack (src/cpp/WebRESTWasm.cpp), repo() is unavailable.
if(NELSON_WASM_BUILD)
  set(NELSON_WEBTOOLS_WASM ON)
else()
  set(NELSON_WEBTOOLS_WASM OFF)
endif()
if(NOT WITH_LIBCURL AND NOT WITH_LIBGIT2 AND NOT NELSON_WEBTOOLS_WASM)
  return()
endif()

# Support both legacy and singular Findlibgit2 variable names.
set(_nelson_libgit2_include_dirs
  ${LIBGIT2_INCLUDE_DIRS}
  ${LIBGIT2_INCLUDE_DIR}
)
list(REMOVE_DUPLICATES _nelson_libgit2_include_dirs)

set(_nelson_libgit2_link_libs
  ${LIBGIT2_LIBRARIES}
  ${LIBGIT2_LIBRARY}
)
list(REMOVE_DUPLICATES _nelson_libgit2_link_libs)
# ==============================================================================
file(GLOB WEBTOOLS_SRC
  "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/c/*.c"
)
if(NOT WITH_LIBCURL)
  list(FILTER WEBTOOLS_SRC
    EXCLUDE
    REGEX "src/cpp/WebREST\\.cpp"
  )
endif()
if(NOT WITH_LIBCURL AND NOT NELSON_WEBTOOLS_WASM)
  list(FILTER WEBTOOLS_SRC
    EXCLUDE
    REGEX "src/cpp/WebOptions\\.cpp"
  )
  list(FILTER WEBTOOLS_SRC
    EXCLUDE
    REGEX "src/cpp/WebParameters\\.cpp"
  )
endif()
if(NOT NELSON_WEBTOOLS_WASM)
  list(FILTER WEBTOOLS_SRC
    EXCLUDE
    REGEX "src/cpp/WebRESTWasm\\.cpp"
  )
endif()
if(NOT WITH_LIBGIT2)
  list(FILTER WEBTOOLS_SRC
    EXCLUDE
    REGEX "src/cpp/Repository[A-Z].*\\.cpp"
  )
endif()
if(NOT WITH_LIBGIT2 AND NOT WITH_LIBCURL AND NOT NELSON_WEBTOOLS_WASM)
  list(FILTER WEBTOOLS_SRC
    EXCLUDE
    REGEX "src/cpp/ResponseCodeToMessage\\.cpp"
  )
endif()
list(FILTER WEBTOOLS_SRC
  EXCLUDE
  REGEX "dllMain\\.cpp"
)
# ==============================================================================
add_library(${module_library_name} ${WEBTOOLS_SRC})
if(NELSON_WEBTOOLS_WASM)
  target_compile_definitions(${module_library_name}
    PRIVATE NELSON_WEBTOOLS_WASM=1
  )
endif()

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    $<$<BOOL:${WITH_LIBCURL}>:${CURL_INCLUDE_DIRS}>
    $<$<BOOL:${WITH_LIBGIT2}>:${_nelson_libgit2_include_dirs}>
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
    ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsFiles_folders_functions
    nlsI18n
    nlsInterpreter
    nlsNelson_manager
    nlsTypes
    $<$<BOOL:${WITH_LIBCURL}>:${CURL_LIBRARIES}>
    $<$<BOOL:${WITH_LIBGIT2}>:${_nelson_libgit2_link_libs}>
)
# ==============================================================================
# Builtin library
# ==============================================================================
file(GLOB WEBTOOLS_BUILTIN
  "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp"
)
if(NOT WITH_LIBCURL AND NOT NELSON_WEBTOOLS_WASM)
  list(FILTER WEBTOOLS_BUILTIN
    EXCLUDE
    REGEX "builtin/cpp/webRESTBuiltin\\.cpp"
  )
endif()
if(NOT WITH_LIBGIT2)
  list(FILTER WEBTOOLS_BUILTIN
    EXCLUDE
    REGEX "builtin/cpp/repoBuiltin\\.cpp"
  )
endif()
list(FILTER WEBTOOLS_BUILTIN
  EXCLUDE
  REGEX "dllMain\\.cpp"
)

add_library(${module_library_builtin_name} ${WEBTOOLS_BUILTIN})
if(NELSON_WEBTOOLS_WASM)
  target_compile_definitions(${module_library_builtin_name}
    PRIVATE NELSON_WEBTOOLS_WASM=1
  )
endif()

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.json"
  EXTRA_DIR_PATTERNS
  "functions/@weboptions:*.m"
  "examples:*.json"
  "examples:*.m"
  "examples/ollama:*.m"
  "examples/ollama:*.md"
)
