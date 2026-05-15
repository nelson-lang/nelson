# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name string)
set(module_library_name nlsString)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WIN32)
  set(_NELSON_PCRE_ROOT "${CMAKE_SOURCE_DIR}/../nelson-thirdparty-${_THIRDPARTY_PLATFORM}/pcre")
  if(EXISTS "${_NELSON_PCRE_ROOT}/include/pcre.h")
    set(PCRE_INCLUDE_DIR "${_NELSON_PCRE_ROOT}/include")
    set(PCRE_LIBRARY "${_NELSON_PCRE_ROOT}/lib/pcre.lib")
    # pcre16: native UTF-16 mode (wchar_t is 16-bit on Windows)
    set(PCRE_WIDE_LIBRARY "${_NELSON_PCRE_ROOT}/lib/pcre16.lib")
  endif()
  if(NOT PCRE_INCLUDE_DIR)
    find_path(PCRE_INCLUDE_DIR pcre.h)
  endif()
  if(NOT PCRE_LIBRARY)
    find_library(PCRE_LIBRARY NAMES pcre)
  endif()
  if(NOT PCRE_WIDE_LIBRARY)
    find_library(PCRE_WIDE_LIBRARY NAMES pcre16)
  endif()
  if(NOT PCRE_INCLUDE_DIR OR NOT PCRE_LIBRARY OR NOT PCRE_WIDE_LIBRARY)
    message(FATAL_ERROR "PCRE/pcre16 libraries are required by the string regular expression functions.")
  endif()
else()
  set(NELSON_STRING_USE_PCRE2 TRUE)
  find_path(
    PCRE_INCLUDE_DIR
    pcre2.h
    HINTS
      "$ENV{CONDA_PREFIX}/include"
      "$ENV{HOMEBREW_PREFIX}/opt/pcre2/include"
      "/opt/homebrew/opt/pcre2/include"
      "/usr/local/opt/pcre2/include"
  )
  # wchar_t is UTF-32 on Linux and macOS, so the native wide subject uses pcre2-32.
  find_library(
    PCRE_WIDE_LIBRARY
    NAMES pcre2-32
    HINTS
      "$ENV{CONDA_PREFIX}/lib"
      "$ENV{HOMEBREW_PREFIX}/opt/pcre2/lib"
      "/opt/homebrew/opt/pcre2/lib"
      "/usr/local/opt/pcre2/lib"
  )
  if(NOT PCRE_INCLUDE_DIR OR NOT PCRE_WIDE_LIBRARY)
    message(FATAL_ERROR "PCRE2 pcre2-32 library is required by the string regular expression functions.")
  endif()
endif()
# ==============================================================================
nelson_module_sources_from_glob(
  STRING_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)

add_library(${module_library_name} ${STRING_SRC})

target_compile_definitions(${module_library_name} PRIVATE NLSSTRING_EXPORTS)
if(NELSON_STRING_USE_PCRE2)
  target_compile_definitions(${module_library_name} PRIVATE NELSON_USE_PCRE2)
endif()

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${EIGEN3_INCLUDE_DIR}
          ${PCRE_INCLUDE_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../operators/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include)

target_link_libraries(
  ${module_library_name}
  PRIVATE nlsCharacters_encoding
          nlsCommons
          nlsCore
          nlsElementary_functions
          nlsElementary_mathematics
          nlsError_manager
          nlsI18n
          nlsOperators
          nlsStream_manager
          nlsNelson_manager
          nlsTypes
          ${PCRE_LIBRARY}
          ${PCRE_WIDE_LIBRARY})
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  STRING_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

add_library(${module_library_builtin_name} ${STRING_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../operators/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
          nlsCommons
          nlsError_manager
          nlsNelson_manager
          nlsI18n
          nlsInterpreter
          nlsOperators
          nlsTypes)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# ==============================================================================
