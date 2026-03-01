# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name characters_encoding)
set(module_library_name nlsCharacters_encoding)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WITHOUT_CHARACTERS_ENCODING_MODULE)
  set(_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/no-src")
  nelson_module_sources_from_glob(
    CHARACTERS_ENCODING_SRC
    DIRS
    ${_src_dir}/cpp)
else()
  set(_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/src")
  nelson_module_sources_from_glob(
    CHARACTERS_ENCODING_SRC
    DIRS
    ${_src_dir}/cpp
    ${_src_dir}/cpp/simdutf)
endif()

add_library(${module_library_name} ${CHARACTERS_ENCODING_SRC})

if(WITHOUT_CHARACTERS_ENCODING_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${_src_dir}/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
else()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${_src_dir}/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/simdutf ${ICU_INCLUDE_DIRS})
  target_link_libraries(${module_library_name} PRIVATE ${ICU_LIBRARIES})
endif()

file(
  GLOB
  CHARACTERS_ENCODING_HEADER
  "${_src_dir}/include/*.hpp"
  "${_src_dir}/include/*.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${CHARACTERS_ENCODING_HEADER}")
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
if(NOT WITHOUT_CHARACTERS_ENCODING_MODULE)
  nelson_module_sources_from_glob(
    CHARACTERS_ENCODING_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

  add_library(${module_library_builtin_name} ${CHARACTERS_ENCODING_BUILTIN})

  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include)

  target_link_libraries(
    ${module_library_builtin_name}
    PRIVATE ${module_library_name}
            nlsI18n
            nlsError_manager
            nlsInterpreter
            nlsTypes)

  nelson_install_library(${module_library_builtin_name})
  nelson_install_module_data(
    ${module_name}
    EXTRA_TEST_PATTERNS
    "*.txt")
endif()
# ==============================================================================
