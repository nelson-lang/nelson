# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name mex)
set(module_library_name nlsMex)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WITHOUT_MEX_MODULE)
  set(_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/no-src")
else()
  set(_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/src")
endif()
nelson_module_sources_from_glob(
  MEX_SRC
  DIRS
  ${_src_dir}/cpp)

add_library(${module_library_name} ${MEX_SRC})

if(WITHOUT_MEX_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${_src_dir}/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
else()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${_src_dir}/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${EIGEN3_INCLUDE_DIR}
            ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../ipc/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../sparse/src/include)
  target_link_libraries(
    ${module_library_name}
    PRIVATE nlsCharacters_encoding
            nlsError_manager
            nlsNelson_manager
            nlsI18n
            nlsInterpreter
            nlsIpc
            nlsSparse
            nlsStream_manager
            nlsTypes
            nlsOs_functions)
endif()

file(
  GLOB
  MEX_HEADER
  "${_src_dir}/include/*.hpp"
  "${_src_dir}/include/*.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${MEX_HEADER}")
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
if(NOT WITHOUT_MEX_MODULE)
  nelson_module_sources_from_glob(
    MEX_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

  add_library(${module_library_builtin_name} ${MEX_BUILTIN})

  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../validators/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../mex/src/include)

  target_link_libraries(
    ${module_library_builtin_name}
    PRIVATE ${module_library_name}
            nlsError_manager
            nlsI18n
            nlsInterpreter
            nlsTypes
            nlsValidators)

  nelson_install_library(${module_library_builtin_name})
  nelson_install_module_data(
    ${module_name}
    EXTRA_TEST_PATTERNS
    "*.c")

  # Install examples
  include(GNUInstallDirs)
  set(_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/examples
    DESTINATION "${_dest}"
    FILES_MATCHING
    PATTERN "*.c")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/examples
    DESTINATION "${_dest}"
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/examples/rust-mex-builder
    DESTINATION "${_dest}/examples"
    FILES_MATCHING
    PATTERN "*.md")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/examples/rust-mex-builder/helloworld
    DESTINATION "${_dest}/examples/rust-mex-builder"
    FILES_MATCHING
    PATTERN "*.*")
endif()
# ==============================================================================
