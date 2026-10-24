# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nflow_fmi)
set(module_library_name nlsNflow_fmi)
set(module_library_builtin_name nlsNflow_fmi_builtin)
# ==============================================================================
# Native builds use vendored fmi4c and the backend-neutral FMI wrapper.
# Wasm builds use the Worker component registry without a dynamic loader.
# The native fmi4c path only loads extracted archives (USE_SYSTEM_ZIP).
# ==============================================================================
if(NELSON_WASM_BUILD)
  nelson_module_sources_from_glob(
    NFLOW_FMI_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/wasm
  )
  add_library(${module_library_name} ${NFLOW_FMI_SRC})
  target_link_libraries(${module_library_name} PRIVATE nlsNflow_blocks)
  target_include_directories(${module_library_name}
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_blocks/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
  )
else()
  nelson_module_sources_from_glob(
    NFLOW_FMI_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  )
  set(FMI4C_VENDORED_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/src/fmi4c/src/fmi4c.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/fmi4c/src/fmi4c_utils.c
    ${CMAKE_CURRENT_SOURCE_DIR}/src/fmi4c/3rdparty/ezxml/ezxml.c
  )
  add_library(${module_library_name} ${NFLOW_FMI_SRC} ${FMI4C_VENDORED_SRC})
endif()
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/fmi4c/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src/fmi4c/3rdparty
    ${CMAKE_CURRENT_SOURCE_DIR}/../file_archiver/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
)
if(NOT NELSON_WASM_BUILD)
  target_compile_definitions(${module_library_name}
    PRIVATE
      FMI4C_STATIC
      EZXML_NOMMAP
      _CRT_SECURE_NO_WARNINGS
      _CRT_NONSTDC_NO_DEPRECATE
  )
endif()
# nflow.fmi3.inspect extracts a .fmu with the ZIP-slip-hardened UnZip.
target_link_libraries(${module_library_name} PRIVATE nlsFile_archiver)
if(NOT WIN32)
  # fmi4c dlopen()s the FMU binary at instantiation time.
  target_link_libraries(${module_library_name} PRIVATE ${CMAKE_DL_LIBS})
endif()
nelson_install_library(${module_library_name})

# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  NFLOW_FMI_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

if(NELSON_WASM_BUILD)
  list(FILTER NFLOW_FMI_BUILTIN EXCLUDE REGEX "Fmu(Me)?ComponentBridge[.]cpp$")
endif()

add_library(${module_library_builtin_name} ${NFLOW_FMI_BUILTIN})
target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_blocks/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../file_archiver/src/include
)
target_link_libraries(${module_library_builtin_name}
  PRIVATE
    nlsError_manager
    nlsNelson_manager
    nlsI18n
    nlsCommons
    nlsTypes
    nlsInterpreter
    nlsCharacters_encoding
    nlsNflow_fmi
    nlsNflow_blocks
    nlsFile_archiver
)
nelson_install_library(${module_library_builtin_name})
# ==============================================================================
nelson_install_module_data(
  ${module_name}
  EXTRA_DIRS
  examples
  EXTRA_DIR_PATTERNS
  "resources/fmi3:*.h,*.md"
)
