# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nflow_gui)
set(module_library_name nlsNflow_gui)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
# Native WebUI/CivetWeb/WebView code is owned by modules/webview. Keep nflow_gui
# focused on NFlow-specific bindings and depend on nlsWebview for the shared
# WebUI headers and compiled implementation.
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/webui/include/webui.h)
  message(FATAL_ERROR
    "nflow_gui must use modules/webview for WebUI; remove the embedded copy."
  )
endif()
# ==============================================================================
nelson_module_sources_from_glob(
  NFLOW_GUI_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
list(FILTER NFLOW_GUI_SRC EXCLUDE REGEX "/nflow_rpc_(bindings|codegen)\\.cpp$")

add_library(${module_library_name} ${NFLOW_GUI_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${EIGEN3_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../operators/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../webview/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsElementary_functions
    nlsElementary_mathematics
    nlsError_manager
    nlsI18n
    nlsModules_manager
    nlsOperators
    nlsStream_manager
    nlsNelson_manager
    nlsTypes
    nlsWebview
)
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  NFLOW_GUI_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)
list(APPEND NFLOW_GUI_BUILTIN
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/nflow_rpc_bindings.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/nflow_rpc_codegen.cpp
)

# The engine sources now live in nlsNflow_engine and the block sources in
# nlsNflow_blocks; the builtin links both instead of recompiling them.
add_library(${module_library_builtin_name} ${NFLOW_GUI_BUILTIN})

# nflow_rpc_bindings.cpp pulls <windows.h> transitively; without NOMINMAX its
# min/max macros clobber std::min/std::max. Define it before any include.
if(WIN32)
  target_compile_definitions(${module_library_name} PRIVATE NOMINMAX)
  target_compile_definitions(${module_library_builtin_name} PRIVATE NOMINMAX)
endif()

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_blocks/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_engine/builtin/include
    # Headers only (GuiDialogProvider.hpp for the system file dialog seam). nlsGui
    # is resolved dynamically at runtime, never linked, so nflow_gui stays Qt-free.
    ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../operators/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsNflow_engine
    nlsNflow_engine_builtin
    nlsNflow_blocks
    nlsNflow_fmi
    nlsWebview
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsNelson_manager
    nlsI18n
    nlsInterpreter
    nlsOperators
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
# ------------------------------------------------------------------------------
# TypeScript build step: if a package.json exists in this module, install
# locked npm dependencies and run the standard frontend build script.
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/package.json)
  nelson_add_frontend_build(
    ${module_library_name}
    nflow_gui_ts_build
    PACKAGE_DIR
    ${CMAKE_CURRENT_SOURCE_DIR}
    DISPLAY_NAME
    nflow_gui
    BUILD_SCRIPT
    build
  )
endif()

nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.nflow"
  "*.c"
  EXTRA_DIRS
  dist
  examples
  resources
)
