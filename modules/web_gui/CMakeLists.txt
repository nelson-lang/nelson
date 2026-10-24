# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# nlsWeb_gui — backend library for nelson-webview (WebUI + RPC + Interface).
#
# Opt-in: built when configured with -DWITH_WEB_GUI_MODULE=ON (see
# modules/CMakeLists.txt and INTEGRATION.md).
# ==============================================================================
set(module_name web_gui)
set(module_library_name nlsWeb_gui)
# ==============================================================================
nelson_module_sources_from_glob(
  WEB_GUI_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
# ==============================================================================
add_library(${module_library_name} ${WEB_GUI_SRC})
# ==============================================================================
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include # nlohmann/json
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../debugger/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include # Interface.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../text_completion/src/include # computeCompletion
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include # Evaluator
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include # NelsonConfiguration
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include # IsExistingModuleName
    ${CMAKE_CURRENT_SOURCE_DIR}/../history_manager/src/include # NelsonHistory
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include # SystemCommand
    ${CMAKE_CURRENT_SOURCE_DIR}/../profiler/src/include
) # Profiler::save
# ==============================================================================
target_compile_definitions(${module_library_name} PRIVATE NLSWEB_GUI_EXPORTS)
# ==============================================================================
target_link_libraries(${module_library_name}
  PUBLIC nlsWebview
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsCore
    nlsDebugger
    nlsError_manager
    nlsStream_manager
    nlsText_completion
    nlsNelson_manager # NelsonConfiguration (interrupt flag)
    nlsModules_manager # installed module capabilities
    nlsHistory_manager # command history
    nlsOs_functions # SystemCommand (cross-platform shell)
    nlsProfiler # profiler export
    nlsInterpreter # Context / Scope / Evaluator
    nlsTypes # ArrayOf
)
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
# Frontend bundle (index.html + app.js) — built by vite from frontend/ into
# frontend/dist (a gitignored build output). `npm run build` bundles the sources
# and its postbuild step folds the CSS into index.html. nelson-webview resolves this
# dist from the module tree at runtime, so it is NOT copied next to the
# executables; it is installed under modules/web_gui/frontend/dist, the same
# convention as nflow_gui.
# ==============================================================================
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/frontend/package.json)
  set(WEB_GUI_FRONTEND_DIR ${CMAKE_CURRENT_SOURCE_DIR}/frontend)
  nelson_add_frontend_build(
    ${module_library_name}
    web_gui_frontend_build
    PACKAGE_DIR
    ${WEB_GUI_FRONTEND_DIR}
    DISPLAY_NAME
    web_gui
    BUILD_SCRIPT
    build
  )
endif()
# ==============================================================================
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/frontend/dist/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/frontend/dist
)
