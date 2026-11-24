# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# nlsGui — backend library for nelson-gui (WebUI + RPC + Interface).
#
# Opt-in: built when configured with -DWITH_GUI_MODULE=ON (see
# modules/CMakeLists.txt and INTEGRATION.md).
# ==============================================================================
set(module_name gui)
set(module_library_name nlsGui)
# ==============================================================================
nelson_module_sources_from_glob(
  GUI_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
# ==============================================================================
add_library(${module_library_name} ${GUI_SRC})
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
target_compile_definitions(${module_library_name} PRIVATE NLSGUI_EXPORTS)
# ==============================================================================
target_link_libraries(${module_library_name}
  PUBLIC nlsWeb_engine
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsCore
    nlsDebugger
    nlsI18n
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
# Desktop builtins (dialogs, browsers, terminal, render_html, uihtml, examples),
# relocated here from the former webview module.
set(module_library_builtin_name ${module_library_name}_builtin)
nelson_module_sources_from_glob(
  GUI_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)
add_library(${module_library_builtin_name} ${GUI_BUILTIN})
target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../web_engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../web_engine/src/webui/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include # GuiDialogProvider.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
)
target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsWeb_engine
    nlsCharacters_encoding
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsEngine
    nlsModules_manager
    nlsNelson_manager
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.js"
  "*.html"
  "*.css"
  "*.xml"
)
# ==============================================================================
# Frontend bundle (index.html + app.js) — built by vite from src/ts/frontend
# into the module-root dist (a gitignored build output). `npm run build` bundles
# the sources, and its postbuild step folds the CSS into index.html and also
# rebuilds the figure page (figure.html + figure.js), so every caller of
# `npm run build` gets it. nelson-gui resolves this dist from the module tree at
# runtime; it is installed under modules/gui/dist, the same convention as nflow_gui.
# ==============================================================================
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/src/ts/frontend/package.json)
  set(GUI_FRONTEND_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/ts/frontend)
  nelson_add_frontend_build(
    ${module_library_name}
    gui_frontend_build
    PACKAGE_DIR
    ${GUI_FRONTEND_DIR}
    DISPLAY_NAME
    gui
    BUILD_SCRIPT
    build
    EXTRA_BUILD_SCRIPTS
    build:replayer
  )
endif()
# ==============================================================================
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/dist/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/dist
)
# The client-only figure replayer bundle, inlined by saveas(fig,'f.html') into a
# self-contained interactive page. Built by the build:replayer script above.
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/dist-replayer/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/dist-replayer
)
