# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nmm_gui)
set(module_library_builtin_name nlsNmm_gui_builtin)
# ==============================================================================
# Package manager window (WebUI). Native WebUI/CivetWeb/WebView code is owned by
# modules/webview: this module only carries the singleton window, the RPC
# dispatcher and the static frontend, and depends on nlsWebview for WebUI.
# ==============================================================================
# Window strings: locale/nmm-ui-<locale>.json (Crowdin catalogs) are embedded in
# the binary through a generated header, one raw literal per catalog line (the
# Visual Studio project does the same in its GenerateNmmGuiLocaleJson target).
set(_nmm_gui_generated_dir ${CMAKE_CURRENT_BINARY_DIR}/generated)
file(GLOB _nmm_gui_catalogs
  CONFIGURE_DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/locale/nmm-ui-*.json
)
list(SORT _nmm_gui_catalogs)
set(NMM_GUI_UI_CATALOGS "")
foreach(_nmm_gui_catalog ${_nmm_gui_catalogs})
  set_property(
    DIRECTORY
    APPEND
    PROPERTY
      CMAKE_CONFIGURE_DEPENDS
      ${_nmm_gui_catalog}
  )
  get_filename_component(_nmm_gui_locale ${_nmm_gui_catalog} NAME_WE)
  string(REPLACE "nmm-ui-" "" _nmm_gui_locale "${_nmm_gui_locale}")
  file(READ ${_nmm_gui_catalog} _nmm_gui_json)
  string(REPLACE "\r" "" _nmm_gui_json "${_nmm_gui_json}")
  string(REGEX REPLACE "\n+$" "" _nmm_gui_json "${_nmm_gui_json}")
  string(REPLACE
    "\n"
    ")nmmui\"\n      R\"nmmui("
    _nmm_gui_json
    "${_nmm_gui_json}"
  )
  string(APPEND
    NMM_GUI_UI_CATALOGS
    "    { \"${_nmm_gui_locale}\",\n      R\"nmmui(${_nmm_gui_json})nmmui\" },\n"
  )
endforeach()
string(REGEX REPLACE "\n$" "" NMM_GUI_UI_CATALOGS "${NMM_GUI_UI_CATALOGS}")
file(MAKE_DIRECTORY ${_nmm_gui_generated_dir})
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/NmmGuiLocaleJson.hpp.in
  ${_nmm_gui_generated_dir}/NmmGuiLocaleJson.hpp
  @ONLY
)
# ==============================================================================
nelson_module_sources_from_glob(
  NMM_GUI_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)
add_library(${module_library_builtin_name} ${NMM_GUI_BUILTIN})
target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${_nmm_gui_generated_dir}
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../webview/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../webview/src/webui/include
)
# the embedded catalogs are UTF-8
target_compile_options(${module_library_builtin_name}
  PRIVATE $<$<CXX_COMPILER_ID:MSVC>:/utf-8>
)
target_link_libraries(${module_library_builtin_name}
  PRIVATE
    nlsCharacters_encoding
    nlsEngine
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsModules_manager
    nlsNelson_manager
    nlsTypes
    nlsWebview
)
# ==============================================================================
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# ==============================================================================
# React + TypeScript frontend, built by Vite into frontend/dist (a gitignored
# build output) and served from there by WebUI (see nmm_guiBuiltin.cpp).
# ==============================================================================
nelson_add_frontend_build(
  ${module_library_builtin_name}
  nmm_gui_frontend_build
  PACKAGE_DIR
  ${CMAKE_CURRENT_SOURCE_DIR}/frontend
  DISPLAY_NAME
  nmm_gui
  BUILD_SCRIPT
  build
)
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/frontend/dist/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/frontend/dist
)
