# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name web_engine)
set(module_library_name nlsWeb_engine)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(WEBUI_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/src/webui)
set(WEBUI_SOURCES
  ${WEBUI_ROOT}/src/webui.c
  ${WEBUI_ROOT}/src/civetweb/civetweb.c
)

if(APPLE)
  enable_language(OBJC)
  list(APPEND WEBUI_SOURCES
    ${WEBUI_ROOT}/src/webview/wkwebview.m
    ${WEBUI_ROOT}/src/webview/macos_render_html.m
    ${WEBUI_ROOT}/src/webview/macos_html_to_pdf.m
    ${CMAKE_CURRENT_SOURCE_DIR}/src/objc/MacFigurePrint.m
  )
elseif(WIN32)
  # webui >= 2.5.0-beta.4 factors the WebView2 backend out of webui.c.
  list(APPEND WEBUI_SOURCES ${WEBUI_ROOT}/src/webview/win32_wv2.cpp)
endif()

if(NOT WIN32 AND CMAKE_C_COMPILER_ID MATCHES "Clang|GNU|AppleClang")
  set_source_files_properties(${WEBUI_ROOT}/src/webui.c
    PROPERTIES COMPILE_OPTIONS "-fvisibility=default"
  )
endif()

nelson_module_sources_from_glob(
  WEB_ENGINE_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${WEB_ENGINE_SRC} ${WEBUI_SOURCES})

if(WIN32)
  set_target_properties(${module_library_name}
    PROPERTIES VS_GLOBAL_CharacterSet "MultiByte"
  )
  if(MSVC)
    target_compile_options(${module_library_name} PRIVATE /UUNICODE /U_UNICODE)
  else()
    target_compile_options(${module_library_name} PRIVATE -UUNICODE -U_UNICODE)
  endif()
endif()

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<BUILD_INTERFACE:${WEBUI_ROOT}/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${WEBUI_ROOT}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
)

target_compile_definitions(${module_library_name}
  PRIVATE
    NO_CACHING
    NO_CGI
    NLSWEB_ENGINE_USE_WEBUI_DLLMAIN
    USE_WEBSOCKET
    NO_SSL
)

if(WIN32)
  target_link_libraries(${module_library_name}
    PRIVATE
      ws2_32
      user32
      shell32
      ole32
      winhttp
  )
elseif(APPLE)
  find_library(COCOA_FRAMEWORK Cocoa REQUIRED)
  find_library(WEBKIT_FRAMEWORK WebKit REQUIRED)
  target_link_libraries(${module_library_name}
    PRIVATE
      ${COCOA_FRAMEWORK}
      ${WEBKIT_FRAMEWORK}
  )
else()
  target_link_libraries(${module_library_name} PRIVATE pthread m dl)
endif()

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons # DynamicLibrary (examples providers)
    nlsI18n
    nlsModules_manager # GetModules (examples catalog)
    nlsNelson_manager # current locale
    nlsTypes # ArrayOf, Dimensions (relocated dialogs)
    nlsInterpreter # PathFunctionIndexerManager, BuiltInFunctionDefManager (relocated dialogs)
    nlsError_manager # Error / RaiseError (relocated dialogs)
)
# ==============================================================================
# web_engine is a shared engine library (webui + helpers), linked by gui, ai,
# nflow_gui and nmm_gui. Its desktop builtins, functions, help and tests were
# relocated to the gui module, so it exposes no builtin layer and is not loaded
# through the module registry.
# ==============================================================================
nelson_install_library(${module_library_name})
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/frontend/examples
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/frontend
  FILES_MATCHING
    PATTERN "*.html"
    PATTERN "*.css"
    PATTERN "*.js"
)
