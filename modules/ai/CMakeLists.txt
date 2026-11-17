# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name ai)
set(module_library_name nlsAi)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  AI_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${AI_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../json/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include # nlohmann/json
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCore
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsJson
    nlsNelson_manager
    nlsTypes
)

# Token streaming of model replies needs libcurl; without it (e.g. a wasm build)
# the streaming client compiles as a stub and callers fall back to webwrite.
if(WITH_LIBCURL)
  target_include_directories(${module_library_name}
    PRIVATE ${CURL_INCLUDE_DIRS}
  )
  target_link_libraries(${module_library_name} PRIVATE ${CURL_LIBRARIES})
  target_compile_definitions(${module_library_name} PRIVATE NELSON_AI_WITH_CURL)
endif()
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  AI_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${AI_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    # AssistantSink (per-surface event routing + Stop) is used by __ai_notify__
    # and __ai_interrupt_pending__ in EVERY build, webview or not.
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsCharacters_encoding
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsNelson_manager
    nlsTypes
)

# The live-progress push (__ai_notify__ -> PushWebToolEvent) needs the WebView
# host; when that module is disabled (minimal/wasm builds) that one call compiles
# as a no-op, but the AssistantSink routing (nelson_manager, linked above) stays.
if(WITHOUT_WEBVIEW_MODULE)
  target_compile_definitions(${module_library_builtin_name}
    PRIVATE NLSAI_WITHOUT_WEBVIEW
  )
else()
  target_include_directories(${module_library_builtin_name}
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../webview/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../webview/src/webui/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
  )
  target_link_libraries(${module_library_builtin_name}
    PRIVATE
      nlsWebview
      nlsModules_manager
  )
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name} EXTRA_DIRS skills-catalog resources)
