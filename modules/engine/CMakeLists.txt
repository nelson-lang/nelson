# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name engine)
set(module_library_name nlsEngine)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  ENGINE_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${ENGINE_SRC})

if(NELSON_WASM_BUILD)
  target_compile_definitions(${module_library_name}
    PRIVATE NELSON_WASM_PERSISTENT_SESSION
  )
endif()

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${EIGEN3_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../memory_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../file_archiver/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../terminal/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../localization/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../sparse/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../time/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../ipc/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsCore
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsIpc
    nlsLocalization
    nlsMemory_manager
    nlsModules_manager
    nlsNelson_manager
    nlsOs_functions
    nlsSparse
    nlsStream_manager
    nlsTerminal
    nlsTypes
)

# WEB_GUI support loads its backend only when a Web mode is requested.
if(WITH_WEB_GUI_MODULE)
  target_compile_definitions(${module_library_name}
    PRIVATE NELSON_WEB_GUI_ENABLED
  )
  target_include_directories(${module_library_name}
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../web_gui/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../webview/src/include
  )
  add_dependencies(${module_library_name} nlsWeb_gui)
endif()

file(GLOB ENGINE_HEADER
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/nlsEngine_exports.h"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/MainEvaluator.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/PortableEngine.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/PortableEngineC.h"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/NelsonEngine.h"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/StartNelsonApplication.h"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/NelSon_engine_mode.h"
)
set_target_properties(${module_library_name}
  PROPERTIES
    VERSION ${Nelson_VERSION}
    SOVERSION ${Nelson_VERSION_MAJOR}
    PUBLIC_HEADER "${ENGINE_HEADER}"
)
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  ENGINE_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${ENGINE_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsNelson_manager
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
