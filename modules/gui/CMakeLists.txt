# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name gui)
set(module_library_name nlsGui)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(CMAKE_AUTOMOC ON)
# ==============================================================================
nelson_module_sources_from_glob(
  GUI_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  EXCLUDE
  "moc_.*\\.cpp$"
  "qrc_.*\\.cpp$"
)

add_library(${module_library_name} ${GUI_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../history_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../qml_engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../text_completion/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../dictionary/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../profiler/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../ipc/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsInterpreter
    nlsTypes
    nlsError_manager
    nlsNelson_manager
    nlsStream_manager
    nlsCharacters_encoding
    nlsCore
    nlsHistory_manager
    nlsI18n
    nlsOs_functions
    nlsCommons
    nlsEngine
    nlsText_completion
    nlsDictionary
    nlsModules_manager
    nlsProfiler
    nlsIpc
    Qt6::Core
    Qt6::Widgets
    Qt6::Network
    Qt6::PrintSupport
)
# ==============================================================================
# Builtin library
# ==============================================================================
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
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsInterpreter
    nlsTypes
    nlsNelson_manager
    nlsCore
    nlsI18n
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.xml"
  EXTRA_DIR_PATTERNS
  "resources:*.qss"
)
