# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name text_editor)
set(module_library_name nlsText_editor)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(CMAKE_AUTOMOC ON)
# ==============================================================================
if(WITHOUT_TEXT_EDITOR_MODULE)
  nelson_module_sources_from_glob(
    TEXT_EDITOR_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp)
else()
  nelson_module_sources_from_glob(
    TEXT_EDITOR_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)
endif()

add_library(${module_library_name} ${TEXT_EDITOR_SRC})

if(WITHOUT_TEXT_EDITOR_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include)
else()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../functions_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../text_completion/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../debugger/src/include)
  target_link_libraries(
    ${module_library_name}
    PRIVATE nlsCharacters_encoding
            nlsCommons
            nlsEngine
            nlsFunctions_manager
            nlsGui
            nlsI18n
            nlsInterpreter
            nlsModules_manager
            nlsNelson_manager
            nlsText_completion
            nlsDebugger
            Qt${QT_VERSION_MAJOR}::Core
            Qt${QT_VERSION_MAJOR}::Widgets
            Qt${QT_VERSION_MAJOR}::PrintSupport)
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
if(NOT WITHOUT_TEXT_EDITOR_MODULE)
  nelson_module_sources_from_glob(
    TEXT_EDITOR_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

  add_library(${module_library_builtin_name} ${TEXT_EDITOR_BUILTIN})

  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include)

  target_link_libraries(
    ${module_library_builtin_name}
    PRIVATE ${module_library_name}
            nlsError_manager
            nlsI18n
            nlsInterpreter
            nlsTypes
            nlsNelson_manager
            nlsOs_functions)

  nelson_install_library(${module_library_builtin_name})
endif()
# ==============================================================================
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.txt"
  "*.ref"
  EXTRA_DIRS
  resources)
# ==============================================================================
