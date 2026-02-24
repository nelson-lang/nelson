# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name help_tools)
set(module_library_name nlsHelp_tools)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  HELP_TOOLS_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/c/cmark)

add_library(${module_library_name} ${HELP_TOOLS_SRC})

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/c/cmark
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../xml/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../localization/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
          ${LIBXML2_INCLUDE_DIR})

target_link_libraries(
  ${module_library_name}
  PRIVATE nlsCharacters_encoding
          nlsCommons
          nlsError_manager
          nlsI18n
          nlsInterpreter
          nlsModules_manager
          nlsNelson_manager
          nlsXml
          ${LIBXML2_LIBRARIES})
if(NOT WITHOUT_TBB AND TBB_FOUND)
  target_link_libraries(${module_library_name} PRIVATE ${TBB_LIBRARY})
endif()
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  HELP_TOOLS_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

add_library(${module_library_builtin_name} ${HELP_TOOLS_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../xml/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
          nlsCharacters_encoding
          nlsCommons
          nlsError_manager
          nlsI18n
          nlsInterpreter
          nlsNelson_manager
          nlsTypes)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_DIR_PATTERNS
  "tests/xml:*.xml"
  "tests/md:*.md"
  "resources:*.*"
  "templates:*.xml")
# help_tools also installs *.json from help/
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.json")
# ==============================================================================
