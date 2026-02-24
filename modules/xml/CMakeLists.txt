# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name xml)
set(module_library_name nlsXml)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  XML_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)

add_library(${module_library_name} ${XML_SRC})

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
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
          ${LIBXML2_INCLUDE_DIR}
          ${LIBXSLT_INCLUDE_DIR})

target_link_libraries(
  ${module_library_name}
  PRIVATE nlsCharacters_encoding
          nlsI18n
          ${LIBXML2_LIBRARIES}
          ${LIBXSLT_LIBRARIES})
if(NOT WITHOUT_TBB AND TBB_FOUND)
  target_link_libraries(${module_library_name} PRIVATE ${TBB_LIBRARY})
endif()
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  XML_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

add_library(${module_library_builtin_name} ${XML_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
          nlsCommons
          nlsNelson_manager
          nlsError_manager
          nlsI18n
          nlsInterpreter
          nlsTypes)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
# Custom install: tests include *.xml/*.xslt/*.xsd alongside *.m,
# and help only excludes xml/ (not md/)
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.xml"
  "*.xslt"
  "*.xsd")
# Override help install: exclude only xml/, keep md/
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.nhz"
  PATTERN "xml" EXCLUDE)
# ==============================================================================
