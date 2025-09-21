# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name i18n)
set(module_library_name nlsI18n)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WITHOUT_I18N_MODULE)
  file(GLOB I18N_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
  list(FILTER I18N_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/dllMain.cpp")
else()
  file(GLOB I18N_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
  list(FILTER I18N_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} ${I18N_SRC})
# ==============================================================================
if(WITHOUT_I18N_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include)
# ==============================================================================
  target_link_libraries(${module_library_name} 
    PRIVATE
      nlsCharacters_encoding)
# ==============================================================================
else()
# ==============================================================================
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include)
# ==============================================================================
  target_link_libraries(${module_library_name} 
    PRIVATE
      nlsCharacters_encoding
      nlsNelson_manager)
# ==============================================================================
endif()
# ==============================================================================
if(WITHOUT_I18N_MODULE)
  file(GLOB I18N_HEADER "${CMAKE_CURRENT_SOURCE_DIR}/no-src/include/*.hpp"
       "${CMAKE_CURRENT_SOURCE_DIR}/no-src/include/*.h")
else()
  file(GLOB I18N_HEADER "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.hpp"
       "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.h")
endif()
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${I18N_HEADER}")
# ==============================================================================
if(NOT WITHOUT_I18N_MODULE)
  file(GLOB I18N_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
  list(FILTER I18N_BUILTIN EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
  # ==============================================================================
  add_library(${module_library_builtin_name} ${I18N_BUILTIN})
  # ==============================================================================
  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include)
  # ==============================================================================
  target_link_libraries(${module_library_builtin_name}
    PRIVATE
      ${module_library_name}
      nlsError_manager
      nlsInterpreter
      nlsTypes)
  # ==============================================================================
  file(GLOB I18N_HEADER "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.hpp"
       "${CMAKE_CURRENT_SOURCE_DIR}/src/include/*.h")
  set_target_properties(
    ${module_library_builtin_name}
    PROPERTIES VERSION ${Nelson_VERSION}
               SOVERSION ${Nelson_VERSION_MAJOR}
               PUBLIC_HEADER "${I18N_HEADER}")
endif()
# ==============================================================================
include(GNUInstallDirs)
# ==============================================================================
install(
  TARGETS ${module_library_name}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
# ==============================================================================
if(NOT WITHOUT_I18N_MODULE)
  # ==============================================================================
  install(
    TARGETS ${module_library_builtin_name}
    EXPORT ${PROJECT_NAME}-targets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER
      DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
  # ==============================================================================
  install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/loader.m
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/etc
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.nhz"
    PATTERN "xml" EXCLUDE
    PATTERN "md" EXCLUDE)
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
    DESTINATION
      "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.m")
  # ==============================================================================
endif()
# ==============================================================================
