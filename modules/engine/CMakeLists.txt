# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name engine)
set(module_library_name nlsEngine)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
file(GLOB ENGINE_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER ENGINE_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_name} ${ENGINE_SRC})
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_name}
  PRIVATE ${EIGEN3_INCLUDE_DIR}
          ${Boost_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/src/include
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
          ${CMAKE_CURRENT_SOURCE_DIR}/../sio_client/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../sio_client/src/socket_io/src)

target_link_libraries(
  ${module_library_name}
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
  nlsSio_client
  nlsStream_manager
  nlsTerminal)
# ==============================================================================
file(
  GLOB
  ENGINE_HEADER
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/nlsEngine_exports.h"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/MainEvaluator.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/NelSon_engine_mode.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${ENGINE_HEADER}")
# ==============================================================================
file(GLOB ENGINE_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
list(FILTER ENGINE_BUILTIN EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_builtin_name} ${ENGINE_BUILTIN})
target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_builtin_name}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  ${module_library_name}
  nlsError_manager
  nlsI18n
  nlsInterpreter
  nlsNelson_manager
  nlsTypes)
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
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/etc
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
# ==============================================================================
