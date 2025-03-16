# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name core)
set(module_library_name nlsCore)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
file(GLOB CORE_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER CORE_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_name} ${CORE_SRC})
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_name}
  PRIVATE ${EIGEN3_INCLUDE_DIR}
          ${OpenMP_CXX_INCLUDE_DIRS}
          ${Boost_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include/picoSHA2)

target_link_libraries(
  ${module_library_name}
  nlsCharacters_encoding
  nlsCommons
  nlsError_manager
  nlsFiles_folders_functions
  nlsI18n
  nlsInterpreter
  nlsNelson_manager
  nlsOs_functions
  nlsStream_manager
  nlsTypes
  ${MAC_FRAMEWORK_FOUNDATION_LIBRARY})
# ==============================================================================
file(
  GLOB
  CORE_HEADER
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/EvaluateCommand.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/EvaluateInterface.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/EvaluateScriptFile.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/src/include/nlsCore_exports.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${CORE_HEADER}")
# ==============================================================================
file(GLOB CORE_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
list(FILTER CORE_BUILTIN EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
# ==============================================================================
add_custom_target(invalidate_file ALL
  COMMAND ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/versionBuiltin.cpp")
# ==============================================================================
add_library(${module_library_builtin_name} ${CORE_BUILTIN})
target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_builtin_name}
  PRIVATE ${OpenMP_CXX_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
          ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  ${module_library_name}
  nlsCharacters_encoding
  nlsCommons
  nlsError_manager
  nlsI18n
  nlsInterpreter
  nlsNelson_manager
  nlsTypes
  ${OpenMP_CXX_LIBRARIES})
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
