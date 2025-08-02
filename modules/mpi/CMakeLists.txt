# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name mpi)
set(module_library_name nlsMpi)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
file(GLOB CONSTRUCTORS_FUNCTIONS_SRC
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER CONSTRUCTORS_FUNCTIONS_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_name} ${CONSTRUCTORS_FUNCTIONS_SRC})
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${Boost_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../sparse/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../function_handle/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
          ${MPI_INCLUDE_PATH})
# ==============================================================================
target_link_libraries(
  ${module_library_name}
  PRIVATE
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsSparse
    nlsTypes
    ${MPI_C_LIBRARIES})
# ==============================================================================
file(GLOB CONSTRUCTORS_FUNCTIONS_BUILTIN
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
list(FILTER CONSTRUCTORS_FUNCTIONS_BUILTIN EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_builtin_name} ${CONSTRUCTORS_FUNCTIONS_BUILTIN})
# ==============================================================================
target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../handle/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../display_format/src/include
          ${MPI_INCLUDE_PATH})
# ==============================================================================
target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsDisplay_format
    nlsCharacters_encoding
    nlsError_manager
    nlsHandle
    nlsI18n
    nlsInterpreter
    nlsTypes
    ${MPI_C_LIBRARIES})
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
install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions/@MPI_Comm
    DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/functions"
    FILES_MATCHING
    PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
# ==============================================================================
