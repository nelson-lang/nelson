# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name text_editor)
set(module_library_name nlsText_editor)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(CMAKE_AUTOMOC ON)
add_definitions(${Qt${QT_VERSION_MAJOR}Widgets_DEFINITIONS})
set(CMAKE_CXX_FLAGS "${Qt${QT_VERSION_MAJOR}Widgets_EXECUTABLE_COMPILE_FLAGS}")
# ==============================================================================
if(WITHOUT_TEXT_EDITOR_MODULE)
file(GLOB MEX_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
list(FILTER MEX_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/dllMain.cpp")
else()
file(GLOB TEXT_EDITOR_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER TEXT_EDITOR_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} ${TEXT_EDITOR_SRC})
# ==============================================================================
if(WITHOUT_TEXT_EDITOR_MODULE)
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_name}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/no-src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include)

target_link_libraries(${module_library_name})
else()
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_name}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
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
          ${Qt${QT_VERSION_MAJOR}Widgets_INCLUDE_DIRS})

target_link_libraries(
  ${module_library_name}
  nlsCharacters_encoding
  nlsCommons
  nlsEngine
  nlsFunctions_manager
  nlsGui
  nlsI18n
  nlsInterpreter
  nlsModules_manager
  nlsNelson_manager
  nlsText_completion
  Qt${QT_VERSION_MAJOR}::Core
  Qt${QT_VERSION_MAJOR}::Widgets
  Qt${QT_VERSION_MAJOR}::PrintSupport)
endif()
# ==============================================================================
if(WITHOUT_TEXT_EDITOR_MODULE)
else()
file(GLOB TEXT_EDITOR_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
list(FILTER TEXT_EDITOR_BUILTIN EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_builtin_name} ${TEXT_EDITOR_BUILTIN})
target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_builtin_name}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
          ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include)

target_link_libraries(${module_library_builtin_name} ${module_library_name}
                      nlsError_manager nlsI18n nlsInterpreter nlsTypes)
# ==============================================================================
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
if(WITHOUT_TEXT_EDITOR_MODULE)
else()
install(
  TARGETS ${module_library_builtin_name}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
endif()
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
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.txt")
install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
    FILES_MATCHING
    PATTERN "*.ref")
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
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.*")
# ==============================================================================
