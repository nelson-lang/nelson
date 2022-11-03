# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name terminal)
set(module_library_name nlsTerminal)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
include_directories(
  ${Boost_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/src/c
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../history_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include)
# ==============================================================================
file(GLOB TERMINAL_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/BasicTerminal.cpp"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/BsdTerminal.cpp"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/linenoise.cpp")
# ==============================================================================
add_library(${module_library_name} SHARED ${TERMINAL_SRC})
# ==============================================================================
target_link_libraries(${module_library_name} 
  nlsStream_manager
  nlsInterpreter
  nlsCharacters_encoding
  nlsI18n
  nlsCommons
  nlsHistory_manager)
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
# install(TARGETS ${module_library_builtin_name} LIBRARY DESTINATION
# ${LIBRARY_OUTPUT}) install(FILES
# ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/startup.m DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name}/etc) install(FILES
# ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/finish.m DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name}/etc) install(FILES
# ${CMAKE_SOURCE_DIR}/modules/${module_name}/loader.m DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name}) install(DIRECTORY
# ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.m")
# install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.qch" PATTERN
# "xml" EXCLUDE PATTERN "md" EXCLUDE)
# ==============================================================================
