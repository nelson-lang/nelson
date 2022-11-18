# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name api_nelson)
set(module_library_name nlsApi_nelson)
# ==============================================================================
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
  ${Boost_INCLUDE_DIRS})
# ==============================================================================
file(GLOB API_NELSON_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER API_NELSON_SRC EXCLUDE REGEX "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp") 
# ==============================================================================
add_library(${module_library_name} SHARED ${API_NELSON_SRC})
# ==============================================================================
target_link_libraries(${module_library_name} nlsError_manager nlsTypes nlsI18n)
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
# install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/etc/startup.m
# DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower}/etc) install(FILES
# ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/etc/finish.m DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name_lower}/etc) install(FILES
# ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/loader.m DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name_lower}) install(DIRECTORY
# ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/functions DESTINATION
# ${ROOT_OUTPUT}/modules/${module_name_lower} FILES_MATCHING PATTERN "*.m")
# install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/help
# DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower} FILES_MATCHING PATTERN
# "*.qch" PATTERN "xml" EXCLUDE PATTERN "md" EXCLUDE)
# ==============================================================================
