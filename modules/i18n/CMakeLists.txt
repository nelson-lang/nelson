# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name i18n)
set(module_library_name nlsI18n)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if (WITHOUT_I18N_MODULE)
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include)
else()
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
  ${Boost_INCLUDE_DIRS})
endif()
# ==============================================================================
if (WITHOUT_I18N_MODULE)
file(GLOB I18N_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
else()
file(GLOB I18N_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} SHARED ${I18N_SRC})
# ==============================================================================
if (WITHOUT_I18N_MODULE)
target_link_libraries(${module_library_name} nlsCharacters_encoding)
else()                      
target_link_libraries(${module_library_name} nlsCharacters_encoding
                      ${Boost_LIBRARIES})
endif()
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
if (WITHOUT_I18N_MODULE)
file(GLOB I18N_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/no-builtin/cpp/*.cpp")
else()
file(GLOB I18N_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
endif()
# ==============================================================================
if (WITHOUT_I18N_MODULE)
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/no-builtin/include
  ${CMAKE_CURRENT_SOURCE_DIR}/no-src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include)
else()
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include)
endif()
# ==============================================================================
add_library(${module_library_builtin_name} SHARED ${I18N_BUILTIN})
# ==============================================================================
target_link_libraries(
  ${module_library_builtin_name}
  ${module_library_name}
  nlsError_manager
  nlsInterpreter
  nlsOverload
  nlsTypes
  nlsApi_nelson
  nlsFunction_handle
  nlsCharacters_encoding)
# ==============================================================================
set_target_properties(
  ${module_library_builtin_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS ${module_library_builtin_name}
        LIBRARY DESTINATION ${LIBRARY_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/startup.m
        DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/finish.m
        DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/loader.m
        DESTINATION ${ROOT_OUTPUT}/modules/${module_name})
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
  DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
  FILES_MATCHING
  PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help
  DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/src/include
  DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/src
  FILES_MATCHING
  PATTERN "*.hpp")
# ==============================================================================
