# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name mex)
set(module_library_name nlsMex)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(NOT WITHOUT_MEX_MODULE)
  include_directories(
    ${EIGEN3_INCLUDE_DIR}
    ${Boost_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../ipc/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../sparse/src/include)
endif()
# ==============================================================================
if(NOT WITHOUT_MEX_MODULE)
  file(GLOB MEX_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
  list(FILTER MEX_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
endif()
# ==============================================================================
if(NOT WITHOUT_MEX_MODULE)
  add_library(${module_library_name} SHARED ${MEX_SRC})
  target_link_libraries(
    ${module_library_name}
    nlsCharacters_encoding
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsIpc
    nlsSparse
    nlsStream_manager
    nlsTypes
    nlsOs_functions
    ${BOOST_LIBRARIES})
endif()
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
if(NOT WITHOUT_MEX_MODULE)
  file(GLOB MEX_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
  list(FILTER MEX_BUILTIN EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
  # ==============================================================================
  include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../validators/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../mex/src/include)
  # ==============================================================================
  add_library(${module_library_builtin_name} SHARED ${MEX_BUILTIN})
  # ==============================================================================
  target_link_libraries(
    ${module_library_builtin_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes
    nlsValidators)
  # ==============================================================================
  set_target_properties(
    ${module_library_builtin_name}
    PROPERTIES
      VERSION
      ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
      SOVERSION ${Nelson_VERSION_MAJOR})
  # ==============================================================================
endif()
# ==============================================================================
if(NOT WITHOUT_MEX_MODULE)
  install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
  install(TARGETS ${module_library_builtin_name}
          LIBRARY DESTINATION ${LIBRARY_OUTPUT})
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/src/include
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/src
    FILES_MATCHING
    PATTERN "*.h"
    PATTERN "*.hpp")
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
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.c")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.c")
endif()
# ==============================================================================
