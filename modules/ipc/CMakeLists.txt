# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name ipc)
set(module_library_name nlsIpc)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WITHOUT_IPC_MODULE)
  include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/no-src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include)
else()
  include_directories(
    ${EIGEN3_INCLUDE_DIR}
    ${Boost_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../file_archiver/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../time/src/include)
endif()
# ==============================================================================
if(WITHOUT_IPC_MODULE)
  file(GLOB ENGINE_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
  list(FILTER ENGINE_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/dllMain.cpp")
else()
  file(GLOB ENGINE_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
  list(FILTER ENGINE_SRC EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} SHARED ${ENGINE_SRC})
# ==============================================================================
if(NOT WITHOUT_IPC_MODULE)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    find_library(LIBRT rt)
  endif()
endif()
# ==============================================================================
if(WITHOUT_IPC_MODULE)
  target_link_libraries(${module_library_name} nlsTypes)
else()
  target_link_libraries(
    ${module_library_name}
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsFile_archiver
    nlsI18n
    nlsInterpreter
    nlsNelson_manager
    nlsOs_functions
    nlsStream_manager
    nlsTime
    nlsTypes
    ${BOOST_LIBRARIES}
    ${LIBRT})
endif()
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
if(NOT WITHOUT_IPC_MODULE)
  # ==============================================================================
  file(GLOB ENGINE_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
  list(FILTER ENGINE_BUILTIN EXCLUDE REGEX
       "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
  # ==============================================================================
  include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include)
  # ==============================================================================
  add_library(${module_library_builtin_name} SHARED ${ENGINE_BUILTIN})
  # ==============================================================================
  target_link_libraries(
    ${module_library_builtin_name}
    ${module_library_name}
    nlsCore
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsNelson_manager
    nlsTypes)
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
if(WITHOUT_IPC_MODULE)
  install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
else()
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
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
    DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
    FILES_MATCHING
    PATTERN "*.ref")
endif()
# ==============================================================================
