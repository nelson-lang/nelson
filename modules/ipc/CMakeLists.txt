# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name ipc)
set(module_library_name nlsIpc)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WITHOUT_IPC_MODULE)
  nelson_module_sources_from_glob(
    IPC_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp)
else()
  nelson_module_sources_from_glob(
    IPC_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)
endif()

add_library(${module_library_name} ${IPC_SRC})

if(WITHOUT_IPC_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include)
  target_link_libraries(${module_library_name} PRIVATE nlsTypes)
else()
  if(CMAKE_SYSTEM_NAME
     STREQUAL
     "Linux")
    find_library(LIBRT rt)
  endif()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${EIGEN3_INCLUDE_DIR}
            ${Boost_INCLUDE_DIRS}
            ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../file_archiver/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../time/src/include)
  target_link_libraries(
    ${module_library_name}
    PRIVATE nlsCharacters_encoding
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
            ${Boost_LIBRARIES}
            ${LIBRT})
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
if(NOT WITHOUT_IPC_MODULE)
  nelson_module_sources_from_glob(
    IPC_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

  add_library(${module_library_builtin_name} ${IPC_BUILTIN})

  target_include_directories(
    ${module_library_builtin_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${Boost_INCLUDE_DIRS})

  target_link_libraries(
    ${module_library_builtin_name}
    PRIVATE ${module_library_name}
            nlsCore
            nlsError_manager
            nlsI18n
            nlsInterpreter
            nlsNelson_manager
            nlsTypes)

  nelson_install_library(${module_library_builtin_name})
  nelson_install_module_data(${module_name})
endif()
# ==============================================================================
