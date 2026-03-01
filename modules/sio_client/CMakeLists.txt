# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name sio_client)
set(module_library_name nlsSio_client)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(WITHOUT_SIO_CLIENT_MODULE)
  nelson_module_sources_from_glob(
    SIO_CLIENT_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp)
else()
  nelson_module_sources_from_glob(
    SIO_CLIENT_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/src
    ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/src/internal)
endif()

add_library(${module_library_name} ${SIO_CLIENT_SRC})

if(WITHOUT_SIO_CLIENT_MODULE)
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/no-src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include)
else()
  target_include_directories(
    ${module_library_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
           $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE ${Boost_INCLUDE_DIRS}
            ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
            ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/src
            ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/src/internal
            ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/lib/websocketpp
            ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/lib/asio/asio/include
            ${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/lib/rapidjson/include)
  target_link_libraries(
    ${module_library_name}
    PRIVATE nlsCharacters_encoding
            nlsCommons
            nlsInterpreter
            nlsNelson_manager
            nlsStream_manager
            nlsTypes
            ${Boost_LIBRARIES})
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
# ==============================================================================
if(NOT WITHOUT_SIO_CLIENT_MODULE)
  nelson_module_sources_from_glob(
    SIO_CLIENT_BUILTIN
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

  add_library(${module_library_builtin_name} ${SIO_CLIENT_BUILTIN})

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
            ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include)

  target_link_libraries(
    ${module_library_builtin_name}
    PRIVATE ${module_library_name}
            nlsError_manager
            nlsI18n
            nlsInterpreter
            nlsTypes)

  nelson_install_library(${module_library_builtin_name})
  nelson_install_module_data(${module_name})
endif()
# ==============================================================================
