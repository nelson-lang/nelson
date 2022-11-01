# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name sio_client)
set(module_library_name nlsSio_client)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if (WITHOUT_SIO_CLIENT_MODULE)
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/no-src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include)
else()
include_directories(
  ${Boost_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
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
endif()  
# ==============================================================================
if (WITHOUT_SIO_CLIENT_MODULE)
file(GLOB SIO_CLIENT_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
else()
file(GLOB SIO_CLIENT_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/src/*.cpp"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/socket_io/src/internal/*.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} SHARED ${SIO_CLIENT_SRC})
# ==============================================================================
if (WITHOUT_SIO_CLIENT_MODULE)
target_link_libraries(
  ${module_library_name})
else()
target_link_libraries(
  ${module_library_name}
  nlsError_manager
  nlsTypes
  nlsInterpreter
  nlsNelson_manager
  nlsCharacters_encoding
  nlsCommons
  nlsI18n)
endif()
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
file(GLOB SIO_CLIENT_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
# ==============================================================================
if (NOT WITHOUT_SIO_CLIENT_MODULE)
# ==============================================================================
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include)
# ==============================================================================
add_library(${module_library_builtin_name} SHARED ${SIO_CLIENT_BUILTIN})
# ==============================================================================
target_link_libraries(
  ${module_library_builtin_name}
  ${module_library_name}
  nlsApi_nelson
  nlsError_manager
  nlsInterpreter
  nlsOverload
  nlsTypes
  nlsI18n
  nlsElementary_functions)
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
if (WITHOUT_SIO_CLIENT_MODULE)
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
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help
  DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
  DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
  FILES_MATCHING
  PATTERN "*.m")
endif()
# ==============================================================================
