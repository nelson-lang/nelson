# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name characters_encoding)
set(module_library_name nlsCharacters_encoding)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if (WITHOUT_CHARACTERS_ENCODING_MODULE)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/no-src/include)
else()
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src/include
                    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/simdutf
                    ${ICU_INCLUDE_DIRS}
                    ${Boost_INCLUDE_DIRS})
endif()                    
# ==============================================================================
if (WITHOUT_CHARACTERS_ENCODING_MODULE)
file(GLOB CHARACTERS_ENCODING_SRC "${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp")
else()
file(GLOB CHARACTERS_ENCODING_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/simdutf/*.cpp")
endif()
# ==============================================================================
add_library(${module_library_name} SHARED ${CHARACTERS_ENCODING_SRC})
# ==============================================================================
target_link_libraries(${module_library_name} ${ICU_LIBRARIES})
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
if (NOT WITHOUT_CHARACTERS_ENCODING_MODULE)
file(GLOB CHARACTERS_ENCODING_BUILTIN
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
# ==============================================================================
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
  ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include)
# ==============================================================================
add_library(${module_library_builtin_name} SHARED
            ${CHARACTERS_ENCODING_BUILTIN})
# ==============================================================================
target_link_libraries(
  ${module_library_builtin_name}
  ${module_library_name}
  nlsElementary_functions
  nlsError_manager
  nlsInterpreter
  nlsOverload
  nlsTypes
  nlsI18n
  nlsApi_nelson)
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
if (WITHOUT_CHARACTERS_ENCODING_MODULE)
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
  PATTERN "*.txt")
endif()  
# ==============================================================================
