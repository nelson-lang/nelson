# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name audio)
set(module_library_name nlsAudio)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
file(GLOB AUDIO_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER AUDIO_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_name} ${AUDIO_SRC})
# ==============================================================================
if(NOT TAGLIB_FOUND)
  set(TAGLIB_INCLUDES "")
  set(TAGLIB_LIBRARIES "")
endif()
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>)
target_include_directories(
  ${module_library_name}
  PRIVATE ${EIGEN3_INCLUDE_DIR}
          ${Boost_INCLUDE_DIRS}
          ${PORTAUDIO_INCLUDE_DIRS}
          ${SNDFILE_INCLUDE_DIRS}
          ${TAGLIB_INCLUDES}
          ${ALSA_INCLUDE_DIRS}
          ${JACK_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/src/c
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../operators/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include)
target_link_libraries(
  ${module_library_name}
  nlsCharacters_encoding
  nlsCommons
  nlsError_manager
  nlsI18n
  nlsOperators
  nlsTypes
  ${PORTAUDIO_LIBRARIES}
  ${SNDFILE_LIBRARIES}
  ${TAGLIB_LIBRARIES}
  ${ALSA_LIBRARIES}
  ${JACK_LIBRARIES})
# ==============================================================================
file(GLOB AUDIO_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
list(FILTER AUDIO_BUILTIN EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_builtin_name} ${AUDIO_BUILTIN})
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
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../display_format/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../handle/src/include
          ${PORTAUDIO_INCLUDE_DIRS}
          ${SNDFILE_INCLUDE_DIRS})
target_link_libraries(
  ${module_library_builtin_name}
  ${module_library_name}
  nlsCharacters_encoding
  nlsDisplay_format
  nlsError_manager
  nlsHandle
  nlsI18n
  nlsInterpreter
  nlsNelson_manager
  nlsStream_manager
  nlsTypes)
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
install(
  TARGETS ${module_library_builtin_name}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
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
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.wav")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.ogg")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.flac")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/tests
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.json")
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
# ==============================================================================
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.wav")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.flac")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.ogg")
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/examples
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.m")
# ==============================================================================
