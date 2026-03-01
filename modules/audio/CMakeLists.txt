# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name audio)
set(module_library_name nlsAudio)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
if(NOT TAGLIB_FOUND)
  set(TAGLIB_INCLUDES "")
  set(TAGLIB_LIBRARIES "")
endif()
# ==============================================================================
nelson_module_sources_from_glob(
  AUDIO_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)

add_library(${module_library_name} ${AUDIO_SRC})

target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${EIGEN3_INCLUDE_DIR}
          ${PORTAUDIO_INCLUDE_DIRS}
          ${SNDFILE_INCLUDE_DIRS}
          ${TAGLIB_INCLUDES}
          ${ALSA_INCLUDE_DIRS}
          ${JACK_INCLUDE_DIRS}
          ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
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
  PRIVATE nlsInterpreter
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
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  AUDIO_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

add_library(${module_library_builtin_name} ${AUDIO_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
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
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
          ${PORTAUDIO_INCLUDE_DIRS}
          ${SNDFILE_INCLUDE_DIRS})

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
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
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.wav"
  "*.ogg"
  "*.flac"
  "*.json"
  EXTRA_DIR_PATTERNS
  "examples:*.wav"
  "examples:*.flac"
  "examples:*.ogg"
  "examples:*.m")
# ==============================================================================
