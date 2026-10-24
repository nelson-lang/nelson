# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name fftw)
set(module_library_name nlsFftw)
set(module_library_builtin_name ${module_library_name}_builtin)
option(NELSON_FFTW_STATIC_BACKEND
  "Link FFTW symbols directly instead of loading runtime libraries"
  OFF
)
set(
  NELSON_FFTW_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing fftw3.h for the static FFTW backend"
)
set(
  NELSON_FFTW_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static double-precision FFTW library"
)
set(
  NELSON_FFTWF_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static single-precision FFTW library"
)
# ==============================================================================
nelson_module_sources_from_glob(
  FFTW_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${FFTW_SRC})

if(NELSON_FFTW_STATIC_BACKEND)
  if(NOT NELSON_FFTW_LIBRARY OR NOT NELSON_FFTWF_LIBRARY)
    message(FATAL_ERROR
      "NELSON_FFTW_STATIC_BACKEND requires NELSON_FFTW_LIBRARY and "
      "NELSON_FFTWF_LIBRARY."
    )
  endif()
endif()

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${OpenMP_CXX_INCLUDE_DIRS}
    ${EIGEN3_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
)

if(NELSON_FFTW_STATIC_BACKEND)
  target_compile_definitions(${module_library_name}
    PRIVATE NELSON_FFTW_STATIC_BACKEND
  )
  if(NELSON_FFTW_INCLUDE_DIR)
    target_include_directories(${module_library_name} BEFORE
      PRIVATE ${NELSON_FFTW_INCLUDE_DIR}
    )
  endif()
endif()

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsNelson_manager
    nlsI18n
    nlsTypes
    ${OpenMP_CXX_LIBRARIES}
)
if(NELSON_FFTW_STATIC_BACKEND)
  target_link_libraries(${module_library_name}
    PRIVATE
      ${NELSON_FFTW_LIBRARY}
      ${NELSON_FFTWF_LIBRARY}
  )
endif()
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  FFTW_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${FFTW_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
