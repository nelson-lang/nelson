# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name parquet)
set(module_library_name nlsParquet)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  PARQUET_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${PARQUET_SRC})
target_compile_features(${module_library_name} PRIVATE cxx_std_20)

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${PARQUET_INCLUDE_DIRS}
    ${ARROW_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
)

if(NOT WIN32)
  target_link_directories(${module_library_name}
    PRIVATE
      ${PARQUET_LIBRARY_DIRS}
      ${ARROW_LIBRARY_DIRS}
  )
endif()

set(PARQUET_LINK_LIBRARIES ${PARQUET_LIBRARIES} ${ARROW_LIBRARIES})
list(REMOVE_DUPLICATES PARQUET_LINK_LIBRARIES)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsTypes
    ${PARQUET_LINK_LIBRARIES}
)

if(MSVC)
  target_compile_definitions(${module_library_name} PRIVATE NLSPARQUET_EXPORTS)
endif()
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  PARQUET_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${PARQUET_BUILTIN})
target_compile_features(${module_library_builtin_name} PRIVATE cxx_std_20)

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
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

if(MSVC)
  target_compile_definitions(${module_library_builtin_name}
    PRIVATE NLSPARQUET_BUILTIN_EXPORTS
  )
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
if(WIN32 AND ARROW_RUNTIME_DIR)
  install(
    DIRECTORY "${ARROW_RUNTIME_DIR}/"
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    FILES_MATCHING PATTERN "*.dll"
  )
endif()
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.parquet"
)
