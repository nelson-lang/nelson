# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name netcdf)
set(module_library_name nlsNetcdf)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  NETCDF_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${NETCDF_SRC})
target_compile_features(${module_library_name} PRIVATE cxx_std_20)

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${NETCDF_INCLUDE_DIRS}
)

if(NOT WIN32)
  target_link_directories(${module_library_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
endif()

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsStream_manager
    nlsTypes
    ${NETCDF_LIBRARIES}
)

if(MSVC)
  target_compile_definitions(${module_library_name} PRIVATE NLSNETCDF_EXPORTS)
endif()
# ==============================================================================
nelson_module_sources_from_glob(
  NETCDF_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${NETCDF_BUILTIN})
target_compile_features(${module_library_builtin_name} PRIVATE cxx_std_20)

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsInterpreter
    nlsTypes
)

if(MSVC)
  target_compile_definitions(${module_library_builtin_name}
    PRIVATE NLSNETCDF_BUILTIN_EXPORTS
  )
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
if(WIN32 AND NETCDF_RUNTIME_DIR)
  install(
    DIRECTORY "${NETCDF_RUNTIME_DIR}/"
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    FILES_MATCHING PATTERN "*.dll"
  )
endif()
nelson_install_module_data(
  ${module_name}
  EXTRA_TEST_PATTERNS
  "*.nc"
)
