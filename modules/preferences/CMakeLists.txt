# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name preferences)
set(module_library_name nlsPreferences)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  PREFERENCES_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${PREFERENCES_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../hdf5/src/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../hdf5/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${HDF5_C_INCLUDE_DIR}
    ${HDF5_INCLUDE_DIRS}
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsHdf5
    nlsI18n
    nlsInterpreter
    nlsNelson_manager
    nlsStream_manager
    nlsTypes
    ${HDF5_LIBRARIES}
)
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  PREFERENCES_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${PREFERENCES_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsStream_manager
    nlsTypes
)

if(TARGET nlsGui)
  target_compile_definitions(${module_library_builtin_name}
    PRIVATE WITH_NELSON_GUI_PREFS
  )
  target_include_directories(${module_library_builtin_name}
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
  )
  target_link_libraries(${module_library_builtin_name} PRIVATE nlsGui)
endif()
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
