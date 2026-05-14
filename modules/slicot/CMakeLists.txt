# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name slicot)
set(module_library_name nlsSlicot)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
file(GLOB SLICOT_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
list(FILTER SLICOT_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
file(GLOB SLICOT_FORTRAN_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/fortran/*.f")
# ============================================================================== 
set(CONVERTED_SLICOT_C_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src/f2c")
file(MAKE_DIRECTORY ${CONVERTED_SLICOT_C_DIR})


# Ensure f2c tool is available
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(F2C_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../bin/linux")
  set(NELSON_F2C_EXECUTABLE "${F2C_PATH}/nelson_f2c")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")  # macOS
  set(F2C_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../bin/macOS")
  set(NELSON_F2C_EXECUTABLE "${F2C_PATH}/nelson_f2c")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  set(F2C_PATH "${BIN_DIRECTORY}")
  set(NELSON_F2C_EXECUTABLE "${F2C_PATH}/nelson_f2c.exe")
else()
  message(FATAL_ERROR "Unsupported OS: ${CMAKE_SYSTEM_NAME}")
endif()

set(CONVERTED_SLICOT_C_FILES "")

# Ensure nelson_f2c is built before using it
add_custom_target(convert_f2c DEPENDS nelson_f2c)
# ==============================================================================  
foreach(F_FILE ${SLICOT_FORTRAN_SRC})
  # Extract filename without extension
  get_filename_component(FILENAME_WE ${F_FILE} NAME_WE)
  set(C_FILE "${CONVERTED_SLICOT_C_DIR}/${FILENAME_WE}.c")
  # Convert using f2c
  add_custom_command(
    OUTPUT ${C_FILE}
    COMMAND "${NELSON_F2C_EXECUTABLE}" "${F_FILE}" "-E" "-I${CMAKE_CURRENT_SOURCE_DIR}/../f2c/src/include" "-d${CONVERTED_SLICOT_C_DIR}"
    DEPENDS ${F_FILE}
    COMMENT "Converting ${F_FILE} to C using f2c"
    VERBATIM
  )
  list(APPEND CONVERTED_SLICOT_C_FILES ${C_FILE})
endforeach() 
# ==============================================================================  
add_library(${module_library_name} ${SLICOT_SRC} ${CONVERTED_SLICOT_C_FILES})
# The f2c-converted C sources have no visibility annotations, so we must
# make their symbols visible for other libraries to link against them.
# On Windows/MSVC, C_VISIBILITY_PRESET has no effect; WINDOWS_EXPORT_ALL_SYMBOLS
# is required to auto-generate the import library (.lib) alongside the DLL.
set_target_properties(${module_library_name} PROPERTIES
    C_VISIBILITY_PRESET default
    WINDOWS_EXPORT_ALL_SYMBOLS ON)
# ============================================================================== 
# Now that the library target exists, we can add the dependency
add_dependencies(${module_library_name} nelson_f2c)
# ============================================================================== 
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../f2c/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)
# ============================================================================== 
target_link_libraries(${module_library_name} 
  PRIVATE
    nlsCommons
    nlsF2C
    nlsCharacters_encoding
    ${NELSON_BLAS_LAPACKX_LIBRARIES})
# ============================================================================== 
# Builtin library
# ==============================================================================
file(GLOB SLICOT_BUILTIN "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp")
list(FILTER SLICOT_BUILTIN EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/dllMain.cpp")
add_library(${module_library_builtin_name} ${SLICOT_BUILTIN})
# ============================================================================== 
target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include)
# ============================================================================== 
target_link_libraries(
  ${module_library_builtin_name} 
  PRIVATE ${module_library_name}
    nlsError_manager 
    nlsI18n 
    nlsInterpreter
    nlsTypes)
# ============================================================================== 
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# ==============================================================================
