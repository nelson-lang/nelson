# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name compiler)
set(module_library_name nlsCompiler)
set(module_library_builtin_name ${module_library_name}_builtin)
if(CMAKE_SYSTEM_NAME STREQUAL "Linux" OR APPLE)
  add_subdirectory(src/installer)
elseif(WIN32)
  add_subdirectory(src/installer/windows)
endif()

nelson_module_sources_from_glob(
  COMPILER_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
nelson_module_sources_from_glob(
  COMPILER_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)
add_library(${module_library_name} ${COMPILER_SRC})
add_library(${module_library_builtin_name} ${COMPILER_BUILTIN})
if(TARGET nelson-runtime-installer)
  add_dependencies(${module_library_builtin_name} nelson-runtime-installer)
endif()
if(TARGET nelson-installer)
  add_dependencies(${module_library_builtin_name} nelson-installer)
endif()

foreach(_target ${module_library_name} ${module_library_builtin_name})
  target_compile_features(${_target} PRIVATE cxx_std_17)
  target_include_directories(${_target}
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../file_archiver/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../mex/src/include
      ${EIGEN3_INCLUDE_DIR}
  )
  nelson_install_library(${_target})
endforeach()
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
)
target_link_libraries(${module_library_name}
  PRIVATE
    nlsInterpreter
    nlsNelson_manager
    nlsCharacters_encoding
    nlsCommons
    nlsTypes
    nlsFile_archiver
)
if(WIN32)
  target_link_libraries(${module_library_name} PRIVATE user32 bcrypt)
endif()
target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsInterpreter
    nlsTypes
    nlsError_manager
    nlsCommons
    nlsI18n
)
nelson_install_module_data(${module_name} EXTRA_TEST_PATTERNS "*.ps1")
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_runtime_installer_linux.py
    DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/tests"
  )
  install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures/runtime_installer/checkpoint.c
    DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/tests/fixtures/runtime_installer"
  )
endif()
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/resources/
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/resources"
)
install(
  FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/examples/standalone/factor.txt
    ${CMAKE_CURRENT_SOURCE_DIR}/examples/standalone/app_icon.png
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/examples/standalone"
)
