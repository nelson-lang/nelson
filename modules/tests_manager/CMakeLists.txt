# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name tests_manager)
set(module_library_name nlsTests_manager)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(TESTS_MANAGER_SRC
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/ParseTags.cpp
)

if(NOT NELSON_PORTABLE_BUILD)
  list(APPEND TESTS_MANAGER_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/TestChildProcessPosix.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/TestChildProcessWindows.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/TestProcessSupervisor.cpp
  )
endif()

add_library(${module_library_name} ${TESTS_MANAGER_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsFiles_folders_functions
    nlsI18n
    nlsInterpreter
)
# ==============================================================================
# Builtin library
# ==============================================================================
set(TESTS_MANAGER_BUILTIN
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/Gateway.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_parsetags__Builtin.cpp
)

if(NOT NELSON_PORTABLE_BUILD)
  list(APPEND TESTS_MANAGER_BUILTIN
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/ProcessSupervisorBuiltinHelpers.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_cleanup_processes__Builtin.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_process_supervisor_poll__Builtin.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_process_supervisor_start__Builtin.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_process_supervisor_stop__Builtin.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_worker_emit__Builtin.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_worker_receive__Builtin.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/__nelson_unittest_run_processes__Builtin.cpp
  )
endif()

add_library(${module_library_builtin_name} ${TESTS_MANAGER_BUILTIN})

if(NELSON_PORTABLE_BUILD)
  target_compile_definitions(${module_library_builtin_name}
    PRIVATE NELSON_TESTS_MANAGER_INLINE_ONLY
  )
endif()

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../string/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsFiles_folders_functions
    nlsI18n
    nlsInterpreter
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# Extra: minimal_tests.m and all *.m from module root
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/minimal_tests.m
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
)
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING PATTERN "*.m"
)
