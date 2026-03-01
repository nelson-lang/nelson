# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Helper functions to reduce boilerplate in Nelson module CMakeLists.txt files.
# ==============================================================================
include_guard(GLOBAL)
include(GNUInstallDirs)
# ==============================================================================
# nelson_install_library(<target>)
#
# Standard install rule for a Nelson library target.
# ==============================================================================
function(nelson_install_library _target)
  install(
    TARGETS ${_target}
    EXPORT ${PROJECT_NAME}-targets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION
      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/$<TARGET_PROPERTY:${_target},NELSON_MODULE_NAME>
  )
endfunction()
# ==============================================================================
# nelson_install_module_data(<module_name>
#   [EXTRA_TEST_PATTERNS  pat1 pat2 ...]
#   [EXTRA_DIRS           dir1 dir2 ...]
#   [EXTRA_DIR_PATTERNS   "dir1:pat1;pat2" "dir2:pat1" ...]
# )
#
# Standard install rules for Nelson module data files (loader.m, etc/, tests/,
# help/, functions/).  Accepts additional test file patterns and extra
# directories with their file patterns.
# ==============================================================================
function(nelson_install_module_data _module_name)
  cmake_parse_arguments(ARG
    ""
    ""
    "EXTRA_TEST_PATTERNS;EXTRA_DIRS;EXTRA_DIR_PATTERNS"
    ${ARGN}
  )

  set(_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${_module_name}")

  # loader.m
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/loader.m")
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/loader.m DESTINATION "${_dest}")
  endif()

  # etc/*.m
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/etc")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/etc
      DESTINATION "${_dest}"
      FILES_MATCHING PATTERN "*.m"
    )
  endif()

  # tests/*.m (always) plus extra patterns
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/tests")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
      DESTINATION "${_dest}"
      FILES_MATCHING PATTERN "*.m"
    )
    foreach(_pat IN LISTS ARG_EXTRA_TEST_PATTERNS)
      install(
        DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
        DESTINATION "${_dest}"
        FILES_MATCHING PATTERN "${_pat}"
      )
    endforeach()
  endif()

  # help/*.nhz  (exclude xml/ and md/)
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/help")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
      DESTINATION "${_dest}"
      FILES_MATCHING
        PATTERN "*.nhz"
        PATTERN "xml"
        EXCLUDE PATTERN "md"
        EXCLUDE
    )
  endif()

  # functions/*.m
  if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module_name}/functions")
    install(
      DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_module_name}/functions
      DESTINATION "${_dest}"
      FILES_MATCHING PATTERN "*.m"
    )
  endif()

  # Extra directories: "dirname" -> install with PATTERN "*.*"
  foreach(_dir IN LISTS ARG_EXTRA_DIRS)
    if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}")
      install(
        DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}
        DESTINATION "${_dest}"
        FILES_MATCHING PATTERN "*.*"
      )
    endif()
  endforeach()

  # Extra directory + patterns: "dir:pat1;pat2"
  foreach(_spec IN LISTS ARG_EXTRA_DIR_PATTERNS)
    string(FIND "${_spec}" ":" _pos)
    if(_pos GREATER -1)
      string(SUBSTRING "${_spec}" 0 ${_pos} _dir)
      math(EXPR _start "${_pos} + 1")
      string(SUBSTRING "${_spec}" ${_start} -1 _pats_str)
      string(REPLACE ";" ";" _pats "${_pats_str}")
      foreach(_pat IN LISTS _pats)
        if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}")
          install(
            DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}
            DESTINATION "${_dest}"
            FILES_MATCHING PATTERN "${_pat}"
          )
        endif()
      endforeach()
    endif()
  endforeach()
endfunction()
# ==============================================================================
# nelson_module_sources_from_glob(<output_var>
#   DIRS dir1 [dir2 ...]
#   [EXCLUDE regex1 ...]
# )
#
# Glob .cpp and .c files from directories, filtering out dllMain and optional
# extra patterns.
# ==============================================================================
function(nelson_module_sources_from_glob _outvar)
  cmake_parse_arguments(ARG "" "" "DIRS;EXCLUDE" ${ARGN})
  set(_all_src)
  foreach(_dir IN LISTS ARG_DIRS)
    file(GLOB _cpp "${_dir}/*.cpp")
    file(GLOB _cxx "${_dir}/*.cxx")
    file(GLOB _cc "${_dir}/*.cc")
    file(GLOB _c "${_dir}/*.c")
    list(APPEND _all_src ${_cpp} ${_cxx} ${_cc} ${_c})
  endforeach()
  # Always exclude dllMain
  list(FILTER _all_src EXCLUDE REGEX "dllMain\\.(cpp|c)$")
  foreach(_ex IN LISTS ARG_EXCLUDE)
    list(FILTER _all_src EXCLUDE REGEX "${_ex}")
  endforeach()
  set(${_outvar} "${_all_src}" PARENT_SCOPE)
endfunction()
