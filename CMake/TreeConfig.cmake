# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(WIN32)
  message(STATUS "Windows build: using an explicit bin output directory.")
elseif("${CMAKE_BINARY_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
  message(STATUS "Build in sources tree.")
else()
  message(STATUS "Build in binaries tree.")
  if(NELSON_PORTABLE_BUILD)
    # Portable profiles must be able to coexist with a regular desktop
    # build.  Keep their artifacts in the selected binary tree instead of
    # routing both configurations through the historical source-tree bin
    # directory.
    if(IS_SYMLINK "${CMAKE_BINARY_DIR}/bin")
      file(READ_SYMLINK "${CMAKE_BINARY_DIR}/bin" _nelson_bin_symlink_target)
      if(_nelson_bin_symlink_target STREQUAL "${CMAKE_SOURCE_DIR}/bin")
        file(REMOVE "${CMAKE_BINARY_DIR}/bin")
      else()
        message(FATAL_ERROR
          "Refusing to replace unexpected bin symlink: ${_nelson_bin_symlink_target}"
        )
      endif()
    endif()
    file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
  else()
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/bin"
        "${CMAKE_BINARY_DIR}/bin"
    )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/etc"
        "${CMAKE_BINARY_DIR}/etc"
    )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/modules"
        "${CMAKE_BINARY_DIR}/modules"
    )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/tools"
        "${CMAKE_BINARY_DIR}/tools"
    )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/locale"
        "${CMAKE_BINARY_DIR}/locale"
    )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/resources"
        "${CMAKE_BINARY_DIR}/resources"
    )
  endif()
endif()
