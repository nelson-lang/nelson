# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if ("${CMAKE_BINARY_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
  message(STATUS "Build in sources tree.")
else()
  message(STATUS "Build in binaries tree.")
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/etc" "${CMAKE_BINARY_DIR}/etc")
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/bin" "${CMAKE_BINARY_DIR}/bin")
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/modules" "${CMAKE_BINARY_DIR}/modules")
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/tools" "${CMAKE_BINARY_DIR}/tools")
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/locale" "${CMAKE_BINARY_DIR}/locale")
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/resources" "${CMAKE_BINARY_DIR}/resources")
endif()
# ==============================================================================
