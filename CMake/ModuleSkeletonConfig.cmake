# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(EXISTS "${CMAKE_SOURCE_DIR}/module_skeleton")
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/etc/startup.m
          DESTINATION ${ROOT_OUTPUT}/module_skeleton/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/etc/finish.m
          DESTINATION ${ROOT_OUTPUT}/module_skeleton/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/loader.m
          DESTINATION ${ROOT_OUTPUT}/module_skeleton OPTIONAL)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/builder.m
          DESTINATION ${ROOT_OUTPUT}/module_skeleton)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/module.json
          DESTINATION ${ROOT_OUTPUT}/module_skeleton)
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/functions
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/help
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/builtin
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/src
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/tests
    DESTINATION ${ROOT_OUTPUT}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
endif()
# ==============================================================================
