# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include(GNUInstallDirs)
# ==============================================================================
if(EXISTS "${CMAKE_SOURCE_DIR}/module_skeleton")
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/etc/startup.m
          DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/etc/finish.m
          DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton/etc)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/loader.m
          DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton OPTIONAL)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/builder.m
          DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton)
  install(FILES ${CMAKE_SOURCE_DIR}/module_skeleton/module.json
          DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton)
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/functions
    DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton
    FILES_MATCHING
    PATTERN "*.m")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/help
    DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/builtin
    DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/src
    DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
  install(
    DIRECTORY ${CMAKE_SOURCE_DIR}/module_skeleton/tests
    DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/module_skeleton
    FILES_MATCHING
    PATTERN "*.*")
endif()
# ==============================================================================
