# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name main)
# ==============================================================================
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include)
# ==============================================================================
file(GLOB MAINCLI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_cli/*.cpp")
# ==============================================================================
add_executable(nelson-cli-exec ${MAINCLI_SRC})
# ==============================================================================
# force some dependencies here, to force build order
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  set(HELP_TOOLS_LIBRARIES nlsHelp_tools nlsHelp_tools_builtin)
endif()
# ==============================================================================
target_link_libraries(
  nelson-cli-exec
  nlsEngine
  nlsModules_manager
  nlsModules_manager_builtin
  nlsMemory_manager
  nlsMemory_manager_builtin
  ${HELP_TOOLS_LIBRARIES})
# ==============================================================================
if(NOT WITHOUT_GUI_MODULE)
  file(GLOB MAINADVCLI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_adv_cli/*.cpp")
  add_executable(nelson-adv-cli-exec ${MAINADVCLI_SRC})
  target_link_libraries(nelson-adv-cli-exec nlsEngine)
endif()
# ==============================================================================
if(NOT WITHOUT_GUI_MODULE)
  file(GLOB MAINGUI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_gui/*.cpp")
  add_executable(nelson-gui-exec ${MAINGUI_SRC})
  target_link_libraries(nelson-gui-exec nlsEngine)
endif()
# ==============================================================================
if(NOT WITHOUT_SIO_CLIENT_MODULE)
  file(GLOB MAINSIOCLI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_sio_cli/*.cpp")
  add_executable(nelson-sio-cli-exec ${MAINSIOCLI_SRC})
  target_link_libraries(nelson-sio-cli-exec nlsEngine)
endif()
# ==============================================================================
if(NOT WITHOUT_WEBTOOLS_MODULE)
  set(GET_MODULE_SKELETON_ACTION
      COMMAND
      ${BIN_DIRECTORY}/nelson-cli
      --noipc
      --nousermodules
      --quiet
      --file
      "${BIN_DIRECTORY}/../../tools/clone_module_skeleton/clone.m"
  )
  set(BUILD_ALL_GET_MODULE_SKELETON_ACTION ${GET_MODULE_SKELETON_ACTION})
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  set(BUILDHELP_ACTION
      COMMAND
      ${CMAKE_COMMAND}
      -E
      env
      'QTDIR_BINARIES=${QTDIR_BINARIES}'
      ${BIN_DIRECTORY}/nelson-cli
      --noipc
      --nousermodules
      --quiet
      --file
      "${BIN_DIRECTORY}/../../tools/buildhelp/buildhelp.m")
  set(BUILD_ALL_BUILDHELP_ACTION ${BUILDHELP_ACTION})
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  set(TESTS_MINIMAL_ACTION
      COMMAND
      ${BIN_DIRECTORY}/nelson-cli
      --nousermodules
      --noipc
      --quiet
      --file
      "${BIN_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.m"
  )
  set(BUILD_ALL_TESTS_MINIMAL_ACTION ${TESTS_MINIMAL_ACTION})
endif()
# ==============================================================================
list(APPEND executableList nelson-cli-exec)
if(NOT WITHOUT_GUI_MODULE)
  list(APPEND executableList nelson-gui-exec nelson-adv-cli-exec)
endif()
add_custom_target(
  Name ALL
  COMMAND ${BUILD_ALL_LOCALIZATION_ACTION}
  DEPENDS ${executableList})
# ==============================================================================
if(NOT WITHOUT_LOCALIZATION_MODULE)
  add_custom_target(
    localization
    COMMAND ${LOCALIZATION_ACTION}
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_LOCALIZATION_MODULE)
  add_custom_target(
    updatelocalization
    COMMAND
    ${BIN_DIRECTORY}/nelson-cli --nousermodules --noipc
      --quiet --file
      "${BIN_DIRECTORY}/../../tools/gettext/generatepo.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_WEBTOOLS_MODULE)
  add_custom_target(
    get_module_skeleton
    COMMAND ${GET_MODULE_SKELETON_ACTION}
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  add_custom_target(
    buildhelp
    COMMAND ${BUILDHELP_ACTION}
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  add_custom_target(
    missing_help
    COMMAND
    ${BIN_DIRECTORY}/nelson-adv-cli --nousermodules --noipc
      --quiet --file
      "${BIN_DIRECTORY}/../../tools/missing_help/missing_help.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  add_custom_target(
    tests_all
    COMMAND
    ${BIN_DIRECTORY}/nelson-adv-cli --quiet --noipc --file
      "${BIN_DIRECTORY}/../../tools/tests_all/runtests_all.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  add_custom_target(
    benchs_all
    COMMAND
    ${BIN_DIRECTORY}/nelson-adv-cli --quiet --noipc --file
      "${BIN_DIRECTORY}/../../tools/benchs_all/runbenchs_all.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  add_custom_target(
    tests_all_no_display
    COMMAND
    ${BIN_DIRECTORY}/nelson-cli --quiet --noipc --file
      "${BIN_DIRECTORY}/../../tools/tests_all/runtests_all.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  add_custom_target(
    benchs_all_no_display
    COMMAND
    ${BIN_DIRECTORY}/nelson-cli --quiet --noipc --file
      "${BIN_DIRECTORY}/../../tools/benchs_all/runbenchs_all.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  add_custom_target(
    tests_minimal
    COMMAND
    ${BIN_DIRECTORY}/nelson-cli --nousermodules --noipc
      --quiet --file
      "${BIN_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.m"
    DEPENDS nelson-cli-exec)
endif()
# ==============================================================================
include(GNUInstallDirs)
install(
  FILES ${BIN_DIRECTORY}/nelson
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS
    WORLD_EXECUTE
    WORLD_READ
    GROUP_EXECUTE
    GROUP_READ
    OWNER_EXECUTE
    OWNER_READ
    OWNER_WRITE)

if(NOT WITHOUT_GUI_MODULE)
  install(
    FILES ${BIN_DIRECTORY}/nelson-adv-cli
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS
      WORLD_EXECUTE
      WORLD_READ
      GROUP_EXECUTE
      GROUP_READ
      OWNER_EXECUTE
      OWNER_READ
      OWNER_WRITE)
endif()

install(
  FILES ${BIN_DIRECTORY}/nelson-cli
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS
    WORLD_EXECUTE
    WORLD_READ
    GROUP_EXECUTE
    GROUP_READ
    OWNER_EXECUTE
    OWNER_READ
    OWNER_WRITE)

if(NOT WITHOUT_GUI_MODULE)
  install(
    FILES ${BIN_DIRECTORY}/nelson-gui
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS
      WORLD_EXECUTE
      WORLD_READ
      GROUP_EXECUTE
      GROUP_READ
      OWNER_EXECUTE
      OWNER_READ
      OWNER_WRITE)
endif()

if(NOT WITHOUT_SIO_CLIENT_MODULE)
  install(
    FILES ${BIN_DIRECTORY}/nelson-sio-cli
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS
      WORLD_EXECUTE
      WORLD_READ
      GROUP_EXECUTE
      GROUP_READ
      OWNER_EXECUTE
      OWNER_READ
      OWNER_WRITE)
endif()
# ==============================================================================
install(TARGETS nelson-cli-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
if(NOT WITHOUT_GUI_MODULE)
  install(TARGETS nelson-adv-cli-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(TARGETS nelson-gui-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
if(NOT WITHOUT_SIO_CLIENT_MODULE)
  install(TARGETS nelson-sio-cli-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
# ==============================================================================
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
# ==============================================================================
