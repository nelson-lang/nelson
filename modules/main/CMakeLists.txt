# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
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
if (NOT WITHOUT_HELP_TOOLS_MODULE)
set (HELP_TOOLS_LIBRARIES nlsHelp_tools nlsHelp_tools_builtin)
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
file(GLOB MAINADVCLI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_adv_cli/*.cpp")
# ==============================================================================
add_executable(nelson-adv-cli-exec ${MAINADVCLI_SRC})
# ==============================================================================
target_link_libraries(nelson-adv-cli-exec nlsEngine)
# ==============================================================================
file(GLOB MAINGUI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_gui/*.cpp")
# ==============================================================================
add_executable(nelson-gui-exec ${MAINGUI_SRC})
# ==============================================================================
target_link_libraries(nelson-gui-exec nlsEngine)
# ==============================================================================
file(GLOB MAINSIOCLI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_sio_cli/*.cpp")
# ==============================================================================
add_executable(nelson-sio-cli-exec ${MAINSIOCLI_SRC})
# ==============================================================================
target_link_libraries(nelson-sio-cli-exec nlsEngine)
# ==============================================================================
set(LOCALIZATION_ACTION 
  COMMAND 
  ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --nousermodules --noipc --quiet
  --file
  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/gettext/generatemo.m")
set(BUILD_ALL_LOCALIZATION_ACTION ${LOCALIZATION_ACTION})
# ==============================================================================
set(GET_MODULE_SKELETON_ACTION
  COMMAND 
  ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --noipc --nousermodules --quiet --file
  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/clone_module_skeleton/clone.m")
set(BUILD_ALL_GET_MODULE_SKELETON_ACTION ${GET_MODULE_SKELETON_ACTION})
# ==============================================================================
if (NOT WITHOUT_HELP_TOOLS_MODULE)
set (BUILDHELP_ACTION
  COMMAND 
  ${CMAKE_COMMAND} -E env 'QTDIR_BINARIES=${QTDIR_BINARIES}'
  ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --noipc --nousermodules --quiet --file
  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/buildhelp/buildhelp.m")
set(BUILD_ALL_BUILDHELP_ACTION ${BUILDHELP_ACTION})
endif()
# ==============================================================================
set (TESTS_MINIMAL_ACTION
  COMMAND
  ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --nousermodules --noipc --quiet
  --file
  "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.m")
set(BUILD_ALL_TESTS_MINIMAL_ACTION ${TESTS_MINIMAL_ACTION})
# ==============================================================================
add_custom_target(
  Name ALL
  COMMAND
  ${BUILD_ALL_LOCALIZATION_ACTION}
  DEPENDS nelson-cli-exec nelson-adv-cli-exec nelson-gui-exec)
# ==============================================================================
add_custom_target(
  localization
  COMMAND
  ${LOCALIZATION_ACTION}
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  updatelocalization
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --nousermodules --noipc --quiet
    --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/gettext/generatepo.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  get_module_skeleton
  COMMAND
  ${GET_MODULE_SKELETON_ACTION}
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  buildhelp
  COMMAND
  ${BUILDHELP_ACTION}
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  missing_help
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-adv-cli --nousermodules --noipc
    --quiet --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/missing_help/missing_help.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  tests_all
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-adv-cli --quiet --noipc --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_all/runtests_all.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  benchs_all
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-adv-cli --quiet --noipc --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/benchs_all/runbenchs_all.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  tests_all_no_display
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --quiet --noipc --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_all/runtests_all.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  benchs_all_no_display
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --quiet --noipc --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/benchs_all/runbenchs_all.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  tests_minimal
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --nousermodules --noipc --quiet
    --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
install(FILES ${BIN_DIRECTORY}/nelson DESTINATION ${LIBRARY_OUTPUT} PERMISSIONS 
  WORLD_EXECUTE WORLD_READ 
  GROUP_EXECUTE GROUP_READ 
  OWNER_EXECUTE OWNER_READ OWNER_WRITE)

install(FILES ${BIN_DIRECTORY}/nelson-adv-cli DESTINATION ${LIBRARY_OUTPUT} PERMISSIONS 
  WORLD_EXECUTE WORLD_READ 
  GROUP_EXECUTE GROUP_READ 
  OWNER_EXECUTE OWNER_READ OWNER_WRITE)

install(FILES ${BIN_DIRECTORY}/nelson-cli DESTINATION ${LIBRARY_OUTPUT} PERMISSIONS
  WORLD_EXECUTE WORLD_READ 
  GROUP_EXECUTE GROUP_READ 
  OWNER_EXECUTE OWNER_READ OWNER_WRITE)

install(FILES ${BIN_DIRECTORY}/nelson-gui DESTINATION ${LIBRARY_OUTPUT} PERMISSIONS
  WORLD_EXECUTE WORLD_READ 
  GROUP_EXECUTE GROUP_READ 
  OWNER_EXECUTE OWNER_READ OWNER_WRITE)

install(FILES ${BIN_DIRECTORY}/nelson-sio-cli DESTINATION ${LIBRARY_OUTPUT} PERMISSIONS
  WORLD_EXECUTE WORLD_READ 
  GROUP_EXECUTE GROUP_READ 
  OWNER_EXECUTE OWNER_READ OWNER_WRITE)
# ==============================================================================
install(TARGETS nelson-adv-cli-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-cli-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-gui-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-sio-cli-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
# ==============================================================================
install(
  DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help
  DESTINATION ${ROOT_OUTPUT}/modules/${module_name}
  FILES_MATCHING
  PATTERN "*.qch"
  PATTERN "xml" EXCLUDE
  PATTERN "md" EXCLUDE)
# ==============================================================================
