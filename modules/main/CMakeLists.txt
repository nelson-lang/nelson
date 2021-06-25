# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the License,
# or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation;
# either version 2 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
# ==============================================================================
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include)
# ==============================================================================
file(GLOB MAINCLI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_cli/*.cpp")
# ==============================================================================
add_executable(nelson-cli-exec ${MAINCLI_SRC})
# ==============================================================================
# force some dependencies here, to force build order
# ==============================================================================
target_link_libraries(
  nelson-cli-exec
  nlsEngine
  nlsModules_manager
  nlsModules_manager_builtin
  nlsMemory_manager
  nlsMemory_manager_builtin
  nlsHelp_tools
  nlsHelp_tools_builtin)
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
add_custom_target(
  Name ALL
  COMMAND
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --nousermodules --noipc --quiet
    --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/gettext/generatemo.m"
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
    ${CMAKE_COMMAND} -E env 'QTDIR_BINARIES=${QTDIR_BINARIES}'
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --noipc --nousermodules --quiet --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/clone_module_skeleton/clone.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  buildhelp
  COMMAND
    ${CMAKE_COMMAND} -E env 'QTDIR_BINARIES=${QTDIR_BINARIES}'
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli --noipc --nousermodules --quiet --file
    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/buildhelp/buildhelp.m"
  DEPENDS nelson-cli-exec)
# ==============================================================================
add_custom_target(
  missing_help
  COMMAND
    ${CMAKE_COMMAND} -E env 'QTDIR_BINARIES=${QTDIR_BINARIES}'
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
install(FILES ${BIN_DIRECTORY}/nelson-adv-cli DESTINATION ${LIBRARY_OUTPUT})
install(FILES ${BIN_DIRECTORY}/nelson-cli DESTINATION ${LIBRARY_OUTPUT})
install(FILES ${BIN_DIRECTORY}/nelson-gui DESTINATION ${LIBRARY_OUTPUT})
install(FILES ${BIN_DIRECTORY}/nelson-sio-cli DESTINATION ${LIBRARY_OUTPUT})
# ==============================================================================
install(TARGETS nelson-adv-cli-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-cli-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-gui-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-sio-cli-exec RUNTIME DESTINATION ${LIBRARY_OUTPUT})
# ==============================================================================
