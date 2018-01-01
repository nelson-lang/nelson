#==============================================================================
# Copyright (c) 2016-2018 Allan CORNET (Nelson)
#==============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
#==============================================================================
include_directories(
${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
)
#==============================================================================	
FILE(
GLOB
MAINCLI_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/nelson_cli/*.cpp"
)
#==============================================================================	
ADD_EXECUTABLE(
	nelson-cli
	${MAINCLI_SRC}
)
#==============================================================================	
target_link_libraries(nelson-cli
nlsEngine
)
#==============================================================================
FILE(
GLOB
MAINADVCLI_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/nelson_adv_cli/*.cpp"
)
#==============================================================================	
ADD_EXECUTABLE(
	nelson-adv-cli
	${MAINADVCLI_SRC}
)
#==============================================================================	
target_link_libraries(nelson-adv-cli
nlsEngine
)
#==============================================================================
FILE(
GLOB
MAINGUI_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/nelson_gui/*.cpp"
)
#==============================================================================	
ADD_EXECUTABLE(
	nelson-gui
	${MAINGUI_SRC}
)
#==============================================================================	
target_link_libraries(nelson-gui
nlsEngine
)
#==============================================================================
add_custom_target(Name ALL
    COMMAND nelson-cli --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/gettext/generatemo.nls"
    DEPENDS nelson-cli
)
#==============================================================================
add_custom_target(updatelocalization
    COMMAND nelson-cli --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/gettext/generatepo.nls"
    DEPENDS nelson-cli
)
#==============================================================================
add_custom_target(buildhelp
    COMMAND ${CMAKE_COMMAND} -E env 'QTDIR_BINARIES=${QTDIR_BINARIES}' ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-cli  --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/buildhelp/buildhelp.nls"
    DEPENDS nelson-cli
)
#==============================================================================
add_custom_target(missing_help
    COMMAND ${CMAKE_COMMAND} -E env 'QTDIR_BINARIES=${QTDIR_BINARIES}' ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/nelson-adv-cli  --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/missing_help/missing_help.nls"
    DEPENDS nelson-cli
)
#==============================================================================
add_custom_target(tests_all
    COMMAND nelson-adv-cli --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_all/runtests_all.nls"
    DEPENDS nelson-cli
)
#==============================================================================
add_custom_target(tests_all_no_display
    COMMAND nelson-cli --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_all/runtests_all.nls"
    DEPENDS nelson-cli
)
#==============================================================================
add_custom_target(tests_minimal
    COMMAND nelson-cli --quiet --file "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.nls"
    DEPENDS nelson-cli
)
#==============================================================================
install(TARGETS nelson-adv-cli RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-cli RUNTIME DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS nelson-gui RUNTIME DESTINATION ${LIBRARY_OUTPUT})
#==============================================================================

