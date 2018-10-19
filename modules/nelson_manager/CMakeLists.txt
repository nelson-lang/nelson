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
set(module_name nelson_manager)
set(module_library_name nlsNelson_manager)
#==============================================================================
include_directories(
${Boost_INCLUDE_DIRS}
${CMAKE_CURRENT_SOURCE_DIR}/src/include
)
#==============================================================================	
FILE(
GLOB
CORE_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${CORE_SRC}
)
#==============================================================================	
target_link_libraries(${module_library_name}
nlsCharacters_encoding
)
#==============================================================================	
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
#==============================================================================

