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
set(module_name text_completion)
set(module_library_name nlsText_completion)
#==============================================================================
include_directories(
${Boost_INCLUDE_DIRS}
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../functions_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
)
#==============================================================================	
FILE(
GLOB
TEXT_COMPLETION_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${TEXT_COMPLETION_SRC}
)
#==============================================================================	
target_link_libraries(${module_library_name}
nlsInterpreter
nlsTypes
nlsError_manager
nlsCharacters_encoding
nlsCore
nlsI18n
nlsGui
nlsModules_manager
nlsFunctions_manager
nlsCharacters_encoding
nlsOs_functions
)
#==============================================================================	
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
#==============================================================================




