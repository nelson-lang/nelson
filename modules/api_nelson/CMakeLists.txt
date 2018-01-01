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
set(module_name api_nelson)
set(module_library_name nlsApi_nelson)
#==============================================================================
include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/src/include
	${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
	${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
	${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
	${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
	${Boost_INCLUDE_DIRS}
)
#==============================================================================	
FILE(
GLOB
API_NELSON_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${API_NELSON_SRC}
)
#==============================================================================
target_link_libraries(${module_library_name}
nlsError_manager
nlsTypes
nlsI18n
)
#==============================================================================
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
#install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/etc/startup.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower}/etc) 
#install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/etc/finish.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower}/etc)
#install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/loader.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower})
#install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/functions DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower} FILES_MATCHING PATTERN "*.nlf")
#install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name_lower}/help DESTINATION ${ROOT_OUTPUT}/modules/${module_name_lower} FILES_MATCHING PATTERN "*.qch" PATTERN "xml" EXCLUDE PATTERN "md" EXCLUDE)
#==============================================================================


