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
set(module_name history_manager)
set(module_library_name nlsHistory_manager)
set(module_library_builtin_name ${module_library_name}_builtin)
#==============================================================================
include_directories(
${Boost_INCLUDE_DIRS}
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../elementary_mathematics/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../data_structures/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)
#==============================================================================	
FILE(
GLOB
HISTORY_MANAGER_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${HISTORY_MANAGER_SRC}
)
#==============================================================================	
target_link_libraries(${module_library_name}
nlsElementary_mathematics
nlsError_manager
nlsInterpreter
nlsTypes
nlsCharacters_encoding
nlsData_structures
nlsCore
nlsI18n
${BOOST_LIBRARY}
)
#==============================================================================	
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
FILE(
GLOB
HISTORY_MANAGER_BUILTIN
"${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp"
)
#==============================================================================	
include_directories(
${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_builtin_name}
	SHARED 
	${HISTORY_MANAGER_BUILTIN}
)
#==============================================================================	
target_link_libraries(${module_library_builtin_name}
${module_library_name}
nlsApi_nelson
nlsError_manager
nlsInterpreter
nlsOverload
nlsTypes
nlsI18n
)
#==============================================================================
set_target_properties(${module_library_builtin_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
install(TARGETS ${module_library_builtin_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/startup.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc) 
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/finish.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/loader.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name})
install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions DESTINATION ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.nlf")
install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help DESTINATION ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.qch" PATTERN "xml" EXCLUDE PATTERN "md" EXCLUDE)
#==============================================================================





