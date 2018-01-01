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
set(module_name help_tools)
set(module_library_name nlsHelp_tools)
set(module_library_builtin_name ${module_library_name}_builtin)
#==============================================================================
include_directories(
${Boost_INCLUDE_DIRS}
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/src/c/hoedown
${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../localization/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../dynamic_link/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
${LIBXML2_INCLUDE_DIR}
)
#==============================================================================	
FILE(
GLOB
HELP_TOOLS_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
"${CMAKE_CURRENT_SOURCE_DIR}/src/c/hoedown/*.c"
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${HELP_TOOLS_SRC}
)
#==============================================================================	
target_link_libraries(${module_library_name}
nlsError_manager
nlsTypes
nlsInterpreter
nlsCharacters_encoding
nlsI18n
nlsModules_manager
nlsCore
nlsFiles_folders_functions
nlsLocalization
${LIBXML2_LIBRARIES}
)
#==============================================================================	
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
FILE(
GLOB
HELP_TOOLS_BUILTIN
"${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp"
)
#==============================================================================	
include_directories(
${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../data_structures/src/include
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_builtin_name}
	SHARED 
	${HELP_TOOLS_BUILTIN}
)
#==============================================================================	
target_link_libraries(${module_library_builtin_name}
${module_library_name}
nlsError_manager
nlsInterpreter
nlsOverload
nlsTypes
nlsApi_nelson
nlsFunction_handle
nlsCharacters_encoding
nlsI18n
nlsFiles_folders_functions
nlsModules_manager
nlsData_structures
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
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources/about.txt DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/resources)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources/highlight.pack.js DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/resources)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources/mono-blue.css DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/resources)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources/nelson_help_collection.qhc DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/resources)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources/style.css DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/resources)
#==============================================================================





