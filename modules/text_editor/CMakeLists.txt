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
set(module_name text_editor)
set(module_library_name nlsText_editor)
set(module_library_builtin_name ${module_library_name}_builtin)
#==============================================================================
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
add_definitions(${Qt5Widgets_DEFINITIONS})
add_definitions("-std=c++11")
set(CMAKE_CXX_FLAGS "${Qt5Widgets_EXECUTABLE_COMPILE_FLAGS}")
#==============================================================================
include_directories(
${Boost_INCLUDE_DIRS}
${Qt5Widgets_INCLUDES}
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../modules_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../os_functions/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../functions_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../text_completion/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
)
#==============================================================================	
FILE(
GLOB
TEXT_EDITOR_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
)
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${TEXT_EDITOR_SRC}
)
#==============================================================================	
target_link_libraries(${module_library_name}
nlsCharacters_encoding
nlsFunctions_manager
nlsGui
nlsI18n
nlsCore
nlsInterpreter
nlsModules_manager
nlsNelson_manager
nlsOs_functions
nlsText_completion
${Qt5Widgets_LIBRARIES}
${Qt5PrintSupport_LIBRARIES}
)
#==============================================================================	
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
FILE(
GLOB
TEXT_EDITOR_BUILTIN
"${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp/*.cpp"
)
#==============================================================================	
include_directories(
${CMAKE_CURRENT_SOURCE_DIR}/builtin/include
${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../api_nelson/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
)
#==============================================================================	
ADD_LIBRARY(
	nlsText_editor_builtin
	SHARED 
	${TEXT_EDITOR_BUILTIN}
)
#==============================================================================	
target_link_libraries(${module_library_builtin_name}
${module_library_name}
nlsError_manager
nlsInterpreter
nlsTypes
nlsApi_nelson
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
install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/resources DESTINATION ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.*")
install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/functions DESTINATION ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.nlf")
install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help DESTINATION ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.qch" PATTERN "xml" EXCLUDE PATTERN "md" EXCLUDE)
#==============================================================================




