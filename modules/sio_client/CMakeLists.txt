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
set(module_name sio_client)
set(module_library_name nlsSio_client)
set(module_library_builtin_name ${module_library_name}_builtin)
#==============================================================================
if (WITH_SIO_CLIENT)
include_directories(
${Boost_INCLUDE_DIRS}
${SIO_CLIENT_INCLUDE}
${CMAKE_CURRENT_SOURCE_DIR}/src/include
)
else()
include_directories(
${CMAKE_CURRENT_SOURCE_DIR}/no-src/include
)
endif()
#==============================================================================	
include_directories(
${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)
#==============================================================================	
if (WITH_SIO_CLIENT)
FILE(
GLOB
SIO_CLIENT_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
)
else()
FILE(
GLOB
SIO_CLIENT_SRC
"${CMAKE_CURRENT_SOURCE_DIR}/no-src/cpp/*.cpp"
)
endif()
#==============================================================================	
ADD_LIBRARY(
	${module_library_name}
	SHARED 
	${SIO_CLIENT_SRC}
)
#==============================================================================	
target_link_libraries(${module_library_name}
nlsError_manager
nlsTypes
nlsInterpreter
nlsCharacters_encoding
nlsI18n
)
if (WITH_SIO_CLIENT)
target_link_libraries(${module_library_name}
${SIO_CLIENT_LIBRARY}
)
endif()
#==============================================================================	
set_target_properties(${module_library_name}
                      PROPERTIES 
                      VERSION ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE} SOVERSION ${Nelson_VERSION_MAJOR})
#==============================================================================	
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/startup.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc) 
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/etc/finish.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name}/etc)
install(FILES ${CMAKE_SOURCE_DIR}/modules/${module_name}/loader.nls DESTINATION ${ROOT_OUTPUT}/modules/${module_name})
install(DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${module_name}/help DESTINATION ${ROOT_OUTPUT}/modules/${module_name} FILES_MATCHING PATTERN "*.qch" PATTERN "xml" EXCLUDE PATTERN "md" EXCLUDE)
#==============================================================================


