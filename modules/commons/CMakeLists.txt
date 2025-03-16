# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name commons)
set(module_library_name nlsCommons)
# ==============================================================================
file(GLOB COMMONS_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/fmt/src/*.cc")
list(FILTER COMMONS_SRC EXCLUDE REGEX
     "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/dllMain.cpp")
# ==============================================================================
add_library(${module_library_name} ${COMMONS_SRC})
target_compile_definitions(${module_library_name} PRIVATE FMT_HEADER_ONLY FMT_LIB_EXPORT)
set(COMMONS_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src/include
                         ${CMAKE_CURRENT_SOURCE_DIR}/src/fmt/include)
target_include_directories(
  ${module_library_name}
  PUBLIC "$<BUILD_INTERFACE:${COMMONS_INCLUDE_DIRS}>"
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/stduuid
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include)
# ==============================================================================
target_link_libraries(${module_library_name} 
  PRIVATE
    nlsCharacters_encoding 
    ${CMAKE_DL_LIBS})
if(NOT WITHOUT_TBB AND TBB_FOUND)
  target_link_libraries(${module_library_name} 
  PRIVATE 
    ${TBB_LIBRARY})
endif()
# ==============================================================================
file(GLOB COMMONS_HEADER
     "${CMAKE_CURRENT_SOURCE_DIR}/src/include/Nelson_VERSION.h"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/include/nlsBuildConfig.h")
set_target_properties(
  ${module_library_name}
  PROPERTIES VERSION ${Nelson_VERSION}
             SOVERSION ${Nelson_VERSION_MAJOR}
             PUBLIC_HEADER "${COMMONS_HEADER}")
# ==============================================================================
include(GNUInstallDirs)
install(
  TARGETS ${module_library_name}
  EXPORT ${PROJECT_NAME}-targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${module_name})
# ==============================================================================
