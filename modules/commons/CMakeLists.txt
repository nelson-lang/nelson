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
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src/fmt/include
                    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
                    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/stduuid
                    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include)
# ==============================================================================
add_definitions(-DFMT_EXPORT)
# ==============================================================================
file(GLOB COMMONS_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp"
"${CMAKE_CURRENT_SOURCE_DIR}/src/fmt/src/*.cc")
# ==============================================================================
add_library(${module_library_name} SHARED ${COMMONS_SRC})
# ==============================================================================
target_link_libraries(${module_library_name} nlsCharacters_encoding)
# ==============================================================================
set_target_properties(
  ${module_library_name}
  PROPERTIES
    VERSION
    ${Nelson_VERSION_MAJOR}.${Nelson_VERSION_MINOR}.${Nelson_VERSION_MAINTENANCE}
    SOVERSION ${Nelson_VERSION_MAJOR})
# ==============================================================================
install(TARGETS ${module_library_name} LIBRARY DESTINATION ${LIBRARY_OUTPUT})
# ==============================================================================
