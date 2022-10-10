# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name nelson_manager)
set(module_library_name nlsNelson_manager)
# ==============================================================================
include_directories(${Boost_INCLUDE_DIRS}
                    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include)
# ==============================================================================
file(GLOB NELSON_MANAGER_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/*.cpp")
# ==============================================================================
add_library(${module_library_name} SHARED ${NELSON_MANAGER_SRC})
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
