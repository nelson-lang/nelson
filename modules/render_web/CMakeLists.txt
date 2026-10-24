# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name render_web)
set(module_library_name nlsRenderWeb)
# ==============================================================================
# Qt-free web render backend: RenderWeb (display-list recorder), the software
# rasterizer and the PNG/SVG/PDF writers. Links NO Qt module. It only needs
# graphics headers (RenderInterface, GOPropertyValues — header-only) and the
# font_metrics service. Vendored stb headers provide embedded-image decoding and
# clipboard PNG encoding.
# ==============================================================================
nelson_module_sources_from_glob(
  RENDER_WEB_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
# ==============================================================================
add_library(${module_library_name} ${RENDER_WEB_SRC})
# ==============================================================================
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../font_metrics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
)
# ==============================================================================
target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsFontMetrics
    ${CMAKE_DL_LIBS}
)
# ==============================================================================
set_target_properties(${module_library_name}
  PROPERTIES
    VERSION ${Nelson_VERSION}
    SOVERSION ${Nelson_VERSION_MAJOR}
)
# ==============================================================================
nelson_install_library(${module_library_name})
