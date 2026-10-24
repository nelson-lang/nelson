# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name font_metrics)
set(module_library_name nlsFontMetrics)
# ==============================================================================
# Qt-free font engine (stb_truetype over platform TTFs, AFM tables fallback),
# tuned to track QFontMetrics. Shared ground below graphics and render_web:
# the drawing model measures text (Extent, tick labels) without Qt or a render
# context, and RenderWeb rasterizes glyphs from it. render_qt stays Qt-only,
# render_web stays web-only. Zero dependencies.
# ==============================================================================
nelson_module_sources_from_glob(
  FONT_METRICS_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
# ==============================================================================
add_library(${module_library_name} ${FONT_METRICS_SRC})
# ==============================================================================
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
)
# ==============================================================================
target_link_libraries(${module_library_name} PRIVATE ${CMAKE_DL_LIBS})
# ==============================================================================
set_target_properties(${module_library_name}
  PROPERTIES
    VERSION ${Nelson_VERSION}
    SOVERSION ${Nelson_VERSION_MAJOR}
)
# ==============================================================================
nelson_install_library(${module_library_name})
