# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name render_qt)
set(module_library_name nlsRenderQt)
# ==============================================================================
find_package(OpenGL REQUIRED)
# ==============================================================================
# Qt/OpenGL render backends: RenderQt (QPainter), RenderOpenGL, their BSP/patch
# helpers and the shared render helpers (symbol drawing, DPI/point conversion).
# Split out of graphics so the render backend is a module of its own, mirroring
# render_web. Needs only header-only bits of graphics (RenderInterface,
# GOPropertyValues, RenderQtHelpers) — no symbol back-dependency, so the link
# graph stays acyclic: graphics/graphics_io link nlsRenderQt, not the reverse.
# See modules/web_gui/ZEROQT.md.
# ==============================================================================
nelson_module_sources_from_glob(
  RENDER_QT_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
  EXCLUDE
  "moc_.*\\.cpp$"
)
# ==============================================================================
add_library(${module_library_name} ${RENDER_QT_SRC})
# ==============================================================================
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
)
# ==============================================================================
target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsCommons
    nlsNelson_manager
    nlsGui
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::OpenGL
    Qt6::OpenGLWidgets
    OpenGL::GL
    ${CMAKE_DL_LIBS}
)
# ==============================================================================
# RenderQt.cpp uses the commons parallel helpers (ParallelSort/ParallelTransform),
# whose std::execution::par algorithms are backed by TBB in libstdc++. Link TBB
# so the tbb::detail symbols resolve at load time.
if(NOT WITHOUT_TBB AND TBB_FOUND)
  target_link_libraries(${module_library_name} PRIVATE ${TBB_LIBRARY})
endif()
# ==============================================================================
set_target_properties(${module_library_name}
  PROPERTIES
    VERSION ${Nelson_VERSION}
    SOVERSION ${Nelson_VERSION_MAJOR}
)
# ==============================================================================
nelson_install_library(${module_library_name})
