# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name graphics_io)
set(module_library_name nlsGraphics_io)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
option(WITH_GRAPHICS_IO_QT_ADAPTER
  "Build the optional graphics_io desktop Qt adapter"
  ON
)
file(GLOB RASTER_CODEC_SRC
  CONFIGURE_DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp/Raster*.cpp
)
if(NELSON_WASM_BUILD)
  list(FILTER RASTER_CODEC_SRC
    INCLUDE
    REGEX "/Raster(AtomicFile|CodecPcx|CodecPnm|CodecRegistry|CodecStb|Image)\\.cpp$"
  )
endif()

add_library(nlsGraphics_io_codec STATIC ${RASTER_CODEC_SRC})
set_target_properties(nlsGraphics_io_codec
  PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    AUTOMOC OFF
    AUTOUIC OFF
    AUTORCC OFF
)
target_include_directories(nlsGraphics_io_codec
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/../render_web/src/include
)

if(NELSON_WASM_BUILD)
  target_compile_definitions(nlsGraphics_io_codec
    PRIVATE NELSON_GRAPHICS_IO_STB_ONLY
  )
else()
  find_package(PNG REQUIRED)
  find_package(JPEG REQUIRED)
  if(NOT GIF_INCLUDE_DIR OR NOT GIF_LIBRARIES)
    message(FATAL_ERROR "giflib is required by graphics_io")
  endif()
  if(WIN32)
    find_package(WebP CONFIG QUIET)
    if(TARGET WebP::webp)
      set(NELSON_WEBP_TARGET WebP::webp)
    else()
      find_path(WEBP_INCLUDE_DIR
        webp/decode.h
        HINTS
        ${NELSON_WINDOWS_THIRDPARTY_ROOT}/libwebp/include
        ${NELSON_WINDOWS_THIRDPARTY_ROOT}/include
      )
      find_library(WEBP_LIBRARY
        NAMES
        webp
        libwebp
        HINTS
        ${NELSON_WINDOWS_THIRDPARTY_ROOT}/libwebp/lib
        ${NELSON_WINDOWS_THIRDPARTY_ROOT}/lib
      )
      if(NOT WEBP_INCLUDE_DIR OR NOT WEBP_LIBRARY)
        message(FATAL_ERROR "libwebp is required by graphics_io")
      endif()
      add_library(nlsWebP UNKNOWN IMPORTED)
      set_target_properties(nlsWebP
        PROPERTIES
          IMPORTED_LOCATION ${WEBP_LIBRARY}
          INTERFACE_INCLUDE_DIRECTORIES ${WEBP_INCLUDE_DIR}
      )
      set(NELSON_WEBP_TARGET nlsWebP)
    endif()
  else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(WEBP REQUIRED IMPORTED_TARGET libwebp)
    set(NELSON_WEBP_TARGET PkgConfig::WEBP)
  endif()
  if(NOT TIFF_FOUND)
    find_package(TIFF REQUIRED)
  endif()
  target_include_directories(nlsGraphics_io_codec PRIVATE ${GIF_INCLUDE_DIR})
  target_link_libraries(nlsGraphics_io_codec
    PRIVATE
      PNG::PNG
      JPEG::JPEG
      ${GIF_LIBRARIES}
      ${NELSON_WEBP_TARGET}
  )
  if(TARGET TIFF::TIFF)
    target_link_libraries(nlsGraphics_io_codec PRIVATE TIFF::TIFF)
  else()
    target_include_directories(nlsGraphics_io_codec PRIVATE ${TIFF_INCLUDE_DIR})
    target_link_libraries(nlsGraphics_io_codec PRIVATE ${TIFF_LIBRARIES})
  endif()
endif()

nelson_module_sources_from_glob(
  GRAPHICS_IO_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
list(FILTER GRAPHICS_IO_SRC EXCLUDE REGEX "/Raster[^/]*\\.cpp$")
list(FILTER GRAPHICS_IO_SRC EXCLUDE REGEX "/src/qt/")

add_library(${module_library_name} ${GRAPHICS_IO_SRC})
set_target_properties(${module_library_name}
  PROPERTIES
    AUTOMOC OFF
    AUTOUIC OFF
    AUTORCC OFF
)
if(NELSON_WASM_BUILD)
  target_compile_definitions(${module_library_name}
    PRIVATE NELSON_GRAPHICS_IO_STB_ONLY
  )
endif()

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../render_web/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../elementary_functions/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../core/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../files_folders_functions/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsGraphics_io_codec
    nlsCharacters_encoding
    nlsCommons
    nlsElementary_functions
    nlsError_manager
    nlsGraphics
    nlsRenderWeb
    nlsI18n
    nlsNelson_manager
    nlsInterpreter
    nlsTypes
)

if(NOT WITHOUT_TBB AND TBB_FOUND)
  target_link_libraries(${module_library_name} PRIVATE ${TBB_LIBRARY})
endif()
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  GRAPHICS_IO_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${GRAPHICS_IO_BUILTIN})
set_target_properties(${module_library_builtin_name}
  PROPERTIES
    AUTOMOC OFF
    AUTOUIC OFF
    AUTORCC OFF
)

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../render_web/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../display_format/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsCharacters_encoding
    nlsCommons
    nlsError_manager
    nlsGraphics
    nlsRenderWeb
    nlsNelson_manager
    nlsI18n
    nlsInterpreter
    nlsTypes
)
# ==============================================================================
# Optional desktop adapter. The codec and public graphics_io libraries above
# remain GUI-toolkit-free; this target only supplies native dialogs/clipboard.
if(WITH_GRAPHICS_IO_QT_ADAPTER
  AND NOT WITHOUT_GUI_MODULE
  AND NOT WITHOUT_RENDER_QT_MODULE
  AND NOT WITHOUT_GRAPHICS_QT_MODULE
)
  nelson_module_sources_from_glob(
    GRAPHICS_IO_QT_SRC
    DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/src/qt
  )
  add_library(nlsGraphics_io_qt ${GRAPHICS_IO_QT_SRC})
  target_include_directories(nlsGraphics_io_qt
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../graphics_qt/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/fmt/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../gui/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../render_interface/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../render_qt/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
  )
  # graphics_io_qt only references GOWindow through its (weak) typeinfo, so the
  # GNU linker's default --as-needed drops the libnlsGraphics_qt DT_NEEDED entry
  # even though the symbol is required at load time. Force the dependency to be
  # recorded on Linux; Windows/macOS record it unconditionally.
  target_link_libraries(nlsGraphics_io_qt
    PRIVATE
      ${module_library_name}
      nlsCharacters_encoding
      $<$<PLATFORM_ID:Linux>:-Wl,--no-as-needed>
      nlsGraphics_qt
      $<$<PLATFORM_ID:Linux>:-Wl,--as-needed>
      nlsGui
      Qt6::Core
      Qt6::Gui
      Qt6::Widgets
  )
  # Pull the Qt figure backend into the builtin so its host-factory registers at
  # startup (GraphicsIoQtAdapterLoader's static ctor loads graphics_io_qt, which
  # links graphics_qt). Without this, no desktop figure window is ever created:
  # createFigureHost() finds a null factory and falls back to the headless host.
  target_link_libraries(${module_library_builtin_name}
    PRIVATE nlsGraphics_io_qt
  )
  target_sources(${module_library_builtin_name}
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/builtin/qt/GraphicsIoQtAdapterLoader.cpp
  )
  nelson_install_library(nlsGraphics_io_qt)
endif()
# ==============================================================================
if(NELSON_PORTABLE_BUILD)
  nelson_install_library(nlsGraphics_io_codec)
endif()
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
