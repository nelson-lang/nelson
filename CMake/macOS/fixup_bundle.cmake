# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Post-install script for macOS .app bundle dependency fixup.
#
# This script is executed at CPack time via CPACK_POST_BUILD_SCRIPTS (CMake 3.19+)
# or can be run manually after `cmake --install`:
#
#   cmake -DAPP_BUNDLE_DIR=/path/to/Nelson.app -P fixup_bundle.cmake
#
# It performs:
#   1. Discovers all dylibs and executables in the bundle
#   2. Identifies third-party dependencies (outside /usr/lib, /System)
#   3. Copies them into Contents/Frameworks/
#   4. Rewrites load commands using install_name_tool
#   5. Runs macdeployqt for Qt frameworks/plugins (if available)
#   6. Optionally codesigns with ad-hoc signature
#
# Required variable:
#   APP_BUNDLE_DIR – absolute path to the .app bundle
# ==============================================================================
cmake_minimum_required(VERSION 3.16)

if(NOT APP_BUNDLE_DIR)
  message(FATAL_ERROR
    "APP_BUNDLE_DIR is not set. "
    "Pass -DAPP_BUNDLE_DIR=/path/to/Nelson.app"
  )
endif()
if(NOT IS_DIRECTORY "${APP_BUNDLE_DIR}")
  message(FATAL_ERROR "APP_BUNDLE_DIR does not exist: ${APP_BUNDLE_DIR}")
endif()

set(CONTENTS_DIR "${APP_BUNDLE_DIR}/Contents")
set(FRAMEWORKS_DIR "${CONTENTS_DIR}/Frameworks")
set(RESOURCES_DIR "${CONTENTS_DIR}/Resources")
set(NELSON_LIB_DIR "${RESOURCES_DIR}/lib/Nelson")

file(MAKE_DIRECTORY "${FRAMEWORKS_DIR}")

message(STATUS "=== Nelson macOS bundle fixup ===")
message(STATUS "Bundle: ${APP_BUNDLE_DIR}")

# ==============================================================================
# Step 1: Run macdeployqt if Qt is present
# ==============================================================================
# macdeployqt handles Qt frameworks, plugins, and QML imports.
# It must run BEFORE our general fixup because it creates the Frameworks/
# structure for Qt and we need to fix up the remaining non-Qt libraries.
find_program(MACDEPLOYQT
  macdeployqt
  HINTS
  "$ENV{QTDIR}/bin"
  "$ENV{HOMEBREW_PREFIX}/opt/qt/bin"
  "/opt/homebrew/opt/qt/bin"
  "/usr/local/opt/qt/bin"
)

if(MACDEPLOYQT)
  message(STATUS "Running macdeployqt: ${MACDEPLOYQT}")
  set(_bundle_launcher "${CONTENTS_DIR}/MacOS/nelson-gui")
  set(_real_gui_exec "${RESOURCES_DIR}/bin/nelson-gui-exec")
  set(_launcher_backup "")
  if(EXISTS "${_bundle_launcher}" AND EXISTS "${_real_gui_exec}")
    file(READ "${_bundle_launcher}" _launcher_magic HEX LIMIT 4)
    if(_launcher_magic MATCHES "^2321")
      set(_launcher_backup "${_bundle_launcher}.launcher")
      file(RENAME "${_bundle_launcher}" "${_launcher_backup}")
      file(COPY "${_real_gui_exec}" DESTINATION "${CONTENTS_DIR}/MacOS")
      file(RENAME "${CONTENTS_DIR}/MacOS/nelson-gui-exec" "${_bundle_launcher}")
    endif()
  endif()

  # Tell macdeployqt where to find QML source files so it discovers which
  # QML modules (QtQuick.Controls, QtQuick.Window, QtQuick.Layouts, …) need
  # to be bundled.  Without -qmldir, macdeployqt does NOT copy QML plugins.
  set(_qmldir_args "")
  set(_qml_scan_dir "${RESOURCES_DIR}/share/Nelson/modules/qml_engine")
  if(IS_DIRECTORY "${_qml_scan_dir}")
    list(APPEND _qmldir_args "-qmldir=${_qml_scan_dir}")
    message(STATUS "  macdeployqt -qmldir: ${_qml_scan_dir}")
  endif()

  execute_process(
    COMMAND "${MACDEPLOYQT}" "${APP_BUNDLE_DIR}" -verbose=1
      "-executable=${_real_gui_exec}" ${_qmldir_args}
    RESULT_VARIABLE _mqt_rc
    OUTPUT_VARIABLE _mqt_out
    ERROR_VARIABLE _mqt_err
    TIMEOUT 180
  )
  if(_launcher_backup)
    file(REMOVE "${_bundle_launcher}")
    file(RENAME "${_launcher_backup}" "${_bundle_launcher}")
  endif()
  if(NOT _mqt_rc STREQUAL "0")
    message(WARNING "macdeployqt returned ${_mqt_rc}: ${_mqt_err}")
  else()
    message(STATUS "macdeployqt completed successfully")
  endif()
else()
  message(STATUS
    "macdeployqt not found – skipping Qt framework bundling. "
    "Qt frameworks must be available on the target system or bundled manually."
  )
endif()

# ==============================================================================
# Step 1a: Write qt.conf next to the real executable
# ==============================================================================
# macdeployqt places qt.conf in Contents/Resources/, but Qt looks for it
# relative to applicationDirPath().  Since nelson-gui-exec lives in
# Contents/Resources/bin/ (not Contents/MacOS/), Qt does NOT detect the
# bundle layout and never reads Contents/Resources/qt.conf.
#
# We write a qt.conf next to the binary that sets Prefix = ../..  so that
# Qt's prefix resolves to Contents/ and all standard sub-directories
# (PlugIns, Frameworks, …) are found correctly.
set(_exe_bin_dir "${RESOURCES_DIR}/bin")
if(IS_DIRECTORY "${_exe_bin_dir}")
  file(WRITE "${_exe_bin_dir}/qt.conf"
    "[Paths]\nPrefix = ../..\nPlugins = PlugIns\nQml2Imports = Resources/qml\n"
  )
  message(STATUS "Wrote qt.conf in ${_exe_bin_dir}")
endif()

# ==============================================================================
# Step 1b: Bundle dlopen-loaded libraries (not found by otool -L)
# ==============================================================================
# FFTW is loaded at runtime via dlopen.  We must find and copy it explicitly.
set(_dlopen_lib_names
  "libfftw3.3.dylib"
  "libfftw3f.3.dylib"
  "libfftw3.dylib"
  "libfftw3f.dylib"
)

# Search paths for dlopen-loaded libraries
set(_dlopen_search_paths
  "$ENV{HOMEBREW_PREFIX}/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/pcre2/lib"
  "$ENV{CONDA_PREFIX}/lib"
  "/opt/homebrew/lib"
  "/opt/homebrew/opt/pcre2/lib"
  "/usr/local/lib"
  "/usr/local/opt/pcre2/lib"
)

foreach(_dlib IN LISTS _dlopen_lib_names)
  set(_found_dlib "")
  foreach(_sp IN LISTS _dlopen_search_paths)
    if(EXISTS "${_sp}/${_dlib}")
      set(_found_dlib "${_sp}/${_dlib}")
      break()
    endif()
  endforeach()
  # Also check if it was provided via a Nix store or similar
  if(NOT _found_dlib)
    execute_process(
      COMMAND find /nix/store -maxdepth 3 -name "${_dlib}" -type f
      OUTPUT_VARIABLE _nix_result
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
    )
    if(_nix_result)
      string(REPLACE "\n" ";" _nix_paths "${_nix_result}")
      list(GET _nix_paths 0 _found_dlib)
    endif()
  endif()
  if(_found_dlib AND NOT EXISTS "${FRAMEWORKS_DIR}/${_dlib}")
    message(STATUS "  Bundling dlopen-loaded: ${_dlib} from ${_found_dlib}")
    # Resolve symlinks – Homebrew uses symlinks extensively.
    # file(COPY) preserves symlinks, creating broken links in the bundle.
    get_filename_component(_real_dlib "${_found_dlib}" REALPATH)
    get_filename_component(_real_dlib_name "${_real_dlib}" NAME)
    file(
      COPY "${_real_dlib}"
      DESTINATION "${FRAMEWORKS_DIR}"
      FILE_PERMISSIONS
        OWNER_READ
        OWNER_WRITE
        GROUP_READ
        WORLD_READ
    )
    # Rename to the expected name if the real file differs
    if(NOT "${_real_dlib_name}" STREQUAL "${_dlib}")
      if(EXISTS "${FRAMEWORKS_DIR}/${_real_dlib_name}")
        file(RENAME
          "${FRAMEWORKS_DIR}/${_real_dlib_name}"
          "${FRAMEWORKS_DIR}/${_dlib}"
        )
      endif()
    endif()
    # Fix install name
    execute_process(
      COMMAND install_name_tool -id "@rpath/${_dlib}"
        "${FRAMEWORKS_DIR}/${_dlib}"
      ERROR_QUIET
    )
  endif()
endforeach()

# ==============================================================================
# Step 1c: Bundle Qt QML modules (fallback if macdeployqt missed them)
# ==============================================================================
# macdeployqt should have handled QML modules when given -qmldir, but some
# versions or configurations may miss modules.  Ensure the essential QML
# modules used by Nelson's qml_engine are present in the bundle.

set(_qml_dest_dir "${RESOURCES_DIR}/qml")

# Locate the Qt QML root directory
set(_qt_qml_root "")
foreach(_qml_search
  "$ENV{HOMEBREW_PREFIX}/share/qt/qml"
  "/opt/homebrew/share/qt/qml"
  "/usr/local/share/qt/qml"
  "$ENV{QTDIR}/qml"
  "$ENV{CONDA_PREFIX}/share/qt6/qml"
)
  if(_qml_search AND IS_DIRECTORY "${_qml_search}")
    set(_qt_qml_root "${_qml_search}")
    break()
  endif()
endforeach()

if(NOT _qt_qml_root)
  # Try to infer from macdeployqt location
  if(MACDEPLOYQT)
    get_filename_component(_mqt_bindir "${MACDEPLOYQT}" DIRECTORY)
    get_filename_component(_mqt_prefix "${_mqt_bindir}" DIRECTORY)
    if(IS_DIRECTORY "${_mqt_prefix}/share/qt/qml")
      set(_qt_qml_root "${_mqt_prefix}/share/qt/qml")
    elseif(IS_DIRECTORY "${_mqt_prefix}/qml")
      set(_qt_qml_root "${_mqt_prefix}/qml")
    endif()
  endif()
endif()

if(_qt_qml_root)
  message(STATUS "Qt QML root: ${_qt_qml_root}")
  if(DEFINED _mqt_rc
    AND NOT _mqt_rc STREQUAL "0"
    AND IS_DIRECTORY "${_qml_dest_dir}"
  )
    file(REMOVE_RECURSE "${_qml_dest_dir}")
  endif()
  find_program(RSYNC_EXECUTABLE rsync)

  # Modules required by Nelson's qml_engine
  set(_required_qml_modules
    "QtQuick"
    "QtQuick/Controls"
    "QtQuick/Controls/Basic"
    "QtQuick/Controls/macOS"
    "QtQuick/Controls/impl"
    "QtQuick/Layouts"
    "QtQuick/Templates"
    "QtQuick/Window"
    "QtQuick/Dialogs"
    "QtQuick/Particles"
    "QtQuick/Effects"
    "QtQuick/NativeStyle"
    "QtQml"
    "QtQml/WorkerScript"
  )

  foreach(_qmod IN LISTS _required_qml_modules)
    set(_src_mod "${_qt_qml_root}/${_qmod}")
    set(_dst_mod "${_qml_dest_dir}/${_qmod}")
    if(IS_DIRECTORY "${_src_mod}" AND NOT IS_DIRECTORY "${_dst_mod}")
      file(REAL_PATH "${_src_mod}" _src_mod_real)
      message(STATUS "  Bundling QML module: ${_qmod}")
      get_filename_component(_dst_parent "${_dst_mod}" DIRECTORY)
      if(EXISTS "${_dst_parent}" AND NOT IS_DIRECTORY "${_dst_parent}")
        message(WARNING
          "Skipping QML module ${_qmod}: parent is not a directory: ${_dst_parent}"
        )
      else()
        if(RSYNC_EXECUTABLE)
          file(MAKE_DIRECTORY "${_dst_mod}")
          execute_process(
            COMMAND "${RSYNC_EXECUTABLE}" -aL "${_src_mod_real}/" "${_dst_mod}/"
            RESULT_VARIABLE _rsync_rc
            ERROR_VARIABLE _rsync_err
          )
          if(NOT _rsync_rc EQUAL 0)
            message(FATAL_ERROR
              "Failed to bundle QML module ${_qmod}: ${_rsync_err}"
            )
          endif()
        else()
          file(MAKE_DIRECTORY "${_dst_parent}")
          file(
            COPY "${_src_mod_real}/"
            DESTINATION "${_dst_mod}"
            FOLLOW_SYMLINK_CHAIN
          )
        endif()
      endif()
    endif()
  endforeach()
else()
  message(WARNING
    "Qt QML root not found – QML modules not bundled. "
    "QtQuick.Controls, QtQuick.Window, etc. may be unavailable at runtime."
  )
endif()

# Also write a qt.conf in Contents/Resources/ (macdeployqt creates one here;
# this ensures it always has the correct QmlImports line).
file(WRITE "${RESOURCES_DIR}/qt.conf"
  "[Paths]\nPrefix = ..\nPlugins = PlugIns\nQml2Imports = Resources/qml\n"
)

# ==============================================================================
# Step 1d: Bundle Qt plugins (fallback if macdeployqt missed them)
# ==============================================================================
# Without the cocoa platform plugin, Qt aborts during QGuiApplication startup.
set(_qt_plugins_root "")
foreach(_plugin_search
  "$ENV{HOMEBREW_PREFIX}/share/qt/plugins"
  "$ENV{HOMEBREW_PREFIX}/opt/qt/share/qt/plugins"
  "$ENV{HOMEBREW_PREFIX}/opt/qtbase/share/qt/plugins"
  "/opt/homebrew/share/qt/plugins"
  "/opt/homebrew/opt/qt/share/qt/plugins"
  "/opt/homebrew/opt/qtbase/share/qt/plugins"
  "/opt/homebrew/Cellar/qtbase/6.11.1/share/qt/plugins"
  "/usr/local/share/qt/plugins"
  "/usr/local/opt/qt/share/qt/plugins"
  "/usr/local/opt/qtbase/share/qt/plugins"
  "$ENV{QTDIR}/plugins"
)
  if(_plugin_search AND IS_DIRECTORY "${_plugin_search}/platforms")
    set(_qt_plugins_root "${_plugin_search}")
    break()
  endif()
endforeach()

if(NOT _qt_plugins_root AND MACDEPLOYQT)
  get_filename_component(_mqt_bindir "${MACDEPLOYQT}" DIRECTORY)
  get_filename_component(_mqt_prefix "${_mqt_bindir}" DIRECTORY)
  if(IS_DIRECTORY "${_mqt_prefix}/share/qt/plugins/platforms")
    set(_qt_plugins_root "${_mqt_prefix}/share/qt/plugins")
  elseif(IS_DIRECTORY "${_mqt_prefix}/plugins/platforms")
    set(_qt_plugins_root "${_mqt_prefix}/plugins")
  endif()
endif()

if(_qt_plugins_root)
  message(STATUS "Qt plugins root: ${_qt_plugins_root}")
  set(_required_qt_plugins
    "platforms/libqcocoa.dylib"
    # The offscreen and minimal QPA plugins are not used by the normal GUI app,
    # but the test harness forces QT_QPA_PLATFORM=offscreen for every module run
    # (run_impl_module.m). Without these plugins bundled, every GUI/QML worker
    # aborts in QGuiApplicationPrivate::createPlatformIntegration() (exit 134),
    # which failed the whole qml_engine suite on installed builds while the
    # local build passed (it falls back to the Homebrew Qt plugins).
    "platforms/libqoffscreen.dylib"
    "platforms/libqminimal.dylib"
    "styles/libqmacstyle.dylib"
    "imageformats/libqgif.dylib"
    "imageformats/libqico.dylib"
    "imageformats/libqjpeg.dylib"
    "imageformats/libqsvg.dylib"
    "tls/libqcertonlybackend.dylib"
    "tls/libqsecuretransportbackend.dylib"
    "networkinformation/libqapplenetworkinformation.dylib"
    "generic/libqtuiotouchplugin.dylib"
  )

  foreach(_plugin_rel IN LISTS _required_qt_plugins)
    set(_plugin_src "${_qt_plugins_root}/${_plugin_rel}")
    set(_plugin_dst "${CONTENTS_DIR}/PlugIns/${_plugin_rel}")
    if(EXISTS "${_plugin_src}")
      get_filename_component(_plugin_parent "${_plugin_dst}" DIRECTORY)
      get_filename_component(_plugin_expected_name "${_plugin_dst}" NAME)
      get_filename_component(_plugin_real "${_plugin_src}" REALPATH)
      get_filename_component(_plugin_real_name "${_plugin_real}" NAME)
      file(MAKE_DIRECTORY "${_plugin_parent}")
      file(
        COPY "${_plugin_real}"
        DESTINATION "${_plugin_parent}"
        FILE_PERMISSIONS
          OWNER_READ
          OWNER_WRITE
          OWNER_EXECUTE
          GROUP_READ
          GROUP_EXECUTE
          WORLD_READ
          WORLD_EXECUTE
      )
      if(NOT "${_plugin_real_name}" STREQUAL "${_plugin_expected_name}")
        if(EXISTS "${_plugin_parent}/${_plugin_real_name}")
          file(RENAME
            "${_plugin_parent}/${_plugin_real_name}"
            "${_plugin_dst}"
          )
        endif()
      endif()
    else()
      message(STATUS "  Qt plugin not found, skipping: ${_plugin_rel}")
    endif()
  endforeach()
else()
  message(WARNING
    "Qt plugins root not found – Qt platform plugin may be unavailable at runtime."
  )
endif()

# ==============================================================================
# Step 1e: Resolve system library shadowing conflicts
# ==============================================================================
# macdeployqt may bundle Homebrew libraries whose filename matches a system
# library but whose ABI is INCOMPATIBLE.  The main case is GNU libiconv
# (Homebrew) vs Apple libiconv (/usr/lib/libiconv.2.dylib):
#
#   GNU libiconv exports  : _libiconv, _libiconv_open, _libiconv_close
#   Apple libiconv exports: _iconv,    _iconv_open,    _iconv_close
#
# When both are in the process image, dyld resolves ALL references to the
# bundled copy and libraries linked against the system version (e.g. libglib,
# libgit2, libintl) fail with "Symbol not found: _iconv".
#
# Fix: rename the Homebrew copy so its filename no longer shadows the system
# library.  Libraries that genuinely need the Homebrew version (e.g. libxml2)
# have their references updated to the new name.
#
set(_system_shadow_libs
  "libiconv.2.dylib"
  "libcharset.1.dylib"
) # companion shipped with Homebrew libiconv

foreach(_shadow IN LISTS _system_shadow_libs)
  set(_shadow_path "${FRAMEWORKS_DIR}/${_shadow}")
  if(NOT EXISTS "${_shadow_path}")
    continue()
  endif()

  # Build the new name:  lib<name>.dylib  →  libhb_<name>.dylib
  string(REGEX REPLACE "^lib" "libhb_" _new_name "${_shadow}")
  set(_new_path "${FRAMEWORKS_DIR}/${_new_name}")

  message(STATUS
    "Renaming ${_shadow} -> ${_new_name} (avoid shadowing /usr/lib/${_shadow})"
  )
  file(RENAME "${_shadow_path}" "${_new_path}")

  # Make writable and fix install name
  file(
    CHMOD "${_new_path}"
    PERMISSIONS
      OWNER_READ
      OWNER_WRITE
      GROUP_READ
      WORLD_READ
  )
  execute_process(
    COMMAND install_name_tool -id "@rpath/${_new_name}" "${_new_path}"
    ERROR_QUIET
  )

  # Update every Mach-O in Frameworks/ that references the old name
  file(GLOB _fw_all_shadow "${FRAMEWORKS_DIR}/*.dylib")
  # Also include Qt frameworks
  file(GLOB_RECURSE _fw_frameworks_shadow
    "${FRAMEWORKS_DIR}/*.framework/Versions/*/*"
  )
  list(APPEND _fw_all_shadow ${_fw_frameworks_shadow})
  foreach(_fw_s IN LISTS _fw_all_shadow)
    if(IS_SYMLINK "${_fw_s}")
      continue()
    endif()
    execute_process(
      COMMAND install_name_tool -change "@rpath/${_shadow}"
        "@rpath/${_new_name}" -change "@loader_path/${_shadow}"
        "@rpath/${_new_name}" -change
        "/opt/homebrew/opt/libiconv/lib/${_shadow}" "@rpath/${_new_name}"
        "${_fw_s}"
      ERROR_QUIET
    )
  endforeach()
endforeach()

# ==============================================================================
# Step 1f: Collect every QML plugin bundled under Resources/qml
# ==============================================================================
# macdeployqt copies the QML plugins Nelson imports into Resources/qml but does
# NOT bundle their Qt framework dependencies (QtQuickControls2, …Impl,
# QtQuickTemplates2, the per-style impls, QtQuickEffects, QtQuickParticles, …)
# nor point their @rpath at Frameworks/.  Loading such a module then fails at
# runtime, e.g. "Library not loaded: @rpath/QtQuickEffects.framework/..."
# (which broke test_qml_demo_basic_layouts via QtQuick.Controls' text context
# menu) or the equivalent QtQuickParticles failure that broke
# test_qml_demo_dynamicscene.
#
# Collect EVERY plugin dylib physically present under Resources/qml and feed it
# through the dependency scan below.  This is self-bounding: the set of modules
# in Resources/qml is already fixed by the explicit module list bundled in
# Step 1c plus whatever macdeployqt resolved from the demos' imports, so a
# recursive glob never drags in modules Nelson does not ship — it only closes
# the framework dependencies of the plugins that are actually there.  A
# hardcoded sub-list, by contrast, silently skipped Effects and Particles and
# left their frameworks out of the bundle.
set(_qml_scoped_plugins "")
file(GLOB_RECURSE _all_qml_plugin_dylibs "${RESOURCES_DIR}/qml/*.dylib")
foreach(_qpf IN LISTS _all_qml_plugin_dylibs)
  if(NOT IS_SYMLINK "${_qpf}")
    list(APPEND _qml_scoped_plugins "${_qpf}")
  endif()
endforeach()
list(REMOVE_DUPLICATES _qml_scoped_plugins)
list(LENGTH _qml_scoped_plugins _n_qml_scoped)
message(STATUS "QML plugins to fix up (framework closure): ${_n_qml_scoped}")

# ==============================================================================
# Step 2: Collect all Mach-O binaries in the bundle
# ==============================================================================
file(GLOB_RECURSE _all_files
  "${CONTENTS_DIR}/MacOS/*"
  "${CONTENTS_DIR}/Frameworks/*.dylib"
  "${CONTENTS_DIR}/Frameworks/*.framework/Versions/*/*"
  "${CONTENTS_DIR}/PlugIns/*/*.dylib"
  "${RESOURCES_DIR}/bin/*"
  "${RESOURCES_DIR}/lib/*.dylib"
  "${RESOURCES_DIR}/lib/Nelson/*.dylib"
)
# Feed the scoped QML plugins through the same dependency scan so their Qt
# framework dependencies get bundled.
list(APPEND _all_files ${_qml_scoped_plugins})

set(_macho_files "")
foreach(_f IN LISTS _all_files)
  if(IS_SYMLINK "${_f}")
    continue()
  endif()
  # Quick check: Mach-O files start with specific magic bytes
  execute_process(
    COMMAND file -b "${_f}"
    OUTPUT_VARIABLE _ftype
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  if(_ftype MATCHES "Mach-O")
    list(APPEND _macho_files "${_f}")
  endif()
endforeach()

list(LENGTH _macho_files _n_macho)
message(STATUS "Found ${_n_macho} Mach-O files in bundle")

# ==============================================================================
# Step 3: Discover third-party dependencies
# ==============================================================================
# System libraries that should NOT be bundled:
set(_system_prefixes
  "/usr/lib/"
  "/System/"
)

# Already-bundled paths:
set(_bundle_prefixes
  "@executable_path"
  "@loader_path"
  "@rpath"
)

function(_is_system_lib _path _result)
  set(_sys FALSE)
  foreach(_pfx IN LISTS _system_prefixes _bundle_prefixes)
    if("${_path}" MATCHES "^${_pfx}")
      set(_sys TRUE)
      break()
    endif()
  endforeach()
  set(${_result} ${_sys} PARENT_SCOPE)
endfunction()

function(_framework_name_from_path _path _result)
  set(_fw_name "")
  if("${_path}" MATCHES "(^|/)([^/]+\\.framework)/")
    set(_fw_name "${CMAKE_MATCH_2}")
  endif()
  set(${_result} "${_fw_name}" PARENT_SCOPE)
endfunction()

function(_framework_bundle_path _path _result)
  set(_fw_path "")
  if("${_path}" MATCHES "^(.+/[^/]+\\.framework)/")
    set(_fw_path "${CMAKE_MATCH_1}")
  endif()
  set(${_result} "${_fw_path}" PARENT_SCOPE)
endfunction()

if(NOT RSYNC_EXECUTABLE)
  find_program(RSYNC_EXECUTABLE rsync)
endif()

function(_copy_framework_to_bundle _framework)
  get_filename_component(_fw_name "${_framework}" NAME)
  set(_fw_dest "${FRAMEWORKS_DIR}/${_fw_name}")
  file(REAL_PATH "${_framework}" _fw_real)
  file(REMOVE_RECURSE "${_fw_dest}")

  if(RSYNC_EXECUTABLE)
    file(MAKE_DIRECTORY "${_fw_dest}")
    execute_process(
      COMMAND "${RSYNC_EXECUTABLE}" -a "${_fw_real}/" "${_fw_dest}/"
      RESULT_VARIABLE _fw_rsync_rc
      ERROR_VARIABLE _fw_rsync_err
    )
    if(NOT _fw_rsync_rc EQUAL 0)
      message(FATAL_ERROR
        "Failed to bundle framework ${_fw_name}: ${_fw_rsync_err}"
      )
    endif()
  else()
    file(MAKE_DIRECTORY "${_fw_dest}")
    file(COPY "${_fw_real}/" DESTINATION "${_fw_dest}" FOLLOW_SYMLINK_CHAIN)
  endif()
endfunction()

# ---- Build a set of library names already inside the bundle ----
# Libraries already installed under Resources/ (e.g. libnls*.dylib) must NOT
# be copied a second time into Frameworks/.  Collect their names so we can
# skip them in the dependency scan.
set(_bundled_lib_names "")
set(_bundled_framework_names "")
file(GLOB _existing_nlib "${NELSON_LIB_DIR}/*.dylib")
foreach(_el IN LISTS _existing_nlib)
  if(NOT IS_SYMLINK "${_el}")
    get_filename_component(_elname "${_el}" NAME)
    list(APPEND _bundled_lib_names "${_elname}")
  endif()
endforeach()
file(GLOB _existing_fw "${FRAMEWORKS_DIR}/*.dylib")
foreach(_el IN LISTS _existing_fw)
  if(NOT IS_SYMLINK "${_el}")
    get_filename_component(_elname "${_el}" NAME)
    list(APPEND _bundled_lib_names "${_elname}")
  endif()
endforeach()
file(GLOB _existing_frameworks "${FRAMEWORKS_DIR}/*.framework")
foreach(_efw IN LISTS _existing_frameworks)
  if(IS_DIRECTORY "${_efw}")
    get_filename_component(_efw_name "${_efw}" NAME)
    list(APPEND _bundled_framework_names "${_efw_name}")
  endif()
endforeach()
list(REMOVE_DUPLICATES _bundled_lib_names)
list(REMOVE_DUPLICATES _bundled_framework_names)
list(LENGTH _bundled_lib_names _n_existing)
message(STATUS "Libraries already in bundle: ${_n_existing}")

set(_deps_to_bundle "")
set(_frameworks_to_bundle "")

foreach(_bin IN LISTS _macho_files)
  execute_process(
    COMMAND otool -L "${_bin}"
    OUTPUT_VARIABLE _otool_out
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  # Parse each line: "	/path/to/lib.dylib (compatibility ...)"
  string(REPLACE "\n" ";" _lines "${_otool_out}")
  foreach(_line IN LISTS _lines)
    string(STRIP "${_line}" _line)
    if(_line MATCHES "^\t?(/[^ ]+\\.dylib)")
      set(_dep "${CMAKE_MATCH_1}")
      _is_system_lib("${_dep}" _is_sys)
      if(NOT _is_sys AND EXISTS "${_dep}")
        # Skip libraries that are already inside the bundle
        get_filename_component(_dep_check_name "${_dep}" NAME)
        list(FIND _bundled_lib_names "${_dep_check_name}" _already_idx)
        if(_already_idx EQUAL -1)
          list(APPEND _deps_to_bundle "${_dep}")
        endif()
      endif()
    elseif(_line MATCHES "^\t?(/[^ ]+\\.framework/[^ ]+)")
      set(_fw_dep "${CMAKE_MATCH_1}")
      _is_system_lib("${_fw_dep}" _fw_dep_sys)
      _framework_name_from_path("${_fw_dep}" _fw_name)
      _framework_bundle_path("${_fw_dep}" _fw_bundle)
      list(FIND _bundled_framework_names "${_fw_name}" _fw_already_idx)
      if(NOT _fw_dep_sys
        AND _fw_bundle
        AND IS_DIRECTORY "${_fw_bundle}"
        AND _fw_already_idx EQUAL -1
      )
        list(APPEND _frameworks_to_bundle "${_fw_bundle}")
      endif()
    endif()
  endforeach()
endforeach()

# Also scan already-bundled Frameworks/ and Qt plugin dylibs for deps that the
# initial scan above may have missed. In particular, ICU libraries reference
# libicudata via @loader_path and Qt plugins reference frameworks via @rpath.
#
# Build a set of known source directories from deps found so far,
# plus common Homebrew / system library paths, so we can resolve
# @loader_path and @rpath references from Frameworks/ libraries.
set(_known_lib_dirs "")
foreach(_d IN LISTS _deps_to_bundle)
  get_filename_component(_ddir "${_d}" DIRECTORY)
  list(APPEND _known_lib_dirs "${_ddir}")
endforeach()
foreach(_sp
  IN
  ITEMS
  "$ENV{HOMEBREW_PREFIX}/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/icu4c/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/icu4c@78/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/icu4c@77/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/icu4c@76/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/pcre2/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/qt/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/qtbase/lib"
  "$ENV{HOMEBREW_PREFIX}/opt/qtdeclarative/lib"
  "/opt/homebrew/lib"
  "/opt/homebrew/opt/icu4c/lib"
  "/opt/homebrew/opt/icu4c@78/lib"
  "/opt/homebrew/opt/icu4c@77/lib"
  "/opt/homebrew/opt/icu4c@76/lib"
  "/opt/homebrew/opt/pcre2/lib"
  "/opt/homebrew/opt/qt/lib"
  "/opt/homebrew/opt/qtbase/lib"
  "/opt/homebrew/opt/qtdeclarative/lib"
  "/usr/local/lib"
  "/usr/local/opt/icu4c/lib"
  "/usr/local/opt/icu4c@78/lib"
  "/usr/local/opt/icu4c@77/lib"
  "/usr/local/opt/icu4c@76/lib"
  "/usr/local/opt/pcre2/lib"
  "/usr/local/opt/qt/lib"
  "/usr/local/opt/qtbase/lib"
  "/usr/local/opt/qtdeclarative/lib"
  "$ENV{CONDA_PREFIX}/lib"
)
  if(_sp AND IS_DIRECTORY "${_sp}")
    list(APPEND _known_lib_dirs "${_sp}")
  endif()
endforeach()
file(GLOB _homebrew_icu_lib_dirs
  "/opt/homebrew/Cellar/icu4c*/[0-9]*/lib"
  "/usr/local/Cellar/icu4c*/[0-9]*/lib"
)
foreach(_icu_lib_dir IN LISTS _homebrew_icu_lib_dirs)
  if(IS_DIRECTORY "${_icu_lib_dir}")
    list(APPEND _known_lib_dirs "${_icu_lib_dir}")
  endif()
endforeach()
file(GLOB _extra_homebrew_lib_dirs
  "/opt/homebrew/Cellar/qt*/[0-9]*/lib"
  "/opt/homebrew/pkgs/*/lib"
  "/usr/local/Cellar/qt*/[0-9]*/lib"
  "/usr/local/pkgs/*/lib"
)
foreach(_extra_lib_dir IN LISTS _extra_homebrew_lib_dirs)
  if(IS_DIRECTORY "${_extra_lib_dir}")
    list(APPEND _known_lib_dirs "${_extra_lib_dir}")
  endif()
endforeach()
list(REMOVE_DUPLICATES _known_lib_dirs)

file(GLOB _fw_dylibs "${FRAMEWORKS_DIR}/*.dylib")
file(GLOB_RECURSE _qt_plugin_dylibs "${CONTENTS_DIR}/PlugIns/*.dylib")
list(APPEND _fw_dylibs ${_qt_plugin_dylibs})
# Scoped QML plugins: this loop (unlike the absolute-path-only main scan above)
# resolves their @rpath/*.framework references against the known Qt library
# dirs, so the Qt Quick Controls frameworks they need get bundled.
list(APPEND _fw_dylibs ${_qml_scoped_plugins})
foreach(_fwlib IN LISTS _fw_dylibs)
  if(IS_SYMLINK "${_fwlib}")
    continue()
  endif()
  execute_process(
    COMMAND otool -L "${_fwlib}"
    OUTPUT_VARIABLE _fw_otool
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  string(REPLACE "\n" ";" _fw_lines "${_fw_otool}")
  foreach(_fw_line IN LISTS _fw_lines)
    string(STRIP "${_fw_line}" _fw_line)
    # ---- Absolute paths ----
    if(_fw_line MATCHES "^\t?(/[^ ]+\\.dylib)")
      set(_fw_dep "${CMAKE_MATCH_1}")
      _is_system_lib("${_fw_dep}" _fw_sys)
      if(NOT _fw_sys AND EXISTS "${_fw_dep}")
        get_filename_component(_fw_dep_name "${_fw_dep}" NAME)
        list(FIND _bundled_lib_names "${_fw_dep_name}" _fw_abs_already_idx)
        if(NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_dep_name}"
          AND _fw_abs_already_idx EQUAL -1
        )
          list(APPEND _deps_to_bundle "${_fw_dep}")
        endif()
      endif()
      # ---- @loader_path references ----
    elseif(_fw_line MATCHES "^\t?(/[^ ]+\\.framework/[^ ]+)")
      set(_fw_dep "${CMAKE_MATCH_1}")
      _is_system_lib("${_fw_dep}" _fw_dep_sys)
      _framework_name_from_path("${_fw_dep}" _fw_name)
      _framework_bundle_path("${_fw_dep}" _fw_bundle)
      list(FIND _bundled_framework_names "${_fw_name}" _fw_already_idx)
      if(NOT _fw_dep_sys
        AND _fw_bundle
        AND IS_DIRECTORY "${_fw_bundle}"
        AND NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_name}"
        AND _fw_already_idx EQUAL -1
      )
        list(APPEND _frameworks_to_bundle "${_fw_bundle}")
      endif()
      # ---- @loader_path references ----
    elseif(_fw_line MATCHES "^\t?@loader_path/([^ ]+\\.dylib)")
      set(_lp_name "${CMAKE_MATCH_1}")
      list(FIND _bundled_lib_names "${_lp_name}" _fw_lp_already_idx)
      if(NOT EXISTS "${FRAMEWORKS_DIR}/${_lp_name}"
        AND _fw_lp_already_idx EQUAL -1
      )
        # Try the lib's install name to find its original directory
        execute_process(
          COMMAND otool -D "${_fwlib}"
          OUTPUT_VARIABLE _fwlib_id
          OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
        )
        string(REPLACE "\n" ";" _id_lines "${_fwlib_id}")
        set(_lp_found "")
        list(LENGTH _id_lines _id_len)
        if(_id_len GREATER 1)
          list(GET _id_lines 1 _install_name)
          string(STRIP "${_install_name}" _install_name)
          if(_install_name MATCHES "^/")
            get_filename_component(_orig_dir "${_install_name}" DIRECTORY)
            if(EXISTS "${_orig_dir}/${_lp_name}")
              set(_lp_found "${_orig_dir}/${_lp_name}")
            endif()
          endif()
        endif()
        # Fallback: search known library directories
        if(NOT _lp_found)
          foreach(_kdir IN LISTS _known_lib_dirs)
            if(EXISTS "${_kdir}/${_lp_name}")
              set(_lp_found "${_kdir}/${_lp_name}")
              break()
            endif()
          endforeach()
        endif()
        if(_lp_found)
          message(STATUS
            "  Found @loader_path dep in Frameworks/: ${_lp_name} -> ${_lp_found}"
          )
          list(APPEND _deps_to_bundle "${_lp_found}")
        endif()
      endif()
      # ---- @rpath references ----
    elseif(_fw_line MATCHES "^\t?@rpath/([^ ]+\\.dylib)")
      set(_rp_name "${CMAKE_MATCH_1}")
      list(FIND _bundled_lib_names "${_rp_name}" _fw_rp_already_idx)
      if(NOT EXISTS "${FRAMEWORKS_DIR}/${_rp_name}"
        AND _fw_rp_already_idx EQUAL -1
      )
        set(_rp_found "")
        foreach(_kdir IN LISTS _known_lib_dirs)
          if(EXISTS "${_kdir}/${_rp_name}")
            set(_rp_found "${_kdir}/${_rp_name}")
            break()
          endif()
        endforeach()
        if(_rp_found)
          message(STATUS
            "  Found @rpath dep in Frameworks/: ${_rp_name} -> ${_rp_found}"
          )
          list(APPEND _deps_to_bundle "${_rp_found}")
        endif()
      endif()
    elseif(_fw_line MATCHES "^\t?@rpath/([^ ]+\\.framework/[^ ]+)")
      set(_rp_fw_rel "${CMAKE_MATCH_1}")
      _framework_name_from_path("${_rp_fw_rel}" _rp_fw_name)
      if(_rp_fw_name AND NOT EXISTS "${FRAMEWORKS_DIR}/${_rp_fw_name}")
        set(_rp_fw_found "")
        foreach(_kdir IN LISTS _known_lib_dirs)
          if(IS_DIRECTORY "${_kdir}/${_rp_fw_name}")
            set(_rp_fw_found "${_kdir}/${_rp_fw_name}")
            break()
          endif()
        endforeach()
        if(_rp_fw_found)
          list(APPEND _frameworks_to_bundle "${_rp_fw_found}")
        endif()
      endif()
    endif()
  endforeach()
endforeach()

list(REMOVE_DUPLICATES _frameworks_to_bundle)
foreach(_framework IN LISTS _frameworks_to_bundle)
  get_filename_component(_fw_name "${_framework}" NAME)
  if(NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_name}")
    message(STATUS "  Bundling framework: ${_fw_name}")
    _copy_framework_to_bundle("${_framework}")
    list(APPEND _bundled_framework_names "${_fw_name}")
  endif()
endforeach()
list(REMOVE_DUPLICATES _bundled_framework_names)

set(_framework_scan_pass 0)
set(_framework_scan_changed TRUE)
while(_framework_scan_changed AND _framework_scan_pass LESS 5)
  math(EXPR _framework_scan_pass "${_framework_scan_pass} + 1")
  set(_framework_scan_changed FALSE)
  file(GLOB_RECURSE _bundled_framework_files
    "${FRAMEWORKS_DIR}/*.framework/Versions/*/*"
  )
  foreach(_fw_bin IN LISTS _bundled_framework_files)
    if(IS_SYMLINK "${_fw_bin}")
      continue()
    endif()
    execute_process(
      COMMAND file -b "${_fw_bin}"
      OUTPUT_VARIABLE _fw_bin_type
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
    )
    if(NOT _fw_bin_type MATCHES "Mach-O")
      continue()
    endif()

    execute_process(
      COMMAND otool -L "${_fw_bin}"
      OUTPUT_VARIABLE _fw_bin_otool
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
    )
    get_filename_component(_fw_bin_dir "${_fw_bin}" DIRECTORY)
    string(REPLACE "\n" ";" _fw_bin_lines "${_fw_bin_otool}")
    foreach(_fw_bin_line IN LISTS _fw_bin_lines)
      string(STRIP "${_fw_bin_line}" _fw_bin_line)
      if(_fw_bin_line MATCHES "^\t?(/[^ ]+\\.dylib)")
        set(_fw_bin_dep "${CMAKE_MATCH_1}")
        _is_system_lib("${_fw_bin_dep}" _fw_bin_sys)
        if(NOT _fw_bin_sys AND EXISTS "${_fw_bin_dep}")
          get_filename_component(_fw_bin_dep_name "${_fw_bin_dep}" NAME)
          list(FIND
            _bundled_lib_names
            "${_fw_bin_dep_name}"
            _fw_bin_already_idx
          )
          if(NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_bin_dep_name}"
            AND _fw_bin_already_idx EQUAL -1
          )
            list(APPEND _deps_to_bundle "${_fw_bin_dep}")
          endif()
        endif()
      elseif(_fw_bin_line MATCHES "^\t?(/[^ ]+\\.framework/[^ ]+)")
        set(_fw_bin_fw_dep "${CMAKE_MATCH_1}")
        _is_system_lib("${_fw_bin_fw_dep}" _fw_bin_fw_sys)
        _framework_name_from_path("${_fw_bin_fw_dep}" _fw_bin_fw_name)
        _framework_bundle_path("${_fw_bin_fw_dep}" _fw_bin_fw_bundle)
        if(NOT _fw_bin_fw_sys
          AND _fw_bin_fw_bundle
          AND IS_DIRECTORY "${_fw_bin_fw_bundle}"
          AND NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_bin_fw_name}"
        )
          message(STATUS "  Bundling framework: ${_fw_bin_fw_name}")
          _copy_framework_to_bundle("${_fw_bin_fw_bundle}")
          list(APPEND _bundled_framework_names "${_fw_bin_fw_name}")
          set(_framework_scan_changed TRUE)
        endif()
      elseif(_fw_bin_line MATCHES "^\t?@loader_path/([^ ]+\\.dylib)")
        set(_fw_bin_lp_rel "${CMAKE_MATCH_1}")
        get_filename_component(_fw_bin_lp_name "${_fw_bin_lp_rel}" NAME)
        list(FIND
          _bundled_lib_names
          "${_fw_bin_lp_name}"
          _fw_bin_lp_already_idx
        )
        if(NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_bin_lp_name}"
          AND _fw_bin_lp_already_idx EQUAL -1
        )
          set(_fw_bin_lp_found "")
          if(EXISTS "${_fw_bin_dir}/${_fw_bin_lp_rel}")
            set(_fw_bin_lp_found "${_fw_bin_dir}/${_fw_bin_lp_rel}")
          else()
            foreach(_kdir IN LISTS _known_lib_dirs)
              if(EXISTS "${_kdir}/${_fw_bin_lp_name}")
                set(_fw_bin_lp_found "${_kdir}/${_fw_bin_lp_name}")
                break()
              endif()
            endforeach()
          endif()
          if(_fw_bin_lp_found)
            message(STATUS
              "  Found @loader_path dep in bundled framework: ${_fw_bin_lp_name} -> ${_fw_bin_lp_found}"
            )
            list(APPEND _deps_to_bundle "${_fw_bin_lp_found}")
          endif()
        endif()
      elseif(_fw_bin_line MATCHES "^\t?@rpath/([^ ]+\\.dylib)")
        set(_fw_bin_rp_name "${CMAKE_MATCH_1}")
        list(FIND
          _bundled_lib_names
          "${_fw_bin_rp_name}"
          _fw_bin_rp_already_idx
        )
        if(NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_bin_rp_name}"
          AND _fw_bin_rp_already_idx EQUAL -1
        )
          set(_fw_bin_rp_found "")
          foreach(_kdir IN LISTS _known_lib_dirs)
            if(EXISTS "${_kdir}/${_fw_bin_rp_name}")
              set(_fw_bin_rp_found "${_kdir}/${_fw_bin_rp_name}")
              break()
            endif()
          endforeach()
          if(_fw_bin_rp_found)
            message(STATUS
              "  Found @rpath dep in bundled framework: ${_fw_bin_rp_name} -> ${_fw_bin_rp_found}"
            )
            list(APPEND _deps_to_bundle "${_fw_bin_rp_found}")
          endif()
        endif()
      elseif(_fw_bin_line MATCHES "^\t?@rpath/([^ ]+\\.framework/[^ ]+)")
        set(_fw_bin_rp_fw_rel "${CMAKE_MATCH_1}")
        _framework_name_from_path("${_fw_bin_rp_fw_rel}" _fw_bin_rp_fw_name)
        if(_fw_bin_rp_fw_name
          AND NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_bin_rp_fw_name}"
        )
          set(_fw_bin_rp_fw_found "")
          foreach(_kdir IN LISTS _known_lib_dirs)
            if(IS_DIRECTORY "${_kdir}/${_fw_bin_rp_fw_name}")
              set(_fw_bin_rp_fw_found "${_kdir}/${_fw_bin_rp_fw_name}")
              break()
            endif()
          endforeach()
          if(_fw_bin_rp_fw_found)
            message(STATUS "  Bundling framework: ${_fw_bin_rp_fw_name}")
            _copy_framework_to_bundle("${_fw_bin_rp_fw_found}")
            list(APPEND _bundled_framework_names "${_fw_bin_rp_fw_name}")
            set(_framework_scan_changed TRUE)
          endif()
        endif()
      endif()
    endforeach()
  endforeach()
endwhile()
list(REMOVE_DUPLICATES _bundled_framework_names)

list(REMOVE_DUPLICATES _deps_to_bundle)
list(LENGTH _deps_to_bundle _n_deps)
message(STATUS "Found ${_n_deps} third-party dylibs to bundle")

# ==============================================================================
# Step 4: Copy dependencies and rewrite load commands
# ==============================================================================
# We iterate until no new deps are discovered (transitive closure).
set(_max_iterations 10)
set(_iteration 0)
set(_all_bundled "")
set(_scanned_deps "")

while(_deps_to_bundle AND _iteration LESS _max_iterations)
  math(EXPR _iteration "${_iteration} + 1")
  message(STATUS "Fixup iteration ${_iteration}: ${_n_deps} libraries")

  set(_new_deps "")
  foreach(_dep IN LISTS _deps_to_bundle)
    get_filename_component(_depname "${_dep}" NAME)
    get_filename_component(_dep_srcdir "${_dep}" DIRECTORY)
    set(_dest "${FRAMEWORKS_DIR}/${_depname}")

    if(NOT EXISTS "${_dest}")
      # Remove stale broken symlinks from a previous run
      if(IS_SYMLINK "${_dest}")
        file(REMOVE "${_dest}")
      endif()

      message(STATUS "  Bundling: ${_depname}")

      # Resolve symlinks before copying – Homebrew uses symlinks extensively
      # (e.g., libicudata.77.dylib → libicudata.77.1.dylib).
      # CMake file(COPY) preserves symlinks, creating broken links in the bundle
      # because the symlink target doesn't exist in Frameworks/.
      get_filename_component(_dep_real "${_dep}" REALPATH)
      get_filename_component(_dep_real_name "${_dep_real}" NAME)

      file(
        COPY "${_dep_real}"
        DESTINATION "${FRAMEWORKS_DIR}"
        FILE_PERMISSIONS
          OWNER_READ
          OWNER_WRITE
          GROUP_READ
          WORLD_READ
      )

      # Rename to the expected name if the real file differs
      # (e.g., libicudata.77.1.dylib must be named libicudata.77.dylib)
      if(NOT "${_dep_real_name}" STREQUAL "${_depname}")
        if(EXISTS "${FRAMEWORKS_DIR}/${_dep_real_name}")
          file(RENAME "${FRAMEWORKS_DIR}/${_dep_real_name}" "${_dest}")
        endif()
      endif()

      # Make writable so install_name_tool can modify it
      file(
        CHMOD "${_dest}"
        PERMISSIONS
          OWNER_READ
          OWNER_WRITE
          GROUP_READ
          WORLD_READ
      )

      # Change the library's own install name
      execute_process(
        COMMAND install_name_tool -id "@rpath/${_depname}" "${_dest}"
        ERROR_QUIET
      )

      list(APPEND _all_bundled "${_dest}")
    endif()

    # Discover transitive deps – runs even if the library was already present
    # (e.g. copied by macdeployqt) so we catch @loader_path siblings like
    # libicudata that macdeployqt may have missed.
    list(FIND _scanned_deps "${_depname}" _scan_idx)
    if(EXISTS "${_dest}" AND _scan_idx EQUAL -1)
      list(APPEND _scanned_deps "${_depname}")
      execute_process(
        COMMAND otool -L "${_dest}"
        OUTPUT_VARIABLE _sub_out
        OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
      )
      string(REPLACE "\n" ";" _sub_lines "${_sub_out}")
      foreach(_sub_line IN LISTS _sub_lines)
        string(STRIP "${_sub_line}" _sub_line)
        # ---- Absolute paths ----
        if(_sub_line MATCHES "^\t?(/[^ ]+\\.dylib)")
          set(_sub_dep "${CMAKE_MATCH_1}")
          _is_system_lib("${_sub_dep}" _sub_sys)
          if(NOT _sub_sys AND EXISTS "${_sub_dep}")
            get_filename_component(_sub_name "${_sub_dep}" NAME)
            list(FIND _bundled_lib_names "${_sub_name}" _sub_already_idx)
            if(NOT EXISTS "${FRAMEWORKS_DIR}/${_sub_name}"
              AND _sub_already_idx EQUAL -1
            )
              list(APPEND _new_deps "${_sub_dep}")
            endif()
          endif()
          # ---- @loader_path references (resolve relative to original source dir) ----
        elseif(_sub_line MATCHES "^\t?(/[^ ]+\\.framework/[^ ]+)")
          set(_sub_fw_dep "${CMAKE_MATCH_1}")
          _is_system_lib("${_sub_fw_dep}" _sub_fw_sys)
          _framework_name_from_path("${_sub_fw_dep}" _sub_fw_name)
          _framework_bundle_path("${_sub_fw_dep}" _sub_fw_bundle)
          if(NOT _sub_fw_sys
            AND _sub_fw_bundle
            AND IS_DIRECTORY "${_sub_fw_bundle}"
            AND NOT EXISTS "${FRAMEWORKS_DIR}/${_sub_fw_name}"
          )
            message(STATUS "    Bundling framework: ${_sub_fw_name}")
            _copy_framework_to_bundle("${_sub_fw_bundle}")
            list(APPEND _bundled_framework_names "${_sub_fw_name}")
          endif()
          # ---- @loader_path references (resolve relative to original source dir) ----
        elseif(_sub_line MATCHES "^\t?@loader_path/([^ ]+\\.dylib)")
          set(_lp_name "${CMAKE_MATCH_1}")
          list(FIND _bundled_lib_names "${_lp_name}" _lp_already_idx)
          if(_lp_already_idx EQUAL -1)
            set(_lp_resolved "${_dep_srcdir}/${_lp_name}")
            # Follow symlinks to find the real file
            if(NOT EXISTS "${_lp_resolved}" AND IS_SYMLINK "${_lp_resolved}")
              get_filename_component(_lp_resolved "${_lp_resolved}" REALPATH)
            endif()
            if(EXISTS "${_lp_resolved}"
              AND NOT EXISTS "${FRAMEWORKS_DIR}/${_lp_name}"
            )
              message(STATUS
                "    Resolved @loader_path/${_lp_name} -> ${_lp_resolved}"
              )
              list(APPEND _new_deps "${_lp_resolved}")
            endif()
          endif()
          # ---- @rpath references (try source dir as fallback) ----
        elseif(_sub_line MATCHES "^\t?@rpath/([^ ]+\\.dylib)")
          set(_rp_name "${CMAKE_MATCH_1}")
          list(FIND _bundled_lib_names "${_rp_name}" _rp_already_idx)
          if(_rp_already_idx EQUAL -1
            AND NOT EXISTS "${FRAMEWORKS_DIR}/${_rp_name}"
          )
            if(EXISTS "${_dep_srcdir}/${_rp_name}")
              message(STATUS
                "    Resolved @rpath/${_rp_name} -> ${_dep_srcdir}/${_rp_name}"
              )
              list(APPEND _new_deps "${_dep_srcdir}/${_rp_name}")
            endif()
          endif()
        endif()
      endforeach()
    endif()
  endforeach()

  list(REMOVE_DUPLICATES _new_deps)
  set(_deps_to_bundle "${_new_deps}")
  list(LENGTH _deps_to_bundle _n_deps)
endwhile()

# ==============================================================================
# Step 5: Rewrite all references in all Mach-O files
# ==============================================================================
# Re-collect after copying
file(GLOB_RECURSE _all_macho_final
  "${CONTENTS_DIR}/MacOS/*"
  "${CONTENTS_DIR}/Frameworks/*.dylib"
  "${CONTENTS_DIR}/Frameworks/*.framework/Versions/*/*"
  "${CONTENTS_DIR}/PlugIns/*/*.dylib"
  "${RESOURCES_DIR}/bin/*"
  "${RESOURCES_DIR}/lib/*.dylib"
  "${RESOURCES_DIR}/lib/Nelson/*.dylib"
)
# Also rewrite the scoped QML plugins so their @rpath framework references
# resolve against Frameworks/ (an @loader_path rpath to Frameworks/ is added).
list(APPEND _all_macho_final ${_qml_scoped_plugins})

# Build a map of original paths → @rpath/name for all bundled libs
set(_rewrite_args "")
foreach(_bundled IN LISTS _all_bundled)
  get_filename_component(_bname "${_bundled}" NAME)
  # We'll fix references in a second pass
endforeach()

foreach(_bin IN LISTS _all_macho_final)
  if(IS_SYMLINK "${_bin}")
    continue()
  endif()
  execute_process(
    COMMAND file -b "${_bin}"
    OUTPUT_VARIABLE _ftype
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  if(NOT _ftype MATCHES "Mach-O")
    continue()
  endif()

  # Make writable
  file(
    CHMOD "${_bin}"
    PERMISSIONS
      OWNER_READ
      OWNER_WRITE
      OWNER_EXECUTE
      GROUP_READ
      GROUP_EXECUTE
      WORLD_READ
      WORLD_EXECUTE
  )

  # ---- Fix install name if it uses @executable_path ----
  # macdeployqt sets install names to @executable_path/../Frameworks/xxx
  # which only works for executables in Contents/MacOS/.
  # Replace with @rpath/xxx which is resolved via LC_RPATH entries.
  execute_process(
    COMMAND otool -D "${_bin}"
    OUTPUT_VARIABLE _id_output
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  string(REPLACE "\n" ";" _id_lines "${_id_output}")
  list(LENGTH _id_lines _id_len)
  if(_id_len GREATER 1)
    list(GET _id_lines 1 _current_id)
    string(STRIP "${_current_id}" _current_id)
    if(_current_id MATCHES "^@executable_path/\\.\\./Frameworks/(.+)")
      set(_new_id "@rpath/${CMAKE_MATCH_1}")
      execute_process(
        COMMAND install_name_tool -id "${_new_id}" "${_bin}"
        ERROR_QUIET
      )
    elseif(_current_id MATCHES "^[^@].*\\.framework/(Versions/[^ ]+)")
      set(_framework_rel_path "${CMAKE_MATCH_1}")
      _framework_name_from_path("${_current_id}" _fw_name)
      if(_fw_name AND EXISTS "${FRAMEWORKS_DIR}/${_fw_name}")
        execute_process(
          COMMAND install_name_tool -id
            "@rpath/${_fw_name}/${_framework_rel_path}" "${_bin}"
          ERROR_QUIET
        )
      endif()
    elseif(_current_id MATCHES "^/[^ ]+/([^ /]+\\.dylib)")
      set(_id_lib_name "${CMAKE_MATCH_1}")
      if(EXISTS "${FRAMEWORKS_DIR}/${_id_lib_name}")
        execute_process(
          COMMAND install_name_tool -id "@rpath/${_id_lib_name}" "${_bin}"
          ERROR_QUIET
        )
      elseif(EXISTS "${NELSON_LIB_DIR}/${_id_lib_name}")
        execute_process(
          COMMAND install_name_tool -id "@rpath/${_id_lib_name}" "${_bin}"
          ERROR_QUIET
        )
      endif()
    endif()
  endif()

  # ---- Remove old absolute rpaths that won't exist on another machine ----
  execute_process(
    COMMAND otool -l "${_bin}"
    OUTPUT_VARIABLE _otool_lc
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  string(REPLACE "\n" ";" _lc_lines "${_otool_lc}")
  set(_in_rpath FALSE)
  set(_seen_rpaths "")
  foreach(_lc_line IN LISTS _lc_lines)
    string(STRIP "${_lc_line}" _lc_line)
    if(_lc_line MATCHES "cmd LC_RPATH")
      set(_in_rpath TRUE)
    elseif(_in_rpath AND _lc_line MATCHES "path (.+) \\(offset")
      set(_old_rpath "${CMAKE_MATCH_1}")
      set(_normalized_rpath "${_old_rpath}")
      string(REGEX REPLACE "/+$" "" _normalized_rpath "${_normalized_rpath}")
      # Remove rpaths that are absolute and do NOT start with @
      if(_old_rpath MATCHES "^/" AND NOT _old_rpath MATCHES "^@")
        execute_process(
          COMMAND install_name_tool -delete_rpath "${_old_rpath}" "${_bin}"
          ERROR_QUIET
        )
        message(STATUS "  Removed old rpath: ${_old_rpath} from ${_bin}")
      elseif(_normalized_rpath IN_LIST _seen_rpaths)
        execute_process(
          COMMAND install_name_tool -delete_rpath "${_old_rpath}" "${_bin}"
          ERROR_QUIET
        )
        message(STATUS "  Removed duplicate rpath: ${_old_rpath} from ${_bin}")
      else()
        list(APPEND _seen_rpaths "${_normalized_rpath}")
      endif()
      set(_in_rpath FALSE)
    endif()
  endforeach()

  # ---- Add @rpath pointing to Frameworks/ ----
  get_filename_component(_bin_dir "${_bin}" DIRECTORY)
  file(RELATIVE_PATH _rel_to_fw "${_bin_dir}" "${FRAMEWORKS_DIR}")
  if(_rel_to_fw STREQUAL "")
    set(_fw_rpath "@loader_path")
  else()
    set(_fw_rpath "@loader_path/${_rel_to_fw}")
  endif()
  execute_process(
    COMMAND install_name_tool -add_rpath "${_fw_rpath}" "${_bin}"
    ERROR_QUIET
  ) # Ignore if already exists

  # ---- Add @rpath pointing to Nelson lib dir (lib/Nelson/) ----
  # So that @rpath/libnlsXxx.dylib references between Nelson libraries resolve.
  file(RELATIVE_PATH _rel_to_nlib "${_bin_dir}" "${NELSON_LIB_DIR}")
  if(_rel_to_nlib STREQUAL "" OR _rel_to_nlib STREQUAL ".")
    set(_nlib_rpath "@loader_path")
  else()
    set(_nlib_rpath "@loader_path/${_rel_to_nlib}")
  endif()
  execute_process(
    COMMAND install_name_tool -add_rpath "${_nlib_rpath}" "${_bin}"
    ERROR_QUIET
  ) # Ignore if already exists

  # ---- Rewrite all third-party references to @rpath/ ----
  execute_process(
    COMMAND otool -L "${_bin}"
    OUTPUT_VARIABLE _refs
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  string(REPLACE "\n" ";" _ref_lines "${_refs}")
  foreach(_ref_line IN LISTS _ref_lines)
    string(STRIP "${_ref_line}" _ref_line)
    # Absolute paths → @rpath/
    if(_ref_line MATCHES "^\t?(/[^ ]+/([^ /]+\\.dylib))")
      set(_old_path "${CMAKE_MATCH_1}")
      set(_lib_name "${CMAKE_MATCH_2}")
      _is_system_lib("${_old_path}" _ref_sys)
      if(NOT _ref_sys)
        if(EXISTS "${FRAMEWORKS_DIR}/${_lib_name}"
          OR EXISTS "${NELSON_LIB_DIR}/${_lib_name}"
        )
          execute_process(
            COMMAND install_name_tool -change "${_old_path}"
              "@rpath/${_lib_name}" "${_bin}"
            ERROR_QUIET
          )
        endif()
      endif()
      # @loader_path/name.dylib → @rpath/name.dylib (when lib is in Frameworks/)
    elseif(_ref_line MATCHES "^\t?(/[^ ]+\\.framework/(Versions/[^ ]+))")
      set(_old_path "${CMAKE_MATCH_1}")
      set(_framework_rel_path "${CMAKE_MATCH_2}")
      _framework_name_from_path("${_old_path}" _fw_name)
      _is_system_lib("${_old_path}" _ref_sys)
      if(NOT _ref_sys AND EXISTS "${FRAMEWORKS_DIR}/${_fw_name}")
        execute_process(
          COMMAND install_name_tool -change "${_old_path}"
            "@rpath/${_fw_name}/${_framework_rel_path}" "${_bin}"
          ERROR_QUIET
        )
      endif()
    elseif(_ref_line MATCHES "^\t?(@loader_path/([^ ]+\\.dylib))")
      set(_old_path "${CMAKE_MATCH_1}")
      get_filename_component(_lib_name "${CMAKE_MATCH_2}" NAME)
      if(EXISTS "${FRAMEWORKS_DIR}/${_lib_name}")
        execute_process(
          COMMAND install_name_tool -change "${_old_path}" "@rpath/${_lib_name}"
            "${_bin}"
          ERROR_QUIET
        )
      endif()
      # @executable_path/../Frameworks/name.dylib → @rpath/name.dylib
      # @executable_path/../Frameworks/Foo.framework/... → @rpath/Foo.framework/...
      # macdeployqt writes these, but they only work when the exe is in
      # Contents/MacOS/.  Nelson executables live in Contents/Resources/bin/
      # so we must use @rpath instead (rpaths are already set above).
    elseif(_ref_line MATCHES
      "^\t?(@loader_path/([^ ]+\\.framework/(Versions/[^ ]+)))"
    )
      set(_old_path "${CMAKE_MATCH_1}")
      set(_rel_path "${CMAKE_MATCH_2}")
      string(REGEX REPLACE "^\\.\\./Frameworks/" "" _rel_path "${_rel_path}")
      if(EXISTS "${FRAMEWORKS_DIR}/${_rel_path}")
        execute_process(
          COMMAND install_name_tool -change "${_old_path}" "@rpath/${_rel_path}"
            "${_bin}"
          ERROR_QUIET
        )
      endif()
    elseif(_ref_line MATCHES "^\t?(@executable_path/\\.\\./Frameworks/([^ ]+))")
      set(_old_path "${CMAKE_MATCH_1}")
      set(_rel_path
        "${CMAKE_MATCH_2}"
      ) # e.g. libfoo.dylib or QtCore.framework/Versions/A/QtCore
      if(EXISTS "${FRAMEWORKS_DIR}/${_rel_path}")
        execute_process(
          COMMAND install_name_tool -change "${_old_path}" "@rpath/${_rel_path}"
            "${_bin}"
          ERROR_QUIET
        )
      endif()
    endif()
  endforeach()
endforeach()

# ==============================================================================
# Step 6: Validate bundled library references
# ==============================================================================
set(_unresolved_refs "")
foreach(_bin IN LISTS _all_macho_final)
  if(IS_SYMLINK "${_bin}")
    continue()
  endif()
  execute_process(
    COMMAND file -b "${_bin}"
    OUTPUT_VARIABLE _ftype
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  if(NOT _ftype MATCHES "Mach-O")
    continue()
  endif()

  get_filename_component(_bin_dir "${_bin}" DIRECTORY)
  execute_process(
    COMMAND otool -L "${_bin}"
    OUTPUT_VARIABLE _refs
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
  )
  string(REPLACE "\n" ";" _ref_lines "${_refs}")
  foreach(_ref_line IN LISTS _ref_lines)
    string(STRIP "${_ref_line}" _ref_line)
    if(_ref_line MATCHES ":$")
      continue()
    endif()
    set(_missing_ref "")
    set(_missing_reason "")

    if(_ref_line MATCHES "^\t?(/[^ ]+\\.(dylib|framework/[^ ]+))")
      set(_abs_ref "${CMAKE_MATCH_1}")
      _is_system_lib("${_abs_ref}" _abs_sys)
      if(NOT _abs_sys)
        string(FIND "${_abs_ref}" "${APP_BUNDLE_DIR}" _in_bundle_pos)
        if(NOT EXISTS "${_abs_ref}")
          set(_missing_ref "${_abs_ref}")
          set(_missing_reason "missing absolute reference")
        elseif(NOT _in_bundle_pos EQUAL 0)
          set(_missing_ref "${_abs_ref}")
          set(_missing_reason "external absolute reference")
        endif()
      endif()
    elseif(_ref_line MATCHES "^\t?@rpath/([^ ]+\\.(dylib|framework/[^ ]+))")
      set(_rp_rel "${CMAKE_MATCH_1}")
      if(NOT EXISTS "${FRAMEWORKS_DIR}/${_rp_rel}"
        AND NOT EXISTS "${NELSON_LIB_DIR}/${_rp_rel}"
        AND NOT EXISTS "${RESOURCES_DIR}/lib/${_rp_rel}"
      )
        set(_missing_ref "@rpath/${_rp_rel}")
        set(_missing_reason "not found in bundled rpaths")
      endif()
    elseif(_ref_line MATCHES
      "^\t?@loader_path/([^ ]+\\.(dylib|framework/[^ ]+))"
    )
      set(_lp_rel "${CMAKE_MATCH_1}")
      if(NOT EXISTS "${_bin_dir}/${_lp_rel}")
        set(_missing_ref "@loader_path/${_lp_rel}")
        set(_missing_reason "not found relative to loader")
      endif()
    elseif(_ref_line MATCHES
      "^\t?@executable_path/([^ ]+\\.(dylib|framework/[^ ]+))"
    )
      set(_ep_rel "${CMAKE_MATCH_1}")
      if(NOT EXISTS "${CONTENTS_DIR}/MacOS/${_ep_rel}")
        set(_missing_ref "@executable_path/${_ep_rel}")
        set(_missing_reason "not found relative to executable path")
      endif()
    endif()

    if(_missing_ref)
      list(APPEND _unresolved_refs
        "${_bin}: ${_missing_ref} (${_missing_reason})"
      )
    endif()
  endforeach()
endforeach()

if(_unresolved_refs)
  list(REMOVE_DUPLICATES _unresolved_refs)
  string(REPLACE ";" "\n  " _unresolved_msg "${_unresolved_refs}")
  message(FATAL_ERROR
    "Unresolved non-system library references remain:\n  ${_unresolved_msg}"
  )
endif()
message(STATUS "All bundled non-system library references resolve")

# ==============================================================================
# Step 7: Ad-hoc codesign (required on Apple Silicon)
# ==============================================================================
# The fixup passes above rewrite load commands with install_name_tool, which
# invalidates every Mach-O's existing (linker) signature.  Everything must be
# re-signed AFTER those rewrites.
#
# `codesign --deep` on the outer bundle re-signs bare Mach-O helpers in
# Resources/bin/ and the dylibs in Frameworks/, but it does NOT descend into a
# nested .app that lives under Resources/ (e.g. Nelson WebView.app): codesign
# treats such a bundle as a sealed *resource*, not as nested code.  That left
# nelson-webview-exec carrying a stale linker-signed signature, so it was
# killed at launch on Apple Silicon (EXC_BAD_ACCESS / CODESIGNING Invalid Page).
#
# Fix: sign inside-out — sign any nested .app bundles FIRST (so their
# executables get a fresh, valid signature matching the rewritten bytes), then
# sign the outer bundle, which seals the freshly-signed nested apps.
if(EXISTS "/usr/bin/codesign")
  set(CODESIGN "/usr/bin/codesign")
else()
  find_program(CODESIGN codesign)
endif()
if(CODESIGN)
  # ---- Sign nested .app bundles first (inside-out) ----
  # Discover nested apps at any depth via their Contents/Info.plist, then keep
  # only paths that resolve to a *.app bundle (this excludes framework
  # Versions/*/Resources/Info.plist files) and exclude the top-level bundle.
  # Note: a `*.app` intermediate wildcard is NOT matched by GLOB_RECURSE, so we
  # glob for the plist and filter afterwards.
  file(GLOB_RECURSE _nested_app_plists "${CONTENTS_DIR}/*/Info.plist")
  set(_nested_apps "")
  foreach(_napp_plist IN LISTS _nested_app_plists)
    string(REGEX REPLACE "/Contents/Info\\.plist$" "" _napp "${_napp_plist}")
    if(_napp MATCHES "\\.app$" AND NOT _napp STREQUAL "${APP_BUNDLE_DIR}")
      list(APPEND _nested_apps "${_napp}")
    endif()
  endforeach()
  list(REMOVE_DUPLICATES _nested_apps)

  # ---- Make nested GUI .app bundles self-sufficient for the Qt platform plugin ----
  # A nested app (e.g. "Nelson WebView.app") can be launched directly by launchd
  # (double-click / Finder), which bypasses the CLI launcher that would set
  # QT_QPA_PLATFORM_PLUGIN_PATH. Qt then searches only the nested bundle's own
  # Contents/PlugIns (empty) and aborts in createPlatformIntegration() with
  # "Could not find the Qt platform plugin 'cocoa'". Copy the platform plugins
  # into each nested app's PlugIns/platforms and add an rpath so they resolve the
  # Qt frameworks from the outer bundle's Frameworks. This runs before the nested
  # apps are signed below, so the seeded plugins get sealed with the bundle.
  set(_main_platforms "${CONTENTS_DIR}/PlugIns/platforms")
  if(IS_DIRECTORY "${_main_platforms}")
    file(GLOB _seed_platform_plugins "${_main_platforms}/*.dylib")
    foreach(_napp IN LISTS _nested_apps)
      set(_napp_platforms "${_napp}/Contents/PlugIns/platforms")
      file(MAKE_DIRECTORY "${_napp_platforms}")
      # Relative depth from the nested platforms dir back to the outer Contents,
      # so the added rpath (@loader_path/../...) reaches Contents/Frameworks.
      file(RELATIVE_PATH _rel "${CONTENTS_DIR}" "${_napp_platforms}")
      string(REGEX REPLACE "/+" ";" _rel_parts "${_rel}")
      list(LENGTH _rel_parts _depth)
      set(_up "")
      foreach(_i RANGE 1 ${_depth})
        set(_up "${_up}../")
      endforeach()
      set(_fw_rpath "@loader_path/${_up}Frameworks")
      foreach(_pp IN LISTS _seed_platform_plugins)
        get_filename_component(_pp_name "${_pp}" NAME)
        file(COPY "${_pp}" DESTINATION "${_napp_platforms}")
        execute_process(
          COMMAND install_name_tool -add_rpath "${_fw_rpath}"
            "${_napp_platforms}/${_pp_name}"
          RESULT_VARIABLE _seed_rp_rc
          ERROR_VARIABLE _seed_rp_err
          OUTPUT_QUIET
        )
        # A duplicate-rpath error is harmless (idempotent re-runs); ignore it.
      endforeach()
      message(STATUS
        "Seeded platform plugins into nested app: ${_napp} (rpath ${_fw_rpath})"
      )
    endforeach()
  endif()

  foreach(_napp IN LISTS _nested_apps)
    message(STATUS "Ad-hoc codesigning nested app: ${_napp}")
    execute_process(
      COMMAND "${CODESIGN}" --force --deep --sign - "${_napp}"
      RESULT_VARIABLE _ncs_rc
      ERROR_VARIABLE _ncs_err
    )
    if(NOT _ncs_rc EQUAL 0)
      message(WARNING "codesign failed for nested app ${_napp}: ${_ncs_err}")
    endif()
  endforeach()

  # ---- Re-sign the scoped QML plugins ----
  # Their load commands were rewritten above (install_name_tool), which
  # invalidates the ad-hoc signature macdeployqt gave them.  They live under
  # Resources/qml, which `codesign --deep` on the outer bundle does NOT reach
  # (same reason as the nested app), so re-sign each one explicitly.
  foreach(_qp IN LISTS _qml_scoped_plugins)
    if(EXISTS "${_qp}")
      execute_process(
        COMMAND "${CODESIGN}" --force --sign - "${_qp}"
        RESULT_VARIABLE _qcs_rc
        ERROR_VARIABLE _qcs_err
      )
      if(NOT _qcs_rc EQUAL 0)
        message(WARNING "codesign failed for QML plugin ${_qp}: ${_qcs_err}")
      endif()
    endif()
  endforeach()

  # ---- Sign the outer bundle last ----
  message(STATUS "Ad-hoc codesigning the bundle...")
  execute_process(
    COMMAND "${CODESIGN}" --force --deep --sign - "${APP_BUNDLE_DIR}"
    RESULT_VARIABLE _cs_rc
    ERROR_VARIABLE _cs_err
  )
  if(NOT _cs_rc EQUAL 0)
    message(WARNING "codesign failed: ${_cs_err}")
  else()
    message(STATUS "Codesigning completed")
  endif()

  # ---- Verify nested app executables are valid (fail fast on regression) ----
  foreach(_napp IN LISTS _nested_apps)
    execute_process(
      COMMAND "${CODESIGN}" --verify --verbose=2 "${_napp}"
      RESULT_VARIABLE _vrc
      ERROR_VARIABLE _verr
    )
    if(NOT _vrc EQUAL 0)
      message(FATAL_ERROR
        "Nested app signature invalid after fixup: ${_napp}\n${_verr}"
      )
    endif()
  endforeach()
else()
  message(WARNING "codesign not found – bundle may not run on Apple Silicon")
endif()

# ==============================================================================
# Step 8: Summary
# ==============================================================================
list(LENGTH _all_bundled _total_bundled)
message(STATUS "=== Fixup complete ===")
message(STATUS
  "Bundled ${_total_bundled} third-party libraries into Frameworks/"
)
message(STATUS "Bundle is ready for DMG packaging")
