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
  message(FATAL_ERROR "APP_BUNDLE_DIR is not set. "
    "Pass -DAPP_BUNDLE_DIR=/path/to/Nelson.app")
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
find_program(MACDEPLOYQT macdeployqt
  HINTS
    "$ENV{QTDIR}/bin"
    "$ENV{HOMEBREW_PREFIX}/opt/qt/bin"
    "/opt/homebrew/opt/qt/bin"
    "/usr/local/opt/qt/bin")

if(MACDEPLOYQT)
  message(STATUS "Running macdeployqt: ${MACDEPLOYQT}")
  execute_process(
    COMMAND "${MACDEPLOYQT}" "${APP_BUNDLE_DIR}" -verbose=1
    RESULT_VARIABLE _mqt_rc
    OUTPUT_VARIABLE _mqt_out
    ERROR_VARIABLE _mqt_err)
  if(NOT _mqt_rc EQUAL 0)
    message(WARNING "macdeployqt returned ${_mqt_rc}: ${_mqt_err}")
  else()
    message(STATUS "macdeployqt completed successfully")
  endif()
else()
  message(STATUS "macdeployqt not found – skipping Qt framework bundling. "
    "Qt frameworks must be available on the target system or bundled manually.")
endif()

# ==============================================================================
# Step 1b: Bundle dlopen-loaded libraries (not found by otool -L)
# ==============================================================================
# FFTW is loaded at runtime via dlopen.  We must find and copy it explicitly.
set(_dlopen_lib_names
  "libfftw3.3.dylib"
  "libfftw3f.3.dylib"
  "libfftw3.dylib"
  "libfftw3f.dylib")

# Search paths for dlopen-loaded libraries
set(_dlopen_search_paths
  "$ENV{HOMEBREW_PREFIX}/lib"
  "$ENV{CONDA_PREFIX}/lib"
  "/opt/homebrew/lib"
  "/usr/local/lib")

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
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET)
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
    file(COPY "${_real_dlib}" DESTINATION "${FRAMEWORKS_DIR}"
      FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
    # Rename to the expected name if the real file differs
    if(NOT "${_real_dlib_name}" STREQUAL "${_dlib}")
      if(EXISTS "${FRAMEWORKS_DIR}/${_real_dlib_name}")
        file(RENAME "${FRAMEWORKS_DIR}/${_real_dlib_name}" "${FRAMEWORKS_DIR}/${_dlib}")
      endif()
    endif()
    # Fix install name
    execute_process(
      COMMAND install_name_tool -id "@rpath/${_dlib}" "${FRAMEWORKS_DIR}/${_dlib}"
      ERROR_QUIET)
  endif()
endforeach()

# ==============================================================================
# Step 2: Collect all Mach-O binaries in the bundle
# ==============================================================================
file(GLOB_RECURSE _all_files
  "${CONTENTS_DIR}/MacOS/*"
  "${CONTENTS_DIR}/Frameworks/*.dylib"
  "${CONTENTS_DIR}/Frameworks/*.framework/Versions/*/lib*"
  "${RESOURCES_DIR}/bin/*"
  "${RESOURCES_DIR}/lib/*.dylib"
  "${RESOURCES_DIR}/lib/Nelson/*.dylib")

set(_macho_files "")
foreach(_f IN LISTS _all_files)
  if(IS_SYMLINK "${_f}")
    continue()
  endif()
  # Quick check: Mach-O files start with specific magic bytes
  execute_process(
    COMMAND file -b "${_f}"
    OUTPUT_VARIABLE _ftype
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET)
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
  "/Library/Frameworks/")

# Already-bundled paths:
set(_bundle_prefixes
  "@executable_path"
  "@loader_path"
  "@rpath")

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

set(_deps_to_bundle "")

foreach(_bin IN LISTS _macho_files)
  execute_process(
    COMMAND otool -L "${_bin}"
    OUTPUT_VARIABLE _otool_out
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET)
  # Parse each line: "	/path/to/lib.dylib (compatibility ...)"
  string(REPLACE "\n" ";" _lines "${_otool_out}")
  foreach(_line IN LISTS _lines)
    string(STRIP "${_line}" _line)
    if(_line MATCHES "^\t?(/[^ ]+\\.dylib)")
      set(_dep "${CMAKE_MATCH_1}")
      _is_system_lib("${_dep}" _is_sys)
      if(NOT _is_sys AND EXISTS "${_dep}")
        list(APPEND _deps_to_bundle "${_dep}")
      endif()
    endif()
  endforeach()
endforeach()

# Also scan already-bundled Frameworks/ dylibs for @loader_path siblings
# that the initial scan above may have missed (e.g. ICU data libraries).
file(GLOB _fw_dylibs "${FRAMEWORKS_DIR}/*.dylib")
foreach(_fwlib IN LISTS _fw_dylibs)
  if(IS_SYMLINK "${_fwlib}")
    continue()
  endif()
  execute_process(
    COMMAND otool -L "${_fwlib}"
    OUTPUT_VARIABLE _fw_otool
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET)
  string(REPLACE "\n" ";" _fw_lines "${_fw_otool}")
  foreach(_fw_line IN LISTS _fw_lines)
    string(STRIP "${_fw_line}" _fw_line)
    if(_fw_line MATCHES "^\t?(/[^ ]+\\.dylib)")
      set(_fw_dep "${CMAKE_MATCH_1}")
      _is_system_lib("${_fw_dep}" _fw_sys)
      if(NOT _fw_sys AND EXISTS "${_fw_dep}")
        get_filename_component(_fw_dep_name "${_fw_dep}" NAME)
        if(NOT EXISTS "${FRAMEWORKS_DIR}/${_fw_dep_name}")
          list(APPEND _deps_to_bundle "${_fw_dep}")
        endif()
      endif()
    endif()
  endforeach()
endforeach()

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

      file(COPY "${_dep_real}" DESTINATION "${FRAMEWORKS_DIR}"
        FILE_PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

      # Rename to the expected name if the real file differs
      # (e.g., libicudata.77.1.dylib must be named libicudata.77.dylib)
      if(NOT "${_dep_real_name}" STREQUAL "${_depname}")
        if(EXISTS "${FRAMEWORKS_DIR}/${_dep_real_name}")
          file(RENAME "${FRAMEWORKS_DIR}/${_dep_real_name}" "${_dest}")
        endif()
      endif()

      # Make writable so install_name_tool can modify it
      file(CHMOD "${_dest}"
        PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)

      # Change the library's own install name
      execute_process(
        COMMAND install_name_tool -id "@rpath/${_depname}" "${_dest}"
        ERROR_QUIET)

      list(APPEND _all_bundled "${_dest}")

      # Discover transitive deps of this newly-copied library
      execute_process(
        COMMAND otool -L "${_dest}"
        OUTPUT_VARIABLE _sub_out
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET)
      string(REPLACE "\n" ";" _sub_lines "${_sub_out}")
      foreach(_sub_line IN LISTS _sub_lines)
        string(STRIP "${_sub_line}" _sub_line)
        # ---- Absolute paths ----
        if(_sub_line MATCHES "^\t?(/[^ ]+\\.dylib)")
          set(_sub_dep "${CMAKE_MATCH_1}")
          _is_system_lib("${_sub_dep}" _sub_sys)
          if(NOT _sub_sys AND EXISTS "${_sub_dep}")
            get_filename_component(_sub_name "${_sub_dep}" NAME)
            if(NOT EXISTS "${FRAMEWORKS_DIR}/${_sub_name}")
              list(APPEND _new_deps "${_sub_dep}")
            endif()
          endif()
        # ---- @loader_path references (resolve relative to original source dir) ----
        elseif(_sub_line MATCHES "^\t?@loader_path/([^ ]+\\.dylib)")
          set(_lp_name "${CMAKE_MATCH_1}")
          set(_lp_resolved "${_dep_srcdir}/${_lp_name}")
          # Follow symlinks to find the real file
          if(NOT EXISTS "${_lp_resolved}" AND IS_SYMLINK "${_lp_resolved}")
            get_filename_component(_lp_resolved "${_lp_resolved}" REALPATH)
          endif()
          if(EXISTS "${_lp_resolved}" AND NOT EXISTS "${FRAMEWORKS_DIR}/${_lp_name}")
            message(STATUS "    Resolved @loader_path/${_lp_name} -> ${_lp_resolved}")
            list(APPEND _new_deps "${_lp_resolved}")
          endif()
        # ---- @rpath references (try source dir as fallback) ----
        elseif(_sub_line MATCHES "^\t?@rpath/([^ ]+\\.dylib)")
          set(_rp_name "${CMAKE_MATCH_1}")
          if(NOT EXISTS "${FRAMEWORKS_DIR}/${_rp_name}")
            if(EXISTS "${_dep_srcdir}/${_rp_name}")
              message(STATUS "    Resolved @rpath/${_rp_name} -> ${_dep_srcdir}/${_rp_name}")
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
  "${RESOURCES_DIR}/bin/*"
  "${RESOURCES_DIR}/lib/*.dylib"
  "${RESOURCES_DIR}/lib/Nelson/*.dylib")

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
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
  if(NOT _ftype MATCHES "Mach-O")
    continue()
  endif()

  # Make writable
  file(CHMOD "${_bin}"
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

  # ---- Remove old absolute rpaths that won't exist on another machine ----
  execute_process(
    COMMAND otool -l "${_bin}"
    OUTPUT_VARIABLE _otool_lc
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
  string(REPLACE "\n" ";" _lc_lines "${_otool_lc}")
  set(_in_rpath FALSE)
  foreach(_lc_line IN LISTS _lc_lines)
    string(STRIP "${_lc_line}" _lc_line)
    if(_lc_line MATCHES "cmd LC_RPATH")
      set(_in_rpath TRUE)
    elseif(_in_rpath AND _lc_line MATCHES "path (.+) \\(offset")
      set(_old_rpath "${CMAKE_MATCH_1}")
      # Remove rpaths that are absolute and do NOT start with @
      if(_old_rpath MATCHES "^/" AND NOT _old_rpath MATCHES "^@")
        execute_process(
          COMMAND install_name_tool -delete_rpath "${_old_rpath}" "${_bin}"
          ERROR_QUIET)
        message(STATUS "  Removed old rpath: ${_old_rpath} from ${_bin}")
      endif()
      set(_in_rpath FALSE)
    endif()
  endforeach()

  # ---- Add @rpath pointing to Frameworks/ ----
  get_filename_component(_bin_dir "${_bin}" DIRECTORY)
  file(RELATIVE_PATH _rel_to_fw "${_bin_dir}" "${FRAMEWORKS_DIR}")
  execute_process(
    COMMAND install_name_tool -add_rpath "@loader_path/${_rel_to_fw}" "${_bin}"
    ERROR_QUIET) # Ignore if already exists

  # ---- Add @rpath pointing to Nelson lib dir (lib/Nelson/) ----
  # So that @rpath/libnlsXxx.dylib references between Nelson libraries resolve.
  file(RELATIVE_PATH _rel_to_nlib "${_bin_dir}" "${NELSON_LIB_DIR}")
  if(_rel_to_nlib STREQUAL "")
    set(_rel_to_nlib ".")
  endif()
  execute_process(
    COMMAND install_name_tool -add_rpath "@loader_path/${_rel_to_nlib}" "${_bin}"
    ERROR_QUIET) # Ignore if already exists

  # ---- Rewrite all third-party references to @rpath/ ----
  execute_process(
    COMMAND otool -L "${_bin}"
    OUTPUT_VARIABLE _refs
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
  string(REPLACE "\n" ";" _ref_lines "${_refs}")
  foreach(_ref_line IN LISTS _ref_lines)
    string(STRIP "${_ref_line}" _ref_line)
    # Absolute paths → @rpath/
    if(_ref_line MATCHES "^\t?(/[^ ]+/([^ /]+\\.dylib))")
      set(_old_path "${CMAKE_MATCH_1}")
      set(_lib_name "${CMAKE_MATCH_2}")
      _is_system_lib("${_old_path}" _ref_sys)
      if(NOT _ref_sys AND EXISTS "${FRAMEWORKS_DIR}/${_lib_name}")
        execute_process(
          COMMAND install_name_tool -change "${_old_path}" "@rpath/${_lib_name}" "${_bin}"
          ERROR_QUIET)
      endif()
    # @loader_path/name.dylib → @rpath/name.dylib (when lib is in Frameworks/)
    elseif(_ref_line MATCHES "^\t?(@loader_path/([^ /]+\\.dylib))")
      set(_old_path "${CMAKE_MATCH_1}")
      set(_lib_name "${CMAKE_MATCH_2}")
      if(EXISTS "${FRAMEWORKS_DIR}/${_lib_name}")
        execute_process(
          COMMAND install_name_tool -change "${_old_path}" "@rpath/${_lib_name}" "${_bin}"
          ERROR_QUIET)
      endif()
    endif()
  endforeach()
endforeach()

# ==============================================================================
# Step 6: Ad-hoc codesign (required on Apple Silicon)
# ==============================================================================
find_program(CODESIGN codesign)
if(CODESIGN)
  message(STATUS "Ad-hoc codesigning the bundle...")
  execute_process(
    COMMAND "${CODESIGN}" --force --deep --sign - "${APP_BUNDLE_DIR}"
    RESULT_VARIABLE _cs_rc
    ERROR_VARIABLE _cs_err)
  if(NOT _cs_rc EQUAL 0)
    message(WARNING "codesign failed: ${_cs_err}")
  else()
    message(STATUS "Codesigning completed")
  endif()
else()
  message(WARNING "codesign not found – bundle may not run on Apple Silicon")
endif()

# ==============================================================================
# Step 7: Summary
# ==============================================================================
list(LENGTH _all_bundled _total_bundled)
message(STATUS "=== Fixup complete ===")
message(STATUS "Bundled ${_total_bundled} third-party libraries into Frameworks/")
message(STATUS "Bundle is ready for DMG packaging")
# ==============================================================================
