# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(NOT DEFINED NELSON_BIN_DIRECTORY)
  message(FATAL_ERROR "NELSON_BIN_DIRECTORY is required")
endif()

function(_require_file _path)
  if(NOT EXISTS "${_path}")
    message(FATAL_ERROR "Required bundle file is missing: ${_path}")
  endif()
endfunction()

function(_plist_value _plist _key _output)
  execute_process(
    COMMAND /usr/bin/plutil -extract "${_key}" raw -o - "${_plist}"
    RESULT_VARIABLE _result
    OUTPUT_VARIABLE _value
    ERROR_VARIABLE _error
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(NOT _result EQUAL 0)
    message(FATAL_ERROR "Cannot read ${_key} from ${_plist}: ${_error}")
  endif()
  set(${_output} "${_value}" PARENT_SCOPE)
endfunction()

function(_check_bundle _name _display_name _executable _identifier_output)
  set(_bundle "${NELSON_BIN_DIRECTORY}/${_name}.app")
  set(_plist "${_bundle}/Contents/Info.plist")
  _require_file("${_plist}")
  _require_file("${_bundle}/Contents/MacOS/${_executable}")
  _require_file("${_bundle}/Contents/Resources/Nelson.icns")

  execute_process(
    COMMAND /usr/bin/plutil -lint "${_plist}"
    RESULT_VARIABLE _lint_result
    ERROR_VARIABLE _lint_error
    OUTPUT_QUIET
  )
  if(NOT _lint_result EQUAL 0)
    message(FATAL_ERROR "Invalid bundle property list: ${_lint_error}")
  endif()

  _plist_value("${_plist}" CFBundleDisplayName _actual_display_name)
  _plist_value("${_plist}" CFBundleExecutable _actual_executable)
  _plist_value("${_plist}" CFBundleIconFile _actual_icon)
  _plist_value("${_plist}" CFBundleIdentifier _actual_identifier)
  if(NOT _actual_display_name STREQUAL "${_display_name}")
    message(FATAL_ERROR "Unexpected display name for ${_name}.app")
  endif()
  if(NOT _actual_executable STREQUAL "${_executable}")
    message(FATAL_ERROR "Unexpected executable for ${_name}.app")
  endif()
  if(NOT _actual_icon STREQUAL "Nelson.icns")
    message(FATAL_ERROR "Unexpected icon for ${_name}.app")
  endif()
  if(_actual_identifier STREQUAL "")
    message(FATAL_ERROR "Missing bundle identifier for ${_name}.app")
  endif()
  set(${_identifier_output} "${_actual_identifier}" PARENT_SCOPE)
endfunction()

_check_bundle("Nelson" "Nelson" "nelson-gui-exec" NELSON_IDENTIFIER)
_check_bundle(
  "Nelson WebView"
  "Nelson WebView"
  "nelson-webview-exec"
  NELSON_WEB_IDENTIFIER
)
if(NOT NELSON_WEB_IDENTIFIER STREQUAL "${NELSON_IDENTIFIER}.webview")
  message(FATAL_ERROR
    "Nelson WebView must use the Nelson bundle identifier with a .webview suffix"
  )
endif()

file(READ "${NELSON_BIN_DIRECTORY}/nelson" _launcher)
foreach(_bundle_executable
  "Nelson.app/Contents/MacOS/nelson-gui-exec"
  "Nelson WebView.app/Contents/MacOS/nelson-webview-exec"
)
  string(FIND "${_launcher}" "${_bundle_executable}" _position)
  if(_position EQUAL -1)
    message(FATAL_ERROR "Launcher does not select ${_bundle_executable}")
  endif()
endforeach()
