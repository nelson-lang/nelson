set(_application_icon "${NELSON_TEST_REPOSITORY_DIR}/resources/fibonacci.ico")
if(NOT EXISTS "${_application_icon}")
  message(FATAL_ERROR
    "Windows application icon is missing: ${_application_icon}"
  )
endif()

file(READ "${_application_icon}" _icon_header LIMIT 6 HEX)
string(TOLOWER "${_icon_header}" _icon_header)
if(NOT _icon_header STREQUAL "000001000800")
  message(FATAL_ERROR "Windows application icon must contain eight icon images")
endif()

file(READ "${_application_icon}" _icon_directory LIMIT 134 HEX)
string(TOLOWER "${_icon_directory}" _icon_directory)
string(SUBSTRING "${_icon_directory}" 236 4 _large_icon_size)
if(NOT _large_icon_size STREQUAL "0000")
  message(FATAL_ERROR "Windows application icon must contain a 256x256 image")
endif()

foreach(_size IN ITEMS 16 24 32 48 64 96 128 256 512)
  set(_png
    "${NELSON_TEST_REPOSITORY_DIR}/desktop/icons/hicolor/${_size}x${_size}/apps/nelson.png"
  )
  if(NOT EXISTS "${_png}")
    message(FATAL_ERROR "Linux application icon is missing: ${_png}")
  endif()
  file(READ "${_png}" _png_header LIMIT 8 HEX)
  string(TOLOWER "${_png_header}" _png_header)
  if(NOT _png_header STREQUAL "89504e470d0a1a0a")
    message(FATAL_ERROR "Linux application icon is not a PNG image: ${_png}")
  endif()
endforeach()

set(_snap_icon "${NELSON_TEST_REPOSITORY_DIR}/snap/gui/logo.png")
if(NOT EXISTS "${_snap_icon}")
  message(FATAL_ERROR "Snap application icon is missing: ${_snap_icon}")
endif()
file(READ "${_snap_icon}" _snap_icon_header LIMIT 8 HEX)
string(TOLOWER "${_snap_icon_header}" _snap_icon_header)
if(NOT _snap_icon_header STREQUAL "89504e470d0a1a0a")
  message(FATAL_ERROR "Snap application icon is not a PNG image")
endif()
