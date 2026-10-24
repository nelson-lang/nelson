foreach(_icon_name IN ITEMS m mat nh5 nflow)
  set(_icon "${NELSON_TEST_REPOSITORY_DIR}/resources/${_icon_name}.ico")
  if(NOT EXISTS "${_icon}")
    message(FATAL_ERROR "File-association icon is missing: ${_icon}")
  endif()

  file(READ "${_icon}" _icon_header LIMIT 6 HEX)
  string(TOLOWER "${_icon_header}" _icon_header)
  if(NOT _icon_header STREQUAL "000001000600")
    message(FATAL_ERROR
      "${_icon_name}.ico must contain six Windows icon images"
    )
  endif()

  file(READ "${_icon}" _icon_directory LIMIT 102 HEX)
  string(TOLOWER "${_icon_directory}" _icon_directory)
  string(SUBSTRING "${_icon_directory}" 172 4 _large_icon_size)
  if(NOT _large_icon_size STREQUAL "0000")
    message(FATAL_ERROR
      "${_icon_name}.ico must contain a 256x256 Windows icon image"
    )
  endif()
endforeach()

file(READ "${NELSON_TEST_REPOSITORY_DIR}/modules/main/nelson_gui/resource.h"
  _resource_header
)
if(NOT _resource_header MATCHES "#define IDI_ICON_NFLOW 105")
  message(FATAL_ERROR "NFlow icon resource identifier is missing")
endif()

file(READ "${NELSON_TEST_REPOSITORY_DIR}/modules/main/nelson_gui/resource.rc"
  _resource_script
)
if(NOT _resource_script MATCHES "IDI_ICON_NFLOW[^\n]*nflow\\.ico")
  message(FATAL_ERROR "NFlow icon is not embedded in Nelson-gui.exe")
endif()

file(READ "${NELSON_TEST_REPOSITORY_DIR}/tools/innosetup/registry.iss"
  _registry_script
)
if(NOT _registry_script MATCHES "#define ICON_NFLOW_POS 4")
  message(FATAL_ERROR
    "The .nflow association does not select its dedicated icon"
  )
endif()
