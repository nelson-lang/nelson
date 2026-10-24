# Standalone test projects compile module sources that include Nelson_VERSION.h,
# a header the main configure step generates and git ignores. When it is absent,
# generate it in the build tree from the template and the root defaults.
function(nelson_tests_version_header target repository_dir)
  set(_header "${repository_dir}/modules/commons/src/include/Nelson_VERSION.h")
  if(EXISTS "${_header}")
    return()
  endif()
  file(STRINGS "${repository_dir}/CMakeLists.txt"
    _defaults
    REGEX "set\\(Nelson_VERSION_[A-Z]+_DEFAULT [0-9]+\\)"
  )
  foreach(_line IN LISTS _defaults)
    if(_line MATCHES "set\\(Nelson_VERSION_([A-Z]+)_DEFAULT ([0-9]+)\\)")
      set(Nelson_VERSION_${CMAKE_MATCH_1} ${CMAKE_MATCH_2})
    endif()
  endforeach()
  foreach(_part MAJOR MINOR MAINTENANCE BUILD)
    if(NOT DEFINED Nelson_VERSION_${_part})
      message(FATAL_ERROR
        "Nelson_VERSION_${_part}_DEFAULT not found in ${repository_dir}/CMakeLists.txt"
      )
    endif()
  endforeach()
  set(_generated_dir "${CMAKE_BINARY_DIR}/generated")
  configure_file(
    "${repository_dir}/modules/commons/src/include/Nelson_VERSION.h.in"
    "${_generated_dir}/Nelson_VERSION.h"
  )
  target_include_directories(${target} PRIVATE "${_generated_dir}")
  message(STATUS "Nelson_VERSION.h generated in ${_generated_dir}")
endfunction()
