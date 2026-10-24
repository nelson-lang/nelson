#=============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
#=============================================================================
# This file is part of Nelson.
#=============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
#=============================================================================

include(GNUInstallDirs)
find_package(Threads REQUIRED)
set(_compiler_install_data_dir "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")
set(_compiler_install_binary_dir "${CMAKE_INSTALL_BINDIR}")
set(_compiler_install_library_dir "${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}")
if(WIN32)
  set(_compiler_install_library_dir "${CMAKE_INSTALL_BINDIR}")
endif()
foreach(_directory
  _compiler_install_data_dir
  _compiler_install_binary_dir
  _compiler_install_library_dir
)
  get_filename_component(_compiler_absolute_dir
    "${${_directory}}"
    ABSOLUTE
    BASE_DIR
    "${CMAKE_INSTALL_PREFIX}"
  )
  file(RELATIVE_PATH
    ${_directory}
    "${CMAKE_INSTALL_PREFIX}"
    "${_compiler_absolute_dir}"
  )
  if(IS_ABSOLUTE "${${_directory}}"
    OR "${${_directory}}" MATCHES "(^|[/\\\\])\\.\\.([/\\\\]|$)"
  )
    message(FATAL_ERROR
      "compiler install directories must stay within the installation prefix: ${_directory}"
    )
  endif()
endforeach()
set(_compiler_launchers nelsonc nelson-app)
if(WIN32)
  list(APPEND _compiler_launchers nelson-app-windowed)
endif()
foreach(_launcher IN LISTS _compiler_launchers)
  add_executable(${_launcher}
    PackagerMain.cpp
    RuntimeLibrarySearch.hpp
    LauncherInputFile.hpp
    SharedRuntimeRegistry.hpp
    SharedRuntimeRegistryPosix.hpp
    RuntimeLog.hpp
    RuntimeLogPlatform.hpp
    RuntimeLogConsole.hpp
  )
  if(WIN32)
    target_link_libraries(${_launcher} PRIVATE advapi32)
    target_sources(${_launcher} PRIVATE Packager.manifest)
    set_property(
      TARGET ${_launcher}
      PROPERTY
        MSVC_RUNTIME_LIBRARY
        "MultiThreaded$<$<CONFIG:Debug>:Debug>"
    )
  endif()
  target_compile_features(${_launcher} PRIVATE cxx_std_17)
  if(MSVC)
    target_compile_options(${_launcher} PRIVATE $<$<CONFIG:Release>:/Gy>)
    target_link_options(${_launcher}
      PRIVATE
        $<$<CONFIG:Release>:/OPT:REF>
        $<$<CONFIG:Release>:/OPT:ICF>
    )
  endif()
  target_compile_definitions(${_launcher}
    PRIVATE
      NELSONC_INSTALL_DATADIR="${_compiler_install_data_dir}"
      NELSONC_INSTALL_BINDIR="${_compiler_install_binary_dir}"
      NELSONC_INSTALL_LIBDIR="${_compiler_install_library_dir}"
  )
  target_include_directories(${_launcher}
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../../engine/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../../file_archiver/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../../core/src/include
  )
  target_link_libraries(${_launcher} PRIVATE ${CMAKE_DL_LIBS} Threads::Threads)
  _nelson_configure_main_executable(${_launcher} ${_launcher})
  install(TARGETS ${_launcher} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endforeach()
if(TARGET nelson-app-windowed)
  set_target_properties(nelson-app-windowed PROPERTIES WIN32_EXECUTABLE TRUE)
  target_compile_definitions(nelson-app-windowed PRIVATE NELSONC_WINDOWED)
  target_link_libraries(nelson-app-windowed PRIVATE shell32)
endif()
target_compile_definitions(nelsonc PRIVATE NELSONC_TOOL)
add_dependencies(nelsonc nlsCompiler_builtin)
