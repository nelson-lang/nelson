# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
add_executable(nelson-installer WIN32
  InstallerMain.cpp
  InstallerOptions.cpp
  InstallerOptions.hpp
  InstallerContract.rc
  ../../include/InstallerPayload.hpp
  ../../include/InstallerIO.hpp
  ../../../../main/nelsonc/Packager.manifest
)
target_compile_features(nelson-installer PRIVATE cxx_std_17)
target_compile_definitions(nelson-installer PRIVATE UNICODE _UNICODE)
target_include_directories(nelson-installer
  PRIVATE
    ../../include
    ../../../../core/src/include
    ../../../../file_archiver/src/include
)
target_link_libraries(nelson-installer PRIVATE advapi32 bcrypt shell32)
set_property(
  TARGET nelson-installer
  PROPERTY
    MSVC_RUNTIME_LIBRARY
    "MultiThreaded$<$<CONFIG:Debug>:Debug>"
)
if(MSVC)
  target_compile_options(nelson-installer
    PRIVATE
      /W4
      /WX
      /permissive-
      /utf-8
      $<$<CONFIG:Release>:/Gy>
  )
  target_link_options(nelson-installer
    PRIVATE
      $<$<CONFIG:Release>:/OPT:REF>
      $<$<CONFIG:Release>:/OPT:ICF>
  )
endif()
install(TARGETS nelson-installer RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
