# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
add_executable(nelson-runtime-installer
  RuntimeMain.cpp
  RuntimeFiles.cpp
  RuntimeInventory.cpp
  RuntimeInstall.cpp
  RuntimeUninstall.cpp
  RuntimeInstaller.hpp
)
target_compile_features(nelson-runtime-installer PRIVATE cxx_std_17)
target_include_directories(nelson-runtime-installer
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../core/src/include
)
target_compile_options(nelson-runtime-installer
  PRIVATE
    -Wall
    -Wextra
    -Werror
    -pedantic
    # GCC 13 reports a false overflow in the vendored picosha2 finish() at -O3
    $<$<CXX_COMPILER_ID:GNU>:-Wno-stringop-overflow>
)
install(
  TARGETS nelson-runtime-installer
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
