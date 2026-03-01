# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Core modules (always built, order matters for dependencies)
# ==============================================================================
set(_core_modules
    characters_encoding
    commons
    nelson_manager
    debugger
    i18n
    localization
    elementary_mathematics
    error_manager
    types
    stream_manager
    display_format
    operators
    elementary_functions
    interpreter
    modules_manager
    double
    single
    sparse
    functions_manager
    constructors_functions
    console
    logical
    time
    string
    integer
    linear_algebra
    files_folders_functions
    os_functions
    core
    function_handle
    memory_manager
    overload
    data_structures
    dictionary
    table
    history_manager
    terminal
    handle
    engine
    profiler
    mex
    ipc
    sio_client
    file_archiver)
# ==============================================================================
# Optional modules (conditionally built based on WITHOUT_*_MODULE options)
# ==============================================================================
set(_optional_modules_upper
    F2C
    GUI
    TEXT_COMPLETION
    QML_ENGINE
    GRAPHICS
    GRAPHICS_IO
    TEXT_EDITOR
    FFTW
    VALIDATORS
    MPI
    AUDIO
    WEBTOOLS
    PARALLEL
    DATA_ANALYSIS
    HDF5
    MATIO
    DYNAMIC_LINK
    TESTS_MANAGER
    XML
    JSON
    HELP_TOOLS
    SLICOT
    CONTROL_SYSTEM
    ASSERT_FUNCTIONS
    SPECIAL_FUNCTIONS
    STATISTICS
    TRIGONOMETRIC_FUNCTIONS
    RANDOM
    GEOMETRY
    PYTHON_ENGINE
    SPREADSHEET
    JULIA_ENGINE
    IMAGE_PROCESSING
    SIGNAL_PROCESSING
    POLYNOMIAL_FUNCTIONS
    NIG)

set(_all_modules ${_core_modules})
foreach(_mod IN LISTS _optional_modules_upper)
  if(NOT WITHOUT_${_mod}_MODULE)
    string(TOLOWER "${_mod}" _mod_lower)
    list(
      APPEND
      _all_modules
      ${_mod_lower})
  endif()
endforeach()

# main is always last (depends on everything)
list(
  APPEND
  _all_modules
  main)
# ==============================================================================
# Add all selected modules
# ==============================================================================
foreach(_mod IN LISTS _all_modules)
  add_subdirectory(${_mod})
endforeach()
# ==============================================================================
include(GNUInstallDirs)
install(FILES ${CMAKE_SOURCE_DIR}/modules/modules.m
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules)
# ==============================================================================
