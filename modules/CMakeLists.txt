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
  file_archiver
  notebook
  font_metrics
  render_web
)
# ==============================================================================
# Optional modules (conditionally built based on WITHOUT_*_MODULE options)
# ==============================================================================
set(_optional_modules_upper
  F2C
  GUI
  TEXT_COMPLETION
  QML_ENGINE
  GRAPHICS
  RENDER_QT
  GRAPHICS_QT
  GRAPHICS_IO
  TEXT_EDITOR
  FFTW
  VALIDATORS
  MPI
  AUDIO
  WEBTOOLS
  WEBVIEW
  PARALLEL
  DATA_ANALYSIS
  CATEGORICAL
  ODE_SOLVERS
  HDF5
  PREFERENCES
  NETCDF
  MATIO
  DYNAMIC_LINK
  TESTS_MANAGER
  XML
  JSON
  AI
  HELP_TOOLS
  SLICOT
  CONTROL_SYSTEM
  ASSERT_FUNCTIONS
  SPECIAL_FUNCTIONS
  STATISTICS
  TRIGONOMETRIC_FUNCTIONS
  RANDOM
  OPTIMIZATION
  GEOMETRY
  PYTHON_ENGINE
  PARQUET
  SPREADSHEET
  JULIA_ENGINE
  IMAGE_PROCESSING
  SIGNAL_PROCESSING
  POLYNOMIAL_FUNCTIONS
  NFLOW_BLOCKS
  NFLOW_ENGINE
  NFLOW_FMI
  NFLOW_GUI
  NMM_GUI
)

if(NELSON_PORTABLE_BUILD)
  if(NELSON_WASM_BUILD)
    # The browser uses the portable editor gateway without the desktop GUI.
    set(WITHOUT_TEXT_EDITOR_MODULE OFF)
  endif()
  # Positive module selection for the portable runtime. This first closure
  # preserves the current engine dependencies; later portability work can move
  # native services behind adapters and remove them from this list.
  set(_all_modules
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
    categorical
    history_manager
    terminal
    text_completion
    text_editor
    notebook
    handle
    engine
    profiler
    mex
    ipc
    file_archiver
  )

  if(NELSON_PORTABLE_WITH_PLOTS)
    list(APPEND _all_modules
      font_metrics
      render_web
      json
      special_functions
      trigonometric_functions
      graphics
      graphics_io
    )
  endif()

  if(NELSON_PORTABLE_WITH_NFLOW)
    list(APPEND _all_modules
      nflow_blocks
      nflow_engine
    )
    if(NELSON_WASM_BUILD)
      list(APPEND _all_modules nflow_fmi)
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_CONTROL_SYSTEM)
    list(APPEND _all_modules
      f2c
      slicot
      data_analysis
      polynomial_functions
      validators
      control_system
    )
  endif()

  if(NELSON_PORTABLE_WITH_ODE_SOLVERS)
    list(APPEND _all_modules
      nflow_blocks
      ode_solvers
    )
  endif()

  if(NELSON_PORTABLE_WITH_NMM)
    list(APPEND _all_modules
      file_archiver
      json
    )
  endif()

  if(NELSON_PORTABLE_WITH_STATISTICS)
    list(APPEND _all_modules
      random
      statistics
    )
  endif()

  if(NELSON_PORTABLE_WITH_IMAGE_PROCESSING)
    list(APPEND _all_modules
      image_processing
    )
  endif()

  if(NELSON_PORTABLE_WITH_OPTIMIZATION)
    list(APPEND _all_modules
      optimization
    )
  endif()

  if(NELSON_PORTABLE_WITH_FFTW)
    list(APPEND _all_modules
      fftw
    )
  endif()

  if(NELSON_PORTABLE_WITH_SIGNAL_PROCESSING)
    list(APPEND _all_modules
      polynomial_functions
      special_functions
      trigonometric_functions
      signal_processing
    )
  endif()

  if(NELSON_PORTABLE_WITH_GEOMETRY)
    list(APPEND _all_modules
      polynomial_functions
      special_functions
      trigonometric_functions
      geometry
    )
  endif()

  if(NELSON_PORTABLE_WITH_HDF5)
    list(APPEND _all_modules
      hdf5
    )
  endif()

  if(NELSON_PORTABLE_WITH_NETCDF)
    list(APPEND _all_modules
      hdf5
      netcdf
    )
  endif()

  if(NELSON_PORTABLE_WITH_MATIO)
    list(APPEND _all_modules
      hdf5
      matio
    )
  endif()

  if(NELSON_PORTABLE_WITH_PARQUET)
    list(APPEND _all_modules
      parquet
    )
  endif()

  if(NELSON_PORTABLE_WITH_XML)
    list(APPEND _all_modules
      json
      xml
      help_tools
    )
  endif()

  if(NELSON_PORTABLE_WITH_SPREADSHEET)
    list(APPEND _all_modules
      json
      xml
      spreadsheet
    )
  endif()

  if(NELSON_PORTABLE_WITH_AUDIO)
    list(APPEND _all_modules
      audio
    )
  endif()

  if(NELSON_PORTABLE_WITH_PREFERENCES)
    list(APPEND _all_modules
      hdf5
      preferences
    )
  endif()

  if(NELSON_PORTABLE_WITH_WEBTOOLS AND NELSON_WASM_BUILD)
    list(APPEND _all_modules
      json
      webtools
    )
  endif()

  if(NELSON_PORTABLE_ENABLE_TESTS)
    list(APPEND _all_modules
      assert_functions
      data_analysis
      validators
      tests_manager
    )
  endif()
else()
  set(_all_modules ${_core_modules})
  foreach(_mod IN LISTS _optional_modules_upper)
    if(NOT WITHOUT_${_mod}_MODULE)
      string(TOLOWER "${_mod}" _mod_lower)
      list(APPEND _all_modules
        ${_mod_lower}
      )
    endif()
  endforeach()

  # web_gui is opt-in (OFF by default) — see modules/web_gui/INTEGRATION.md
  if(WITH_WEB_GUI_MODULE)
    list(APPEND _all_modules
      web_gui
    )
  endif()

  # gpu_engine is opt-in (OFF by default): GPU compute via WebGPU/Dawn
  if(WITH_GPU_ENGINE_MODULE)
    list(APPEND _all_modules
      gpu_engine
    )
  endif()
endif()

# WITHOUT_COMPILER_MODULE is declared centrally (NELSON_OPTIONAL_MODULES).
if(NOT WITHOUT_COMPILER_MODULE
  AND NOT WITHOUT_FILE_ARCHIVER_MODULE
  AND NOT WITHOUT_DYNAMIC_LINK_MODULE
  AND NOT WITHOUT_JSON_MODULE
)
  list(APPEND _all_modules compiler)
endif()
# main is always last (depends on everything)
list(APPEND _all_modules
  main
)
list(REMOVE_DUPLICATES _all_modules)
set_property(GLOBAL PROPERTY NELSON_SELECTED_MODULES "${_all_modules}")
message(STATUS "Selected Nelson modules: ${_all_modules}")
# ==============================================================================
# Add all selected modules
# ==============================================================================
foreach(_mod IN LISTS _all_modules)
  add_subdirectory(${_mod})
endforeach()
# ==============================================================================
include(GNUInstallDirs)
install(
  FILES ${NELSON_GENERATED_MODULES_FILE}
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules
)
