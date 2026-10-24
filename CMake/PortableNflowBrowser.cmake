# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
# Build and package the NFlow browser editor and its companion example assets.
set(_nelson_wasm_nflow_enabled 0)
set(_nelson_wasm_nflow_build_commands "")
set(_nelson_wasm_nflow_package_arguments "")
set(_nelson_wasm_nflow_dependencies "")
if(NELSON_PORTABLE_WITH_NFLOW)
  set(_nelson_wasm_nflow_enabled 1)
  set(_nelson_wasm_nflow_examples_dir
    "${CMAKE_BINARY_DIR}/generated/wasm-nflow-examples"
  )
  set_property(
    DIRECTORY
    APPEND
    PROPERTY
      CMAKE_CONFIGURE_DEPENDS
      "${CMAKE_SOURCE_DIR}/tools/wasm/generate-nflow-examples.mjs"
  )
  # Include block and FMI diagrams; the generator excludes Modelica examples.
  execute_process(
    COMMAND "${NELSON_NODE_EXECUTABLE}"
      "${CMAKE_SOURCE_DIR}/tools/wasm/generate-nflow-examples.mjs" --source
      "${CMAKE_SOURCE_DIR}/modules/nflow_blocks/examples" --fmi-source
      "${CMAKE_SOURCE_DIR}/modules/nflow_fmi/examples" --output
      "${_nelson_wasm_nflow_examples_dir}"
    RESULT_VARIABLE _nelson_wasm_nflow_examples_status
    OUTPUT_VARIABLE _nelson_wasm_nflow_examples_output
    ERROR_VARIABLE _nelson_wasm_nflow_examples_error
  )
  if(NOT _nelson_wasm_nflow_examples_status EQUAL 0)
    message(FATAL_ERROR
      "WebAssembly NFlow examples generation failed:\n${_nelson_wasm_nflow_examples_error}"
    )
  endif()
  string(STRIP
    "${_nelson_wasm_nflow_examples_output}"
    _nelson_wasm_nflow_examples_output
  )
  message(STATUS "${_nelson_wasm_nflow_examples_output}")
  file(GLOB_RECURSE _nelson_wasm_nflow_sources
    CONFIGURE_DEPENDS
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui/src/ts/*"
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui/scripts/*"
    "${CMAKE_SOURCE_DIR}/modules/nflow_blocks/libraries/*"
    "${CMAKE_SOURCE_DIR}/modules/nflow_blocks/examples/*"
    "${CMAKE_SOURCE_DIR}/modules/nflow_fmi/examples/*"
  )
  list(APPEND _nelson_wasm_nflow_build_commands
    COMMAND
    "${NELSON_NODE_EXECUTABLE}"
    "${CMAKE_SOURCE_DIR}/tools/wasm/generate-nflow-examples.mjs"
    --source
    "${CMAKE_SOURCE_DIR}/modules/nflow_blocks/examples"
    --fmi-source
    "${CMAKE_SOURCE_DIR}/modules/nflow_fmi/examples"
    --output
    "${_nelson_wasm_nflow_examples_dir}"
    COMMAND
    "${CMAKE_COMMAND}"
    -E
    chdir
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui"
    "${NELSON_NPM_EXECUTABLE}"
    ci
    --ignore-scripts
    COMMAND
    "${CMAKE_COMMAND}"
    -E
    chdir
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui"
    "${NELSON_NPM_EXECUTABLE}"
    run
    build:wasm
  )
  list(APPEND _nelson_wasm_nflow_package_arguments
    --nflow
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui/dist-wasm"
    --nflow-examples
    "${_nelson_wasm_nflow_examples_dir}"
  )
  list(APPEND _nelson_wasm_nflow_dependencies
    ${_nelson_wasm_nflow_sources}
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui/package.json"
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui/package-lock.json"
    "${CMAKE_SOURCE_DIR}/modules/nflow_gui/vite.wasm.config.ts"
    "${CMAKE_SOURCE_DIR}/tools/wasm/generate-nflow-examples.mjs"
  )
endif()
