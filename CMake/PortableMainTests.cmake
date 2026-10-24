#[=======================================================================[.rst:
Portable main tests
-------------------

Register native and WebAssembly portable-runtime tests after the main target
and its generated artifacts have been configured.
#]=======================================================================]
if(NELSON_PORTABLE_BUILD AND NELSON_PORTABLE_ENABLE_TESTS)
  if(NELSON_WASM_BUILD)
    add_test(
      NAME portable-smoke
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/run-tests.mjs"
        "$<TARGET_FILE:nelson-cli-exec>"
    )
    foreach(_nelson_test_module IN LISTS _nelson_runtime_modules)
      add_test(
        NAME "portable-unittest-${_nelson_test_module}"
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/run-portable-tests.mjs" --runtime wasm
          --module "$<TARGET_FILE:nelson-cli-exec>" --junit
          "${CMAKE_BINARY_DIR}/portable-wasm-${_nelson_test_module}.junit.xml"
          --modules "${_nelson_test_module}" --force-display
          "$<IF:$<BOOL:${NELSON_PORTABLE_WITH_PLOTS}>,1,0>"
      )
    endforeach()
    add_test(
      NAME portable-test-run-entrypoint
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/run-test-run.mjs"
        "$<TARGET_FILE:nelson-cli-exec>"
    )
    add_test(
      NAME portable-bench-run-entrypoint
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/run-bench-run.mjs"
        "$<TARGET_FILE:nelson-cli-exec>"
    )
    add_test(
      NAME portable-examples
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/validate-examples.mjs" --module
        "$<TARGET_FILE:nelson-cli-exec>" --catalog
        "${_nelson_wasm_examples_dir}/catalog.json"
    )
    if(NELSON_PORTABLE_WITH_NFLOW)
      add_test(
        NAME portable-nflow-trajectories
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/capture-nflow-trajectories.mjs"
          --runtime wasm --module "$<TARGET_FILE:nelson-cli-exec>" --script
          "${CMAKE_SOURCE_DIR}/tools/wasm/nflow_trajectories.m" --output
          "${CMAKE_BINARY_DIR}/nflow-trajectories.json"
      )
    endif()
  else()
    add_test(
      NAME portable-smoke
      COMMAND ${CMAKE_COMMAND} -E env "NELSON_LIBRARIES_PATH=${BIN_DIRECTORY}"
        "NELSON_ROOT_PATH=${CMAKE_SOURCE_DIR}"
        "NELSON_MODULES_PATH=${CMAKE_SOURCE_DIR}/modules"
        "NELSON_BINARIES_PATH=${BIN_DIRECTORY}" "$<TARGET_FILE:nelson-cli-exec>"
        --no-startup --no-user-modules --no-ipc --quiet --file
        "${CMAKE_SOURCE_DIR}/tools/wasm/portable_smoke.m"
    )
    foreach(_nelson_test_module IN LISTS _nelson_runtime_modules)
      add_test(
        NAME "portable-unittest-${_nelson_test_module}"
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/run-portable-tests.mjs" --runtime
          native --module "$<TARGET_FILE:nelson-cli-exec>" --junit
          "${CMAKE_BINARY_DIR}/portable-native-${_nelson_test_module}.junit.xml"
          --modules "${_nelson_test_module}" --force-display
          "$<IF:$<BOOL:${NELSON_PORTABLE_WITH_PLOTS}>,1,0>" --libraries
          "${BIN_DIRECTORY}"
      )
      set_tests_properties("portable-unittest-${_nelson_test_module}"
        PROPERTIES ENVIRONMENT "NELSON_ROOT_PATH=${CMAKE_SOURCE_DIR};NELSON_MODULES_PATH=${CMAKE_SOURCE_DIR}/modules;NELSON_BINARIES_PATH=${BIN_DIRECTORY}"
      )
    endforeach()
    add_test(
      NAME portable-test-run-entrypoint
      COMMAND ${CMAKE_COMMAND} -E env "NELSON_LIBRARIES_PATH=${BIN_DIRECTORY}"
        "NELSON_ROOT_PATH=${CMAKE_SOURCE_DIR}"
        "NELSON_MODULES_PATH=${CMAKE_SOURCE_DIR}/modules"
        "NELSON_BINARIES_PATH=${BIN_DIRECTORY}" "$<TARGET_FILE:nelson-cli-exec>"
        --no-startup --no-user-modules --no-ipc --quiet --file
        "${CMAKE_SOURCE_DIR}/tools/wasm/test_run_smoke.m"
    )
    add_test(
      NAME portable-bench-run-entrypoint
      COMMAND ${CMAKE_COMMAND} -E env "NELSON_LIBRARIES_PATH=${BIN_DIRECTORY}"
        "NELSON_ROOT_PATH=${CMAKE_SOURCE_DIR}"
        "NELSON_MODULES_PATH=${CMAKE_SOURCE_DIR}/modules"
        "NELSON_BINARIES_PATH=${BIN_DIRECTORY}" "$<TARGET_FILE:nelson-cli-exec>"
        --no-startup --no-user-modules --no-ipc --quiet --file
        "${CMAKE_SOURCE_DIR}/tools/wasm/bench_run_smoke.m"
    )
    if(NELSON_PORTABLE_WITH_NFLOW)
      add_test(
        NAME portable-nflow-trajectories
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/capture-nflow-trajectories.mjs"
          --runtime native --module "$<TARGET_FILE:nelson-cli-exec>" --libraries
          "${BIN_DIRECTORY}" --script
          "${CMAKE_SOURCE_DIR}/tools/wasm/nflow_trajectories.m" --output
          "${CMAKE_BINARY_DIR}/nflow-trajectories.json"
      )
      set_tests_properties(portable-nflow-trajectories
        PROPERTIES ENVIRONMENT "NELSON_ROOT_PATH=${CMAKE_SOURCE_DIR};NELSON_MODULES_PATH=${CMAKE_SOURCE_DIR}/modules;NELSON_BINARIES_PATH=${BIN_DIRECTORY}"
      )
    endif()
    add_test(
      NAME portable-native-install
      COMMAND ${CMAKE_COMMAND} "-DBUILD_DIR=${CMAKE_BINARY_DIR}"
        "-DINSTALL_DIR=${CMAKE_BINARY_DIR}/install-validation/$<CONFIG>"
        "-DCONFIG=$<CONFIG>" -P
        "${CMAKE_SOURCE_DIR}/tools/wasm/validate-native-install.cmake"
    )
    set_tests_properties(portable-native-install PROPERTIES TIMEOUT 120)
  endif()
  if(NELSON_NODE_EXECUTABLE)
    add_test(
      NAME portable-worker-client
      COMMAND "${NELSON_NODE_EXECUTABLE}" --test
        "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker-client.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-engine-broker.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker-protocol.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-web-request.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-workspace-store.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nmm-browser-bridge.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-runtime.test.mjs"
    )
    add_test(
      NAME portable-test-tools
      COMMAND "${NELSON_NODE_EXECUTABLE}" --test
        "${CMAKE_SOURCE_DIR}/tools/wasm/compare-nflow-trajectories.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/generate-nflow-examples.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/generate-examples-catalog.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/nflow-fmu-runtime.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/generate-runtime-capabilities.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/analyze-browser-package.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/compare-runtime-metrics.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/package-browser.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/package-nflow-fmi.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/mirror-nmm-packages.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/validate-examples.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/validate-static-deployment.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/validate-packaged-deployment.test.mjs"
        "${CMAKE_SOURCE_DIR}/tools/wasm/generate-main-module-exports.test.mjs"
    )
    set_tests_properties(portable-test-tools
      PROPERTIES WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    )
  endif()
  if(NELSON_WASM_BUILD AND NELSON_WASM_MAIN_MODULE AND NELSON_NODE_EXECUTABLE)
    add_test(
      NAME portable-side-module
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/run-side-module.mjs" --module
        "$<TARGET_FILE:nelson-cli-exec>" --library
        "${NELSON_WASM_SIDE_MODULE_FIXTURE}" --code
        "disp(__side_module_fixture_twice__(21)); try, __side_module_fixture_twice__('x'); catch e, disp(e.message); end"
        --expect "42"
    )
    set_tests_properties(portable-side-module PROPERTIES TIMEOUT 120)
  endif()
  if(NELSON_WASM_BUILD AND TARGET nelson-wasm-browser)
    # A build that preloads module tests and help into nelson-portable.data
    # is a validation package, not a deployment: its startup payload is
    # checked against a wider budget than the deployment default.
    set(_nelson_wasm_startup_budget "")
    if(NELSON_PORTABLE_ENABLE_TESTS AND NELSON_WASM_PRELOAD_TEST_HELP)
      set(_nelson_wasm_startup_budget --max-startup-mib 256)
    endif()
    add_test(
      NAME portable-static-deployment
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/validate-packaged-deployment.mjs"
        "${BIN_DIRECTORY}/browser" ${_nelson_wasm_startup_budget}
    )
    set_tests_properties(portable-static-deployment PROPERTIES TIMEOUT 120)
  endif()
  if(NELSON_WASM_BUILD
    AND NELSON_PORTABLE_WITH_NFLOW
    AND NELSON_NPX_EXECUTABLE
  )
    add_test(
      NAME portable-browser
      COMMAND "${NELSON_NPX_EXECUTABLE}" playwright test --config
        playwright.wasm.config.ts
    )
    set_tests_properties(portable-browser
      PROPERTIES
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend"
        ENVIRONMENT "NELSON_WASM_BROWSER_DIR=${BIN_DIRECTORY}/browser"
        TIMEOUT 1200
    )
    add_test(
      NAME portable-runtime-budgets
      COMMAND "${NELSON_NODE_EXECUTABLE}"
        "${CMAKE_SOURCE_DIR}/tools/wasm/measure-runtime.mjs" --module
        "$<TARGET_FILE:nelson-cli-exec>" --browser "${BIN_DIRECTORY}/browser"
        --output "${CMAKE_BINARY_DIR}/wasm-metrics.json" --source
        "${CMAKE_SOURCE_DIR}" --modules-file "${NELSON_GENERATED_MODULES_FILE}"
    )
    set_tests_properties(portable-runtime-budgets PROPERTIES TIMEOUT 120)
  endif()
endif()
