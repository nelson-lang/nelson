#[=======================================================================[.rst:
Portable main target configuration
----------------------------------

Configure the static gateway table, portable resources, and WebAssembly
browser bundle for the main executable.
#]=======================================================================]
if(NELSON_PORTABLE_BUILD)
  target_sources(nelson-cli-exec
    PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/commons/PortableRunner.cpp"
  )
  if(NELSON_PORTABLE_WITH_PLOTS)
    target_sources(nelson-cli-exec
      PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/commons/PortableWebExports.cpp"
    )
    target_include_directories(nelson-cli-exec
      PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}/../graphics/src/include"
        "${CMAKE_CURRENT_SOURCE_DIR}/../graphics_io/src/include"
    )
  endif()
  target_compile_definitions(nelson-cli-exec PRIVATE NELSON_PORTABLE_CLI)
  get_property(_nelson_all_builtins GLOBAL PROPERTY NELSON_ALL_BUILTIN_TARGETS)
  get_property(_nelson_gateway_ids GLOBAL PROPERTY NELSON_STATIC_GATEWAY_IDS)
  list(LENGTH _nelson_all_builtins _nelson_builtin_count)
  list(LENGTH _nelson_gateway_ids _nelson_gateway_id_count)
  if(NOT _nelson_builtin_count EQUAL _nelson_gateway_id_count)
    message(FATAL_ERROR "Static gateway target and identifier lists differ")
  endif()

  set(NELSON_STATIC_GATEWAY_DECLARATIONS "")
  set(NELSON_STATIC_MODULE_REGISTRATIONS "")
  set(NELSON_STATIC_GATEWAY_REGISTRATIONS "")
  set(NELSON_STATIC_GATEWAY_REMOVALS "")
  set(NELSON_STATIC_FUNCTION_PATHS "")
  if(_nelson_builtin_count GREATER 0)
    math(EXPR _nelson_builtin_last "${_nelson_builtin_count} - 1")
    foreach(_index RANGE ${_nelson_builtin_last})
      list(GET _nelson_gateway_ids ${_index} _gateway_id)
      string(APPEND
        NELSON_STATIC_GATEWAY_DECLARATIONS
        "extern \"C\" int AddGateway_${_gateway_id}(void*, const wchar_t*);\n"
        "extern \"C\" bool RemoveGateway_${_gateway_id}(void*, const wchar_t*);\n"
      )
      string(APPEND
        NELSON_STATIC_GATEWAY_REGISTRATIONS
        "    result = AddGateway_${_gateway_id}(eval, L\"static:${_gateway_id}\") && result;\n"
      )
    endforeach()
    foreach(_reverse RANGE ${_nelson_builtin_last})
      math(EXPR _index "${_nelson_builtin_last} - ${_reverse}")
      list(GET _nelson_gateway_ids ${_index} _gateway_id)
      string(APPEND
        NELSON_STATIC_GATEWAY_REMOVALS
        "    result = RemoveGateway_${_gateway_id}(eval, L\"static:${_gateway_id}\") && result;\n"
      )
    endforeach()
  endif()

  get_property(_nelson_selected_modules GLOBAL PROPERTY NELSON_SELECTED_MODULES)
  set(_nelson_runtime_modules)
  foreach(_module IN LISTS _nelson_selected_modules)
    string(TOUPPER "${_module}" _module_upper)
    if(DEFINED WITHOUT_${_module_upper}_MODULE
      AND WITHOUT_${_module_upper}_MODULE
      AND NOT (NELSON_WASM_BUILD AND _module STREQUAL "characters_encoding")
    )
      continue()
    endif()
    list(APPEND _nelson_runtime_modules "${_module}")
  endforeach()
  set(_nelson_runtime_capabilities
    portable
    virtual-filesystem
    blas
    lapack
    evaluate
    files
    portable-engine-api
    persistent-session
    reset
    worker
  )
  set(_nelson_runtime_profile portable)
  if(NELSON_PORTABLE_WITH_PLOTS)
    list(APPEND _nelson_runtime_capabilities plots graphics-io)
  endif()
  if(NELSON_PORTABLE_WITH_NFLOW)
    list(APPEND _nelson_runtime_capabilities nflow)
    set(_nelson_runtime_profile portable-nflow)
  endif()
  if(NELSON_PORTABLE_WITH_CONTROL_SYSTEM)
    list(APPEND _nelson_runtime_capabilities control-system slicot)
  endif()
  if(NELSON_PORTABLE_WITH_ODE_SOLVERS)
    list(APPEND _nelson_runtime_capabilities ode-solvers)
  endif()
  if(NELSON_PORTABLE_WITH_NMM)
    list(APPEND _nelson_runtime_capabilities package-manager-core)
  endif()
  if(NELSON_PORTABLE_WITH_STATISTICS)
    list(APPEND _nelson_runtime_capabilities random statistics)
  endif()
  if(NELSON_PORTABLE_WITH_IMAGE_PROCESSING)
    list(APPEND _nelson_runtime_capabilities image-processing)
  endif()
  if(NELSON_PORTABLE_WITH_OPTIMIZATION)
    list(APPEND _nelson_runtime_capabilities optimization)
  endif()
  if(NELSON_PORTABLE_WITH_FFTW)
    list(APPEND _nelson_runtime_capabilities fftw)
  endif()
  if(NELSON_PORTABLE_WITH_SIGNAL_PROCESSING)
    list(APPEND _nelson_runtime_capabilities signal-processing)
  endif()
  if(NELSON_PORTABLE_WITH_GEOMETRY)
    list(APPEND _nelson_runtime_capabilities geometry)
  endif()
  if(NELSON_PORTABLE_WITH_HDF5)
    list(APPEND _nelson_runtime_capabilities hdf5)
  endif()
  if(NELSON_PORTABLE_WITH_NETCDF)
    list(APPEND _nelson_runtime_capabilities netcdf)
  endif()
  if(NELSON_PORTABLE_WITH_MATIO)
    list(APPEND _nelson_runtime_capabilities matio)
  endif()
  if(NELSON_PORTABLE_WITH_XML)
    list(APPEND _nelson_runtime_capabilities xml help)
  endif()
  if(NELSON_PORTABLE_WITH_SPREADSHEET)
    list(APPEND _nelson_runtime_capabilities spreadsheet)
  endif()
  if(NELSON_PORTABLE_WITH_WEBTOOLS AND NELSON_WASM_BUILD)
    list(APPEND _nelson_runtime_capabilities web)
  endif()
  if(NELSON_PORTABLE_WITH_AUDIO)
    list(APPEND _nelson_runtime_capabilities audio audio-output)
    if(NELSON_PORTABLE_AUDIO_WITH_FLAC)
      list(APPEND _nelson_runtime_capabilities audio-flac)
    endif()
    if(NELSON_PORTABLE_AUDIO_WITH_OGG)
      list(APPEND _nelson_runtime_capabilities audio-ogg)
    endif()
    if(NELSON_PORTABLE_AUDIO_WITH_MPEG)
      list(APPEND _nelson_runtime_capabilities audio-mpeg)
    endif()
  endif()
  if(NELSON_WASM_BUILD AND NELSON_WASM_THREADS)
    list(APPEND _nelson_runtime_capabilities wasm-threads)
  endif()
  if(NELSON_PORTABLE_ENABLE_TESTS)
    find_program(NELSON_NODE_EXECUTABLE node)
    if(NOT NELSON_NODE_EXECUTABLE)
      message(FATAL_ERROR
        "Node.js is required to run portable runtime tests"
      )
    endif()
    string(JOIN "," _nelson_test_modules_csv ${_nelson_runtime_modules})
  endif()

  foreach(_module IN LISTS _nelson_runtime_modules)
    string(APPEND
      NELSON_STATIC_MODULE_REGISTRATIONS
      "    result = RegisterStaticModule(L\"${_module}\") && result;\n"
    )
    if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module}/functions")
      string(APPEND
        NELSON_STATIC_FUNCTION_PATHS
        "    result = RegisterStaticFunctionPath(L\"${_module}\", L\"functions\") && result;\n"
      )
    endif()
  endforeach()
  if(NELSON_PORTABLE_WITH_PLOTS)
    string(APPEND
      NELSON_STATIC_FUNCTION_PATHS
      "    result = RegisterStaticFunctionPath(L\"graphics\", L\"functions/colormaps\") && result;\n"
    )
  endif()
  if(NELSON_WASM_BUILD)
    string(APPEND
      NELSON_STATIC_FUNCTION_PATHS
      "    result = RegisterStaticFunctionPath(L\"wasm\", L\"functions\") && result;\n"
    )
    if(NELSON_PORTABLE_WITH_NMM)
      string(APPEND
        NELSON_STATIC_FUNCTION_PATHS
        "    result = RegisterStaticFunctionPath(L\"nmm_gui\", L\"functions\") && result;\n"
      )
    endif()
  endif()

  file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/generated/main")
  set(_static_gateways_source
    "${CMAKE_BINARY_DIR}/generated/main/StaticGateways.cpp"
  )
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/commons/StaticGateways.cpp.in"
    "${_static_gateways_source}"
    @ONLY
  )
  target_sources(nelson-cli-exec PRIVATE "${_static_gateways_source}")
  target_include_directories(nelson-cli-exec
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
  )
  target_link_libraries(nelson-cli-exec PRIVATE ${_nelson_all_builtins})

  if(NELSON_PORTABLE_ENABLE_TESTS AND NOT NELSON_WASM_BUILD)
    add_executable(nelson-portable-engine-smoke
      "${CMAKE_CURRENT_SOURCE_DIR}/tests/portable_engine_smoke.cpp"
      "${_static_gateways_source}"
    )
    target_include_directories(nelson-portable-engine-smoke
      PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include"
        "${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include"
        "${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include"
    )
    target_link_libraries(nelson-portable-engine-smoke
      PRIVATE
        nlsEngine
        ${_nelson_all_builtins}
    )
    add_test(
      NAME nelson-portable-engine-api
      COMMAND nelson-portable-engine-smoke "${CMAKE_SOURCE_DIR}"
        "${BIN_DIRECTORY}"
    )
    set_tests_properties(nelson-portable-engine-api
      PROPERTIES WORKING_DIRECTORY "${BIN_DIRECTORY}"
    )
  endif()

  add_custom_target(nelson-portable DEPENDS nelson-cli-exec)
  if(TARGET nelson-portable-engine-smoke)
    add_dependencies(nelson-portable nelson-portable-engine-smoke)
  endif()

  if(NELSON_WASM_BUILD)
    set_target_properties(nelson-cli-exec
      PROPERTIES
        OUTPUT_NAME "nelson-portable"
        SUFFIX ".mjs"
    )
    set(_nelson_wasm_exception_runtime_options "")
    if(NOT NELSON_WASM_NATIVE_EXCEPTIONS)
      list(APPEND _nelson_wasm_exception_runtime_options
        "-sDISABLE_EXCEPTION_CATCHING=0"
      )
    endif()
    set(_nelson_wasm_lz4_runtime_options "")
    if(NELSON_WASM_LZ4)
      list(APPEND _nelson_wasm_lz4_runtime_options "-sLZ4=1")
    endif()
    set(_nelson_wasm_thread_runtime_options "")
    if(NELSON_WASM_THREADS)
      list(APPEND _nelson_wasm_thread_runtime_options
        "-pthread"
        "-sPTHREAD_POOL_SIZE=${NELSON_WASM_THREAD_POOL_SIZE}"
      )
    endif()
    set(_nelson_wasm_main_module_runtime_options "")
    if(NELSON_WASM_MAIN_MODULE)
      # MAIN_MODULE=2 keeps dead-code elimination, so every engine symbol a
      # toolbox side module imports is listed in
      # tools/wasm/main-module-exports.txt (regenerated from the side modules
      # by tools/wasm/generate-main-module-exports.mjs) and exported through
      # EXPORTED_FUNCTIONS: linker-level exports alone are dropped by the
      # export dead-code pass of emcc. The stack pointer global is required by
      # every side module and is dropped by that pass as well (Emscripten
      # 6.0.12), so it is listed explicitly.
      set(_nelson_wasm_exports_source
        "${CMAKE_SOURCE_DIR}/tools/wasm/main-module-exports.txt"
      )
      set(_nelson_wasm_exports_json
        "${CMAKE_BINARY_DIR}/generated/wasm-runtime/main-module-exports.json"
      )
      file(STRINGS "${_nelson_wasm_exports_source}"
        _nelson_wasm_export_names
        REGEX "^[^#]"
      )
      set(_nelson_wasm_export_entries "\"_main\"" "\"___stack_pointer\"")
      foreach(_nelson_wasm_export_name IN LISTS _nelson_wasm_export_names)
        list(APPEND _nelson_wasm_export_entries
          "\"_${_nelson_wasm_export_name}\""
        )
      endforeach()
      list(JOIN _nelson_wasm_export_entries "," _nelson_wasm_export_entries)
      file(WRITE "${_nelson_wasm_exports_json}"
        "[${_nelson_wasm_export_entries}]\n"
      )
      set_property(
        DIRECTORY
        APPEND
        PROPERTY
          CMAKE_CONFIGURE_DEPENDS
          "${_nelson_wasm_exports_source}"
      )
      list(APPEND _nelson_wasm_main_module_runtime_options
        "-sMAIN_MODULE=2"
        "-sEXPORTED_FUNCTIONS=@${_nelson_wasm_exports_json}"
      )
    endif()
    # Build settings a wasm32 toolbox (Emscripten side module) must share with
    # the engine; consumed by dlgeneratemake through NELSON_DL_TARGET=wasm32 so
    # the flags cannot drift from the ones used here.
    set(_nelson_wasm_side_module_config
      "${CMAKE_BINARY_DIR}/generated/wasm-runtime/side-module.cmake"
    )
    set(_nelson_wasm_side_module_compile_options
      -O3
      -DNDEBUG
      -fPIC
      -fvisibility=hidden
      -fvisibility-inlines-hidden
      ${_nelson_wasm_exception_flag}
      -msimd128
    )
    set(_nelson_wasm_side_module_link_options
      -O3
      -sSIDE_MODULE=2
      ${_nelson_wasm_exception_flag}
      -msimd128
      -Wl,--export-dynamic
    )
    file(WRITE "${_nelson_wasm_side_module_config}"
      "# Generated by the Nelson WebAssembly build. Do not edit.\n"
      "set(NELSON_WASM_EMSCRIPTEN_VERSION \"${EMSCRIPTEN_VERSION}\")\n"
      "set(NELSON_WASM_TOOLCHAIN_FILE \"${CMAKE_TOOLCHAIN_FILE}\")\n"
      "set(NELSON_WASM_SIDE_MODULE_INCLUDE_DIRS \"${NELSON_GENERATED_INCLUDE_DIR}\")\n"
      "set(NELSON_WASM_SIDE_MODULE_COMPILE_OPTIONS \"${_nelson_wasm_side_module_compile_options}\")\n"
      "set(NELSON_WASM_SIDE_MODULE_LINK_OPTIONS \"${_nelson_wasm_side_module_link_options}\")\n"
    )
    if(NELSON_WASM_MAIN_MODULE)
      # The side-module fixture is built with the shared settings above and
      # loaded by the portable-side-module test.
      set(_nelson_wasm_fixture_build "${CMAKE_BINARY_DIR}/side-module-fixture")
      set(NELSON_WASM_SIDE_MODULE_FIXTURE
        "${_nelson_wasm_fixture_build}/libnlsSide_module_fixture_builtin.so"
      )
      add_custom_command(
        OUTPUT "${NELSON_WASM_SIDE_MODULE_FIXTURE}"
        COMMAND "${CMAKE_COMMAND}" -S
          "${CMAKE_SOURCE_DIR}/tools/wasm/side-module-fixture" -B
          "${_nelson_wasm_fixture_build}" -G "${CMAKE_GENERATOR}"
          "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}"
          "-DNELSON_SOURCE_DIR=${CMAKE_SOURCE_DIR}"
          "-DNELSON_WASM_SIDE_MODULE_CONFIG=${_nelson_wasm_side_module_config}"
        COMMAND "${CMAKE_COMMAND}" --build "${_nelson_wasm_fixture_build}"
        DEPENDS
          "${CMAKE_SOURCE_DIR}/tools/wasm/side-module-fixture/Gateway.cpp"
          "${CMAKE_SOURCE_DIR}/tools/wasm/side-module-fixture/CMakeLists.txt"
          "${_nelson_wasm_side_module_config}"
        COMMENT "Building the WebAssembly side-module fixture"
        VERBATIM
      )
      add_custom_target(nelson-wasm-side-module-fixture
        DEPENDS "${NELSON_WASM_SIDE_MODULE_FIXTURE}"
      )
      add_dependencies(nelson-portable nelson-wasm-side-module-fixture)
    endif()
    set(_nelson_wasm_jspi_runtime_options "")
    if(NELSON_WASM_JSPI)
      list(APPEND _nelson_wasm_jspi_runtime_options
        "-sJSPI=1"
        "-sJSPI_EXPORTS=nlsPortableStart,nlsPortableLoadUserModules,nlsPortableEvaluate,nlsPortablePump"
      )
    endif()
    set(_nelson_wasm_memory_runtime_options
      "-sSTACK_SIZE=${NELSON_WASM_STACK_SIZE}"
    )
    if(NELSON_WASM_INITIAL_MEMORY)
      list(APPEND _nelson_wasm_memory_runtime_options
        "-sINITIAL_MEMORY=${NELSON_WASM_INITIAL_MEMORY}"
      )
    elseif(NELSON_WASM_THREADS)
      list(APPEND _nelson_wasm_memory_runtime_options
        "-sINITIAL_MEMORY=268435456"
      )
    endif()
    target_link_options(nelson-cli-exec
      PRIVATE
        "-O3"
        "-sMODULARIZE=1"
        "-sEXPORT_ES6=1"
        "-sEXPORT_NAME=createNelsonPortable"
        "-sENVIRONMENT=web,worker,node"
        "-sALLOW_MEMORY_GROWTH=1"
        ${_nelson_wasm_memory_runtime_options}
        "-sFORCE_FILESYSTEM=1"
        "-sEXIT_RUNTIME=0"
        "-sWASM_BIGINT=1"
        ${_nelson_wasm_exception_runtime_options}
        ${_nelson_wasm_lz4_runtime_options}
        ${_nelson_wasm_thread_runtime_options}
        ${_nelson_wasm_jspi_runtime_options}
        ${_nelson_wasm_main_module_runtime_options}
        "-sEXPORTED_RUNTIME_METHODS=FS,callMain,ccall"
        "-sINCOMING_MODULE_JS_API=ENVIRONMENT,arguments,canvas,dynamicLibraries,elementPointerLock,instantiateWasm,locateFile,monitorRunDependencies,noExitRuntime,noInitialRun,onAbort,onExit,onRuntimeInitialized,postRun,preInit,preRun,print,printErr,setStatus,statusMessage,stderr,stdin,stdout,thisProgram,wasm,wasmBinary,websocket"
    )
    find_program(NELSON_NPM_EXECUTABLE npm)
    find_program(NELSON_NPX_EXECUTABLE npx)
    if(NELSON_NPM_EXECUTABLE)
      if(NOT NELSON_NODE_EXECUTABLE)
        find_program(NELSON_NODE_EXECUTABLE node REQUIRED)
      endif()
      set(_nelson_wasm_example_capabilities
        ${_nelson_runtime_capabilities}
      )
      if(NELSON_PORTABLE_WITH_PLOTS)
        list(APPEND _nelson_wasm_example_capabilities gui web_gui)
      endif()
      string(JOIN "," _nelson_wasm_example_modules ${_nelson_runtime_modules})
      string(JOIN
        ","
        _nelson_wasm_example_capabilities_csv
        ${_nelson_wasm_example_capabilities}
      )
      set(_nelson_wasm_examples_dir
        "${CMAKE_BINARY_DIR}/generated/wasm-examples"
      )
      set_property(
        DIRECTORY
        APPEND
        PROPERTY
          CMAKE_CONFIGURE_DEPENDS
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-examples-catalog.mjs"
      )
      execute_process(
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-examples-catalog.mjs"
          --source-root "${CMAKE_SOURCE_DIR}" --output-root
          "${_nelson_wasm_examples_dir}" --modules
          "${_nelson_wasm_example_modules}" --capabilities
          "${_nelson_wasm_example_capabilities_csv}"
        RESULT_VARIABLE _nelson_wasm_examples_status
        OUTPUT_VARIABLE _nelson_wasm_examples_output
        ERROR_VARIABLE _nelson_wasm_examples_error
      )
      if(NOT _nelson_wasm_examples_status EQUAL 0)
        message(FATAL_ERROR
          "WebAssembly examples catalog generation failed:\n${_nelson_wasm_examples_error}"
        )
      endif()
      string(STRIP
        "${_nelson_wasm_examples_output}"
        _nelson_wasm_examples_output
      )
      message(STATUS "${_nelson_wasm_examples_output}")
      set(_nelson_wasm_capabilities_file
        "${CMAKE_BINARY_DIR}/generated/wasm-runtime/capabilities.json"
      )
      set_property(
        DIRECTORY
        APPEND
        PROPERTY
          CMAKE_CONFIGURE_DEPENDS
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-runtime-capabilities.mjs"
      )
      execute_process(
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-runtime-capabilities.mjs"
          --output "${_nelson_wasm_capabilities_file}" --profile
          "${_nelson_runtime_profile}" --modules
          "${_nelson_wasm_example_modules}" --capabilities
          "${_nelson_wasm_example_capabilities_csv}" --native-exceptions
          "${NELSON_WASM_NATIVE_EXCEPTIONS}" --threads "${NELSON_WASM_THREADS}"
          --main-module "${NELSON_WASM_MAIN_MODULE}"
        RESULT_VARIABLE _nelson_wasm_capabilities_status
        OUTPUT_VARIABLE _nelson_wasm_capabilities_output
        ERROR_VARIABLE _nelson_wasm_capabilities_error
      )
      if(NOT _nelson_wasm_capabilities_status EQUAL 0)
        message(FATAL_ERROR
          "WebAssembly capability manifest generation failed:\n${_nelson_wasm_capabilities_error}"
        )
      endif()
      string(STRIP
        "${_nelson_wasm_capabilities_output}"
        _nelson_wasm_capabilities_output
      )
      message(STATUS "${_nelson_wasm_capabilities_output}")
      # The catalog generator copies every file of a module's examples
      # directory, so any of them must retrigger the browser bundle.
      file(GLOB_RECURSE _nelson_wasm_example_sources
        CONFIGURE_DEPENDS
        "${CMAKE_SOURCE_DIR}/modules/*/examples/*"
      )
      set(_nelson_wasm_module_resource_package_arguments "")
      set(_nelson_wasm_module_resource_dependencies "")
      set(_nelson_wasm_help_archives_dir
        "${CMAKE_BINARY_DIR}/generated/wasm-help-archives"
      )
      if(NOT NELSON_WASM_PRELOAD_TEST_HELP)
        list(APPEND _nelson_wasm_module_resource_package_arguments
          --module-resource-root
          "${CMAKE_SOURCE_DIR}"
          --module-resource-modules
          "${_nelson_wasm_example_modules}"
          --module-resource-help-archives-root
          "${_nelson_wasm_help_archives_dir}"
        )
        if(NELSON_PORTABLE_WITH_NFLOW)
          list(APPEND _nelson_wasm_module_resource_package_arguments
            --module-resource-include-nflow-gui
            "1"
          )
        endif()
        foreach(_module IN LISTS _nelson_runtime_modules)
          foreach(_resource_dir IN ITEMS tests help)
            if(EXISTS "${CMAKE_SOURCE_DIR}/modules/${_module}/${_resource_dir}")
              file(GLOB_RECURSE _nelson_wasm_module_resource_files
                CONFIGURE_DEPENDS
                "${CMAKE_SOURCE_DIR}/modules/${_module}/${_resource_dir}/*"
              )
              list(APPEND _nelson_wasm_module_resource_dependencies
                ${_nelson_wasm_module_resource_files}
              )
            endif()
          endforeach()
        endforeach()
        if(NELSON_PORTABLE_WITH_NFLOW
          AND EXISTS "${CMAKE_SOURCE_DIR}/modules/nflow_gui/tests"
        )
          file(GLOB_RECURSE _nelson_wasm_nflow_gui_test_files
            CONFIGURE_DEPENDS
            "${CMAKE_SOURCE_DIR}/modules/nflow_gui/tests/*"
          )
          list(APPEND _nelson_wasm_module_resource_dependencies
            ${_nelson_wasm_nflow_gui_test_files}
          )
        endif()
      endif()
      if(NELSON_WASM_PRELOAD_TEST_HELP AND NOT WITHOUT_HELP_TOOLS_MODULE)
        list(APPEND _nelson_wasm_module_resource_package_arguments
          --module-resource-root
          "${CMAKE_SOURCE_DIR}"
          --module-resource-modules
          "help_tools"
          --module-resource-help-archives-root
          "${_nelson_wasm_help_archives_dir}"
        )
        if(EXISTS "${CMAKE_SOURCE_DIR}/modules/help_tools/help")
          file(GLOB_RECURSE _nelson_wasm_help_tools_help_files
            CONFIGURE_DEPENDS
            "${CMAKE_SOURCE_DIR}/modules/help_tools/help/*"
          )
          list(APPEND _nelson_wasm_module_resource_dependencies
            ${_nelson_wasm_help_tools_help_files}
          )
        endif()
      endif()
      set(_nelson_wasm_buildhelpjson_stamp "")
      set(_nelson_wasm_buildhelpjson_dependencies "")
      if(NOT WITHOUT_HELP_TOOLS_MODULE)
        set(_nelson_wasm_buildhelpjson_stamp
          "${CMAKE_BINARY_DIR}/generated/wasm-buildhelpjson.stamp"
        )
        file(GLOB_RECURSE _nelson_wasm_help_xml_sources
          CONFIGURE_DEPENDS
          "${CMAKE_SOURCE_DIR}/modules/*/help/*.xml"
          "${CMAKE_SOURCE_DIR}/modules/help_tools/resources/*"
        )
        list(APPEND _nelson_wasm_buildhelpjson_dependencies
          ${_nelson_wasm_help_xml_sources}
          "${CMAKE_SOURCE_DIR}/tools/wasm/run-buildhelpjson.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-runtime.mjs"
        )
        add_custom_command(
          OUTPUT "${_nelson_wasm_buildhelpjson_stamp}"
          COMMAND "${NELSON_NODE_EXECUTABLE}"
            "${CMAKE_SOURCE_DIR}/tools/wasm/run-buildhelpjson.mjs" --module
            "$<TARGET_FILE:nelson-cli-exec>" --source "${CMAKE_SOURCE_DIR}"
            --output "${CMAKE_SOURCE_DIR}/modules/help_tools/help"
            --archives-output "${_nelson_wasm_help_archives_dir}"
          COMMAND "${CMAKE_COMMAND}" -E touch
            "${_nelson_wasm_buildhelpjson_stamp}"
          DEPENDS
            nelson-cli-exec
            ${_nelson_wasm_buildhelpjson_dependencies}
          COMMENT "Building WebAssembly help JSON index"
          VERBATIM
        )
      endif()
      include("${CMAKE_SOURCE_DIR}/CMake/PortableNflowBrowser.cmake")
      set(_nelson_wasm_nmm_enabled 0)
      set(_nelson_wasm_nmm_build_commands "")
      set(_nelson_wasm_nmm_package_arguments "")
      set(_nelson_wasm_nmm_dependencies "")
      if(NELSON_PORTABLE_WITH_NMM)
        set(_nelson_wasm_nmm_enabled 1)
        file(GLOB_RECURSE _nelson_wasm_nmm_sources
          CONFIGURE_DEPENDS
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend/src/*"
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/locale/*"
        )
        list(APPEND _nelson_wasm_nmm_build_commands
          COMMAND
          "${CMAKE_COMMAND}"
          -E
          chdir
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend"
          "${NELSON_NPM_EXECUTABLE}"
          ci
          --ignore-scripts
          COMMAND
          "${CMAKE_COMMAND}"
          -E
          chdir
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend"
          "${NELSON_NPM_EXECUTABLE}"
          run
          build:wasm
        )
        list(APPEND _nelson_wasm_nmm_package_arguments
          --nmm
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend/dist-wasm"
          --nmm-locales
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/locale"
          --nmm-adapter
          "${CMAKE_SOURCE_DIR}/tools/wasm/nmm-browser-bridge.mjs"
        )
        list(APPEND _nelson_wasm_nmm_dependencies
          ${_nelson_wasm_nmm_sources}
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend/package.json"
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend/package-lock.json"
          "${CMAKE_SOURCE_DIR}/modules/nmm_gui/frontend/vite.wasm.config.ts"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nmm-browser-bridge.mjs"
        )
      endif()
      set(_nelson_wasm_runtime_package_arguments "")
      if(NELSON_WASM_JSPI AND NELSON_WASM_CLASSIC_RUNTIME_DIR)
        list(APPEND _nelson_wasm_runtime_package_arguments
          --runtime-jspi
          "${BIN_DIRECTORY}"
          --runtime-classic
          "${NELSON_WASM_CLASSIC_RUNTIME_DIR}"
        )
      endif()
      file(GLOB_RECURSE _nelson_wasm_frontend_sources
        CONFIGURE_DEPENDS
        "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend/src/*"
        "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend/scripts/*"
      )
      set(_nelson_wasm_browser_stamp
        "${CMAKE_BINARY_DIR}/generated/wasm-browser.stamp"
      )
      add_custom_command(
        OUTPUT
          "${_nelson_wasm_browser_stamp}"
          # Regenerate the examples catalog at build time: the configure-time
          # run above only reflects the sources as they were when CMake ran,
          # and package-browser.mjs copies the examples from that directory.
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-examples-catalog.mjs"
          --source-root "${CMAKE_SOURCE_DIR}" --output-root
          "${_nelson_wasm_examples_dir}" --modules
          "${_nelson_wasm_example_modules}" --capabilities
          "${_nelson_wasm_example_capabilities_csv}"
        COMMAND "${NELSON_NPM_EXECUTABLE}" ci --ignore-scripts
        COMMAND "${CMAKE_COMMAND}" -E env
          "NELSON_WASM_NFLOW=${_nelson_wasm_nflow_enabled}"
          "NELSON_WASM_NMM=${_nelson_wasm_nmm_enabled}"
          "${NELSON_NPM_EXECUTABLE}" run build:wasm
          ${_nelson_wasm_nflow_build_commands}
          ${_nelson_wasm_nmm_build_commands}
        COMMAND "${NELSON_NODE_EXECUTABLE}"
          "${CMAKE_SOURCE_DIR}/tools/wasm/package-browser.mjs" --frontend
          "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend/dist-wasm" --runtime
          "${BIN_DIRECTORY}" ${_nelson_wasm_runtime_package_arguments}
          --adapters "${CMAKE_SOURCE_DIR}/tools/wasm" --output
          "${BIN_DIRECTORY}/browser" --examples "${_nelson_wasm_examples_dir}"
          --examples-ui "${CMAKE_SOURCE_DIR}/modules/webview/frontend/examples"
          --capabilities "${_nelson_wasm_capabilities_file}" --pwa-icon
          "${CMAKE_SOURCE_DIR}/desktop/icons/hicolor/512x512/apps/nelson.png"
          ${_nelson_wasm_module_resource_package_arguments}
          ${_nelson_wasm_nflow_package_arguments}
          ${_nelson_wasm_nmm_package_arguments}
        COMMAND "${CMAKE_COMMAND}" -E touch "${_nelson_wasm_browser_stamp}"
        DEPENDS
          nelson-cli-exec
          ${_nelson_wasm_frontend_sources}
          "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend/package.json"
          "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend/package-lock.json"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-runtime.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-coi-serviceworker.js"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker-client.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker-protocol.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-workspace-store.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nflow-fmu-runtime.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/nflow-worker.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/package-browser.mjs"
          ${_nelson_wasm_buildhelpjson_stamp}
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-examples-catalog.mjs"
          "${CMAKE_SOURCE_DIR}/tools/wasm/generate-runtime-capabilities.mjs"
          "${CMAKE_SOURCE_DIR}/desktop/icons/hicolor/512x512/apps/nelson.png"
          "${_nelson_wasm_capabilities_file}"
          ${_nelson_wasm_example_sources}
          ${_nelson_wasm_module_resource_dependencies}
          ${_nelson_wasm_nflow_dependencies}
          ${_nelson_wasm_nmm_dependencies}
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/web_gui/frontend"
        COMMENT "Building the Nelson WebAssembly browser bundle"
        VERBATIM
      )
      add_custom_target(nelson-wasm-browser
        DEPENDS "${_nelson_wasm_browser_stamp}"
      )
    else()
      message(WARNING
        "npm was not found; the nelson-wasm-browser target is unavailable"
      )
    endif()

    function(_nelson_wasm_preload _source _destination)
      if(EXISTS "${_source}")
        target_link_options(nelson-cli-exec
          PRIVATE "SHELL:--preload-file \"${_source}@${_destination}\""
        )
        if(IS_DIRECTORY "${_source}")
          file(GLOB_RECURSE _nelson_preload_dependencies
            CONFIGURE_DEPENDS
            "${_source}/*"
          )
          set_property(
            TARGET nelson-cli-exec
            APPEND
            PROPERTY
              LINK_DEPENDS
              ${_nelson_preload_dependencies}
          )
        else()
          set_property(
            TARGET nelson-cli-exec
            APPEND
            PROPERTY
              LINK_DEPENDS
              "${_source}"
          )
        endif()
      endif()
    endfunction()

    _nelson_wasm_preload(
      "${NELSON_GENERATED_MODULES_FILE}"
      "/modules/modules.m"
    )
    foreach(_nelson_root_file
      IN
      ITEMS
      "README.md"
      "gpl-3.0.md"
      "lgpl-3.0.md"
      "LICENSE"
    )
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/${_nelson_root_file}"
        "/${_nelson_root_file}"
      )
    endforeach()
    foreach(_nelson_etc_file
      IN
      ITEMS
      "defaults.conf"
      "finish.m"
      "gallery_requirements.json"
      "startup.m"
    )
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/etc/${_nelson_etc_file}"
        "/etc/${_nelson_etc_file}"
      )
    endforeach()
    if(NOT WITHOUT_LOCALIZATION_MODULE)
      file(GLOB _nelson_locale_catalogs
        CONFIGURE_DEPENDS
        "${CMAKE_SOURCE_DIR}/locale/nelson-ui-*.json"
        "${CMAKE_SOURCE_DIR}/locale/nelson-errors-*.json"
      )
      foreach(_catalog IN LISTS _nelson_locale_catalogs)
        get_filename_component(_catalog_name "${_catalog}" NAME)
        _nelson_wasm_preload("${_catalog}" "/locale/${_catalog_name}")
      endforeach()
    endif()
    foreach(_module IN LISTS _nelson_runtime_modules)
      foreach(_resource_dir IN ITEMS etc functions resources)
        if(NELSON_WASM_BUILD
          AND _module STREQUAL "text_editor"
          AND _resource_dir STREQUAL "resources"
        )
          # These files are native editor toolbar icons. The browser editor
          # ships its own UI assets in the lazy-loaded frontend pack, so keeping
          # the native resources in nelson-portable.data only increases startup
          # size.
          continue()
        endif()
        _nelson_wasm_preload(
          "${CMAKE_SOURCE_DIR}/modules/${_module}/${_resource_dir}"
          "/modules/${_module}/${_resource_dir}"
        )
      endforeach()
    endforeach()
    if("help_tools" IN_LIST _nelson_runtime_modules)
      # The help templates of the main module (summary header, home page) are
      # read by buildhelp when a toolbox is installed; main itself is not a
      # runtime module, so its help tree is packaged explicitly.
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/modules/main/help"
        "/modules/main/help"
      )
    endif()
    if(NELSON_PORTABLE_ENABLE_TESTS AND NELSON_WASM_PRELOAD_TEST_HELP)
      foreach(_module IN LISTS _nelson_runtime_modules)
        _nelson_wasm_preload(
          "${CMAKE_SOURCE_DIR}/modules/${_module}/tests"
          "/modules/${_module}/tests"
        )
        _nelson_wasm_preload(
          "${CMAKE_SOURCE_DIR}/modules/${_module}/help"
          "/modules/${_module}/help"
        )
      endforeach()
    endif()
    if(NELSON_PORTABLE_WITH_NFLOW)
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/modules/nflow_blocks/libraries"
        "/modules/nflow_blocks/libraries"
      )
      if(NELSON_PORTABLE_ENABLE_TESTS AND NELSON_WASM_PRELOAD_TEST_HELP)
        _nelson_wasm_preload(
          "${CMAKE_SOURCE_DIR}/modules/nflow_gui/tests"
          "/modules/nflow_gui/tests"
        )
      endif()
    endif()
    if(NELSON_PORTABLE_ENABLE_TESTS)
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/tools/wasm/portable_smoke.m"
        "/tests/portable_smoke.m"
      )
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/tools/wasm/test_run_smoke.m"
        "/tests/test_run_smoke.m"
      )
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/tools/wasm/bench_run_smoke.m"
        "/tests/bench_run_smoke.m"
      )
    endif()
    _nelson_wasm_preload(
      "${CMAKE_SOURCE_DIR}/tools/wasm/functions"
      "/modules/wasm/functions"
    )
    if(NELSON_PORTABLE_WITH_NMM)
      _nelson_wasm_preload(
        "${CMAKE_SOURCE_DIR}/modules/nmm_gui/functions"
        "/modules/nmm_gui/functions"
      )
    endif()
  endif()
endif()
