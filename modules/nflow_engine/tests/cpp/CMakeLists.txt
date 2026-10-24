cmake_minimum_required(VERSION 3.16)

project(nflow_model_document_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(NFLOW_ENGINE_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")
set(REPO_MODULES_DIR "${NFLOW_ENGINE_DIR}/..")
set(NFLOW_BLOCKS_DIR "${REPO_MODULES_DIR}/nflow_blocks")

add_executable(nflow_model_document_tests
  model_document_tests.cpp
  "${NFLOW_ENGINE_DIR}/src/cpp/NFlowBlockCatalog.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/NFlowModelDocument.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/NFlowModelRegistry.cpp"
  # NFlowModelDocument::rerouteSystem calls the wire router.
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterGrid.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterCost.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterPathfinder.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterEngine.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterNudge.cpp"
)

target_include_directories(nflow_model_document_tests
  PRIVATE
    "${NFLOW_BLOCKS_DIR}/tests/cpp/third_party/doctest"
    "${NFLOW_ENGINE_DIR}/src/include"
    "${NFLOW_ENGINE_DIR}/src/cpp"
    "${NFLOW_BLOCKS_DIR}/src/include"
    "${REPO_MODULES_DIR}/commons/src/include"
)

target_compile_definitions(nflow_model_document_tests
  PRIVATE
    NLSNFLOW_ENGINE_STATIC
    NLSNFLOW_BLOCKS_STATIC
    "NFLOW_LIBRARIES_DIR=\"${NFLOW_BLOCKS_DIR}/libraries\""
)

if(MSVC)
  target_compile_options(nflow_model_document_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(nflow_model_document_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

add_executable(nflow_dashboard_binding_tests
  dashboard_binding_tests.cpp
  "${NFLOW_ENGINE_DIR}/src/cpp/DashboardBindingManager.cpp"
)

target_include_directories(nflow_dashboard_binding_tests
  PRIVATE
    "${NFLOW_BLOCKS_DIR}/tests/cpp/third_party/doctest"
    "${NFLOW_ENGINE_DIR}/src/include"
    "${NFLOW_BLOCKS_DIR}/src/include"
    "${REPO_MODULES_DIR}/commons/src/include"
)

target_compile_definitions(nflow_dashboard_binding_tests
  PRIVATE
    NLSNFLOW_ENGINE_STATIC
    NLSNFLOW_BLOCKS_STATIC
)

if(MSVC)
  target_compile_options(nflow_dashboard_binding_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(nflow_dashboard_binding_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

# ---------------------------------------------------------------------------
# FMU export tests: golden modelDescription.xml + value-reference assignment.
# ---------------------------------------------------------------------------
add_executable(nflow_fmu_export_tests
  fmu_export_tests.cpp
  "${NFLOW_ENGINE_DIR}/src/cpp/NFlowFmuExport.cpp"
)

target_include_directories(nflow_fmu_export_tests
  PRIVATE
    "${NFLOW_BLOCKS_DIR}/tests/cpp/third_party/doctest"
    "${NFLOW_ENGINE_DIR}/src/include"
)

target_compile_definitions(nflow_fmu_export_tests
  PRIVATE NLSNFLOW_ENGINE_STATIC
)

if(MSVC)
  target_compile_options(nflow_fmu_export_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(nflow_fmu_export_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

# ---------------------------------------------------------------------------
# Router tests: unit tests on computeWireRoutes + parity/golden tests on
# routeWires against the fixtures corpus (shared with the TS router specs).
# ---------------------------------------------------------------------------
add_executable(nflow_router_tests
  router_tests_main.cpp
  router_unit_tests.cpp
  router_hysteresis_tests.cpp
  router_refine_tests.cpp
  router_bench_tests.cpp
  router_nudge_tests.cpp
  router_parity_tests.cpp
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterGrid.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterCost.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterPathfinder.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterEngine.cpp"
  "${NFLOW_ENGINE_DIR}/src/cpp/RouterNudge.cpp"
)

target_include_directories(nflow_router_tests
  PRIVATE
    "${NFLOW_BLOCKS_DIR}/tests/cpp/third_party/doctest"
    "${NFLOW_ENGINE_DIR}/src/include"
    "${NFLOW_ENGINE_DIR}/src/cpp"
    "${REPO_MODULES_DIR}/commons/src/include"
)

target_compile_definitions(nflow_router_tests
  PRIVATE
    NLSNFLOW_ENGINE_STATIC
    "NFLOW_ROUTER_FIXTURES_DIR=\"${CMAKE_CURRENT_LIST_DIR}/fixtures\""
)

if(MSVC)
  target_compile_options(nflow_router_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(nflow_router_tests PRIVATE -Wall -Wextra -Wpedantic)
endif()

# ---------------------------------------------------------------------------
# CMake command tests: the bundled cmake executable must stay quoted so an
# install path with spaces/parentheses does not break the FMU binary build.
# ---------------------------------------------------------------------------
add_executable(nflow_cmake_command_tests
  cmake_command_tests.cpp
)

target_include_directories(nflow_cmake_command_tests
  PRIVATE
    "${NFLOW_BLOCKS_DIR}/tests/cpp/third_party/doctest"
    "${NFLOW_ENGINE_DIR}/src/include"
)

if(MSVC)
  target_compile_options(nflow_cmake_command_tests PRIVATE /W4 /permissive-)
else()
  target_compile_options(nflow_cmake_command_tests
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
  )
endif()

enable_testing()
add_test(NAME nflow_model_document_tests COMMAND nflow_model_document_tests)
add_test(NAME nflow_fmu_export_tests COMMAND nflow_fmu_export_tests)
add_test(NAME nflow_router_tests COMMAND nflow_router_tests)
add_test(NAME nflow_cmake_command_tests COMMAND nflow_cmake_command_tests)
add_test(
  NAME nflow_dashboard_binding_tests
  COMMAND nflow_dashboard_binding_tests
)
