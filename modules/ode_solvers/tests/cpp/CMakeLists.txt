cmake_minimum_required(VERSION 3.16)

project(ode_solvers_tests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Layout: <repo>/modules/ode_solvers/tests/cpp -> repo root is four levels up,
# and the vendored SUNDIALS lives beside the repo as NelSon-thirdparty-<plat>.
set(ODE_DIR "${CMAKE_CURRENT_LIST_DIR}/../..")
get_filename_component(REPO_ROOT "${ODE_DIR}/../.." ABSOLUTE)

if(NOT DEFINED SUNDIALS_ROOT)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(SUNDIALS_ROOT "${REPO_ROOT}/../NelSon-thirdparty-x64/sundials")
  else()
    set(SUNDIALS_ROOT "${REPO_ROOT}/../NelSon-thirdparty-win32/sundials")
  endif()
endif()
get_filename_component(SUNDIALS_ROOT "${SUNDIALS_ROOT}" ABSOLUTE)

add_executable(ode_solvers_tests
  main.cpp
  idas_session_tests.cpp
  sundials_backend_tests.cpp
  "${ODE_DIR}/src/cpp/OdeCore.cpp"
  "${ODE_DIR}/src/cpp/CvodesBackend.cpp"
  "${ODE_DIR}/src/cpp/IdasBackend.cpp"
  "${ODE_DIR}/src/cpp/SundialsSolverBackend.cpp"
)

# Compile the core into the test exe (no DLL): export decoration is harmless and
# NELSON_WITH_SUNDIALS enables the IDAS / CVODES code paths.
target_compile_definitions(ode_solvers_tests
  PRIVATE
    NELSON_WITH_SUNDIALS
    NLSODE_SOLVERS_EXPORTS
)

# Reuse the single vendored doctest header (shared by the nflow_blocks /
# nflow_engine C++ harnesses) instead of copying it per module.
set(DOCTEST_INCLUDE
  "${REPO_ROOT}/modules/nflow_blocks/tests/cpp/third_party/doctest"
)

target_include_directories(ode_solvers_tests
  PRIVATE
    "${ODE_DIR}/src/include"
    # Header-only ISolverBackend / SimDiagnostic contract implemented by the
    # CVODES / IDAS backends (no link to nflow_blocks).
    "${REPO_ROOT}/modules/nflow_blocks/src/include"
    "${DOCTEST_INCLUDE}"
    "${SUNDIALS_ROOT}/include"
)

target_link_directories(ode_solvers_tests PRIVATE "${SUNDIALS_ROOT}/lib")

target_link_libraries(ode_solvers_tests
  PRIVATE
    sundials_core
    sundials_cvodes
    sundials_idas
    sundials_nvecserial
    sundials_sunmatrixdense
    sundials_sunlinsoldense
    sundials_sunnonlinsolnewton
)

# SUNDIALS headers from an MPI-enabled build (Ubuntu, Fedora) pull in <mpi.h>
if(NOT WIN32)
  find_package(MPI COMPONENTS CXX)
  if(MPI_CXX_FOUND)
    target_include_directories(ode_solvers_tests
      PRIVATE ${MPI_CXX_INCLUDE_DIRS}
    )
    target_link_libraries(ode_solvers_tests PRIVATE ${MPI_CXX_LIBRARIES})
  endif()
endif()

enable_testing()
add_test(NAME ode_solvers_tests COMMAND ode_solvers_tests)
