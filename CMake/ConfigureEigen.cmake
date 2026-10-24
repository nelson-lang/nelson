#[=======================================================================[.rst:
Eigen configuration
-------------------

Resolve the Eigen headers for native and WebAssembly builds.
#]=======================================================================]
# -- Eigen3
if(NELSON_WASM_BUILD)
  if(NOT DEFINED EIGEN3_INCLUDE_DIR
    OR NOT EXISTS "${EIGEN3_INCLUDE_DIR}/Eigen/Core"
  )
    # Eigen is header-only.  Look outside the Emscripten sysroot so a regular
    # host installation can be reused by the cross build.
    unset(EIGEN3_INCLUDE_DIR CACHE)
    find_path(EIGEN3_INCLUDE_DIR
      Eigen/Core
      HINTS
      "$ENV{EIGEN3_INCLUDE_DIR}"
      "$ENV{HOMEBREW_PREFIX}/include/eigen3"
      "/opt/homebrew/include/eigen3"
      "/opt/homebrew/opt/eigen/include/eigen3"
      "/usr/local/include/eigen3"
      PATH_SUFFIXES
      eigen3
      NO_CMAKE_FIND_ROOT_PATH
    )
  endif()
  if(NOT EXISTS "${EIGEN3_INCLUDE_DIR}/Eigen/Core")
    message(FATAL_ERROR
      "Eigen3 headers are required for WebAssembly. Set EIGEN3_INCLUDE_DIR."
    )
  endif()
  function(_nelson_check_eigen_blas_abi)
    include(CheckCXXSourceCompiles)
    include(CMakePushCheckState)
    cmake_push_check_state(RESET)
    set(CMAKE_REQUIRED_INCLUDES "${EIGEN3_INCLUDE_DIR}")
    set(CMAKE_REQUIRED_DEFINITIONS -DEIGEN_USE_BLAS)
    # No executable or numerical library is needed to validate declarations.
    set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
    set(_blas_header "${EIGEN3_INCLUDE_DIR}/Eigen/src/misc/blas.h")
    set_property(
      DIRECTORY
      APPEND
      PROPERTY
        CMAKE_CONFIGURE_DEPENDS
        "${_blas_header}"
    )
    file(SHA256 "${_blas_header}" _blas_header_hash)
    set(_blas_check_key "${EIGEN3_INCLUDE_DIR};${_blas_header_hash}")
    if(NOT "${_blas_check_key}" STREQUAL "${NELSON_EIGEN_BLAS_ABI_CHECK_KEY}")
      unset(NELSON_EIGEN_BLAS_VOID_RETURN CACHE)
    endif()
    check_cxx_source_compiles(
      [=[
      #include <Eigen/Core>
      #include <type_traits>
      template <typename Result, typename... Args>
      constexpr bool returnsVoid(Result (*)(Args...))
      {
        return std::is_same<Result, void>::value;
      }
      static_assert(returnsVoid(&sgemv_) && returnsVoid(&dgemv_)
          && returnsVoid(&cgemv_) && returnsVoid(&zgemv_),
          "BLAS matrix-vector subroutines must return void");
      static_assert(returnsVoid(&sgemm_) && returnsVoid(&dgemm_)
          && returnsVoid(&cgemm_) && returnsVoid(&zgemm_),
          "BLAS matrix-matrix subroutines must return void");
      int main() { return 0; }
    ]=]
      NELSON_EIGEN_BLAS_VOID_RETURN
    )
    set(
      NELSON_EIGEN_BLAS_ABI_CHECK_KEY
      "${_blas_check_key}"
      CACHE
        INTERNAL
        "Eigen headers used by the WebAssembly BLAS declaration check"
      FORCE
    )
    cmake_pop_check_state()
    if(NOT NELSON_EIGEN_BLAS_VOID_RETURN)
      message(FATAL_ERROR
        "The selected Eigen headers do not provide the required WebAssembly BLAS ABI. "
        "The s/d/c/z gemv and gemm subroutines must return void; incompatible "
        "declarations can link but trap at runtime. Select Eigen 5.0.1 or newer, "
        "or headers with corrected BLAS declarations, using "
        "-DEIGEN3_INCLUDE_DIR=/absolute/path/to/eigen3. "
        "Current headers: ${EIGEN3_INCLUDE_DIR}"
      )
    endif()
  endfunction()
  _nelson_check_eigen_blas_abi()
  message(STATUS "Using header-only Eigen3 for WebAssembly.")
elseif(WIN32
  AND DEFINED EIGEN3_INCLUDE_DIR
  AND EXISTS "${EIGEN3_INCLUDE_DIR}/Eigen/Core"
)
  message(STATUS
    "Found Eigen3 include directory from Windows third-party root."
  )
else()
  find_package(
    Eigen3
    3.3 QUIET NO_MODULE
  )
  if(NOT Eigen3_FOUND)
    find_package(Eigen3 REQUIRED)
  endif()
  if(TARGET Eigen3::Eigen)
    message(STATUS "Found Eigen3 target: Eigen3::Eigen")
    get_target_property(EIGEN3_INCLUDE_DIR
      Eigen3::Eigen
      INTERFACE_INCLUDE_DIRECTORIES
    )
  elseif(NOT DEFINED EIGEN3_INCLUDE_DIR
    AND DEFINED EIGEN3_INCLUDE_DIRS
  )
    set(EIGEN3_INCLUDE_DIR ${EIGEN3_INCLUDE_DIRS})
  endif()
endif()
message(STATUS "Eigen3 include directory: ${EIGEN3_INCLUDE_DIR}")
