#[=======================================================================[.rst:
BLAS and LAPACK configuration
-----------------------------

Resolve the numerical libraries for supported native architectures and the
WebAssembly cross build.
#]=======================================================================]
# -- BLAS / LAPACK
if(NELSON_WASM_BUILD)
  if(NOT EXISTS "${NELSON_WASM_BLAS_INCLUDE_DIR}/cblas.h")
    message(FATAL_ERROR
      "NELSON_WASM_BLAS_INCLUDE_DIR must contain cblas.h from an Emscripten OpenBLAS build."
    )
  endif()
  if(NOT EXISTS "${NELSON_WASM_BLAS_LIBRARY}")
    message(FATAL_ERROR
      "NELSON_WASM_BLAS_LIBRARY must name a BLAS static library compiled with Emscripten."
    )
  endif()
  if(NELSON_WASM_THREADS
    AND NELSON_WASM_BLAS_LIBRARY MATCHES "/openblas/lib/libopenblas\\.a$"
  )
    message(FATAL_ERROR
      "NELSON_WASM_THREADS requires a pthread OpenBLAS archive. "
      "Run tools/wasm/build-openblas.sh --threads or set "
      "NELSON_WASM_BLAS_LIBRARY to a compatible Emscripten pthread archive."
    )
  endif()
  if(NELSON_WASM_LAPACK_LIBRARY
    AND NOT EXISTS "${NELSON_WASM_LAPACK_LIBRARY}"
  )
    message(FATAL_ERROR
      "NELSON_WASM_LAPACK_LIBRARY does not name an existing Emscripten archive."
    )
  endif()
  set(OpenBLAS_INCLUDE_DIR "${NELSON_WASM_BLAS_INCLUDE_DIR}")
  set(OpenBLAS_LIB "${NELSON_WASM_BLAS_LIBRARY}")
  set(OpenBLAS_FOUND ON)
  include_directories(BEFORE "${OpenBLAS_INCLUDE_DIR}")
  set(NELSON_BLAS_LAPACKX_LIBRARIES)
  if(NELSON_WASM_LAPACK_LIBRARY)
    list(APPEND NELSON_BLAS_LAPACKX_LIBRARIES
      "${NELSON_WASM_LAPACK_LIBRARY}"
    )
  endif()
  list(APPEND NELSON_BLAS_LAPACKX_LIBRARIES "${OpenBLAS_LIB}")
elseif(WIN32
  AND
    (CMAKE_VS_PLATFORM_NAME STREQUAL "ARM64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "^(ARM64|arm64|AARCH64|aarch64)$")
)
  set(_nelson_openblas_arm64_lib
    "${CMAKE_SOURCE_DIR}/../nelson-thirdparty-arm64/openblas/lib/libnlsblaslapack.lib"
  )
  if(NOT EXISTS "${_nelson_openblas_arm64_lib}")
    message(SEND_ERROR
      "Windows ARM64 OpenBLAS wrapper not found: ${_nelson_openblas_arm64_lib}"
    )
  endif()
  set(NELSON_BLAS_LAPACKX_LIBRARIES "${_nelson_openblas_arm64_lib}")
else()
  find_package(MKL)
  if(MKL_FOUND)
    set(NELSON_BLAS_LAPACKX_LIBRARIES ${MKL_LIBRARIES})
  else()
    find_package(OpenBLAS)
    find_package(BLAS)
    find_package(LAPACK)
    if(NOT BLAS_FOUND)
      find_library(NELSON_BLAS_LIBRARY
        NAMES
        blas
        PATHS
        /lib
        /lib64
        /usr/lib
        /usr/lib64
        /usr/local/lib
        /usr/local/lib64
      )
      if(NELSON_BLAS_LIBRARY)
        set(BLAS_FOUND TRUE)
        set(BLAS_LIBRARIES "${NELSON_BLAS_LIBRARY}")
      endif()
    endif()
    if(NOT LAPACK_FOUND)
      find_library(NELSON_LAPACK_LIBRARY
        NAMES
        lapack
        PATHS
        /lib
        /lib64
        /usr/lib
        /usr/lib64
        /usr/local/lib
        /usr/local/lib64
      )
      if(NELSON_LAPACK_LIBRARY)
        set(LAPACK_FOUND TRUE)
        set(LAPACK_LIBRARIES "${NELSON_LAPACK_LIBRARY}")
      endif()
    endif()
    if(OpenBLAS_FOUND)
      if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB})
      else()
        if(FORCE_LIBGFORTRAN_LINK)
          set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB} -lgfortran)
        else()
          set(NELSON_BLAS_LAPACKX_LIBRARIES ${OpenBLAS_LIB})
        endif()
      endif()
    else()
      if(NOT BLAS_FOUND)
        message(SEND_ERROR "BLAS Library required.")
      endif()
      if(NOT LAPACK_FOUND)
        message(SEND_ERROR "LAPACK Library required.")
      endif()
      set(NELSON_BLAS_LAPACKX_LIBRARIES
        ${BLAS_LIBRARIES}
        ${LAPACK_LIBRARIES}
      )
    endif()
  endif()
endif()
message(STATUS
  "NELSON_BLAS_LAPACKX_LIBRARIES: ${NELSON_BLAS_LAPACKX_LIBRARIES}"
)
