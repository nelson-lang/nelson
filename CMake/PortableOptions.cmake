#[=======================================================================[.rst:
Portable build options
----------------------

Declare portable and WebAssembly build options and discover dependency
archives produced by the pinned bootstrap scripts.
#]=======================================================================]
option(NELSON_PORTABLE_BUILD
  "Build the portable static Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_PLOTS
  "Include headless plots in the portable Nelson runtime"
  ON
)
option(NELSON_PORTABLE_WITH_NFLOW
  "Include portable NFlow simulation in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_CONTROL_SYSTEM
  "Include the portable control-system and SLICOT modules in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_ODE_SOLVERS
  "Include portable ODE solvers in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_SUNDIALS
  "Enable the SUNDIALS (CVODES/IDAS) backend in the portable runtime, so NFlow \
DAE/acausal models simulate. Requires a static SUNDIALS built for the target \
(tools/wasm/build-sundials.sh for WebAssembly)."
  OFF
)
option(NELSON_PORTABLE_WITH_NMM
  "Include local NMM package installation in the portable Nelson runtime"
  ON
)
option(NELSON_PORTABLE_WITH_STATISTICS
  "Include random-number generation and statistics in the portable Nelson runtime"
  ON
)
option(NELSON_PORTABLE_WITH_IMAGE_PROCESSING
  "Include portable image-processing functions in the portable Nelson runtime"
  ON
)
option(NELSON_PORTABLE_WITH_OPTIMIZATION
  "Include portable optimization functions in the portable Nelson runtime"
  ON
)
option(NELSON_WASM_BUILD
  "Build the Nelson portable runtime with Emscripten"
  OFF
)
set(_nelson_wasm_native_exceptions_default OFF)
if(NELSON_WASM_BUILD)
  set(_nelson_wasm_native_exceptions_default ON)
endif()
option(NELSON_WASM_NATIVE_EXCEPTIONS
  "Use native WebAssembly exceptions instead of Emscripten JavaScript exception trampolines"
  ${_nelson_wasm_native_exceptions_default}
)
option(NELSON_WASM_MAIN_MODULE
  "Link the WebAssembly engine as an Emscripten main module that can dlopen toolbox side modules (wasm32 packages)"
  OFF
)
option(NELSON_WASM_JSPI
  "Enable experimental WebAssembly JavaScript Promise Integration for selected portable entry points"
  OFF
)
set(
  NELSON_WASM_CLASSIC_RUNTIME_DIR
  ""
  CACHE
    PATH
    "Optional non-JSPI WebAssembly runtime directory used to package Safari-compatible browser engines alongside a JSPI build"
)
option(NELSON_WASM_THREADS
  "Build the WebAssembly runtime with Emscripten pthread support"
  OFF
)
set(
  NELSON_WASM_THREAD_POOL_SIZE
  "navigator.hardwareConcurrency"
  CACHE
    STRING
    "Emscripten pthread pool size for NELSON_WASM_THREADS builds"
)
set(
  NELSON_WASM_STACK_SIZE
  "67108864"
  CACHE
    STRING
    "Emscripten stack size in bytes for the WebAssembly portable runtime"
)
set(
  NELSON_WASM_INITIAL_MEMORY
  ""
  CACHE
    STRING
    "Emscripten initial memory in bytes for the WebAssembly portable runtime; empty uses the Emscripten default, except threaded builds use a Nelson default"
)
option(NELSON_WASM_LZ4
  "Compress the WebAssembly preload data file with Emscripten LZ4"
  ON
)
set(_nelson_wasm_preload_test_help_default ON)
if(NELSON_WASM_BUILD)
  # Keep tests and help in the on-demand resource bundles for browser
  # deployments. This keeps the initial data file small enough for static
  # hosts while test_run and help can still mount them when requested.
  set(_nelson_wasm_preload_test_help_default OFF)
endif()
option(NELSON_WASM_PRELOAD_TEST_HELP
  "Preload module tests and help files into the initial WebAssembly data file"
  ${_nelson_wasm_preload_test_help_default}
)
set(_nelson_portable_with_fftw_default OFF)
set(_nelson_portable_with_signal_processing_default OFF)
if(NELSON_WASM_BUILD)
  set(_nelson_portable_with_fftw_default ON)
  set(_nelson_portable_with_signal_processing_default ON)
endif()
option(NELSON_PORTABLE_WITH_FFTW
  "Include FFTW transforms in the portable Nelson runtime"
  ${_nelson_portable_with_fftw_default}
)
option(NELSON_PORTABLE_WITH_SIGNAL_PROCESSING
  "Include signal-processing functions in the portable Nelson runtime"
  ${_nelson_portable_with_signal_processing_default}
)
option(NELSON_PORTABLE_WITH_GEOMETRY
  "Include geometry functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_HDF5
  "Include HDF5 file functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_NETCDF
  "Include netCDF file functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_MATIO
  "Include MAT file functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_PARQUET
  "Include Apache Arrow/Parquet file functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_XML
  "Include XML and XSLT functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_SPREADSHEET
  "Include spreadsheet and delimited-table functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_AUDIO
  "Include file-based audio functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_PREFERENCES
  "Include get/set preference functions in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_WITH_WEBTOOLS
  "Build the webtools module (webread, websave, webwrite) on the browser network stack (WebAssembly only)"
  OFF
)
option(NELSON_PORTABLE_AUDIO_WITH_FLAC
  "Declare FLAC audio file support in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_AUDIO_WITH_OGG
  "Declare Ogg/Vorbis audio file support in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_AUDIO_WITH_MPEG
  "Declare MPEG audio file support in the portable Nelson runtime"
  OFF
)
option(NELSON_PORTABLE_ENABLE_TESTS
  "Enable the portable runtime smoke tests"
  ON
)
set(
  NELSON_WASM_BLAS_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing cblas.h for the Emscripten OpenBLAS build"
)
set(
  NELSON_WASM_BLAS_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static BLAS library compiled with Emscripten"
)
set(
  NELSON_WASM_LAPACK_LIBRARY
  ""
  CACHE
    FILEPATH
    "Optional separate static LAPACK library compiled with Emscripten"
)
set(
  NELSON_WASM_FFTW_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing fftw3.h for the Emscripten FFTW build"
)
set(
  NELSON_WASM_FFTW_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static double-precision FFTW library compiled with Emscripten"
)
set(
  NELSON_WASM_FFTWF_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static single-precision FFTW library compiled with Emscripten"
)
set(
  NELSON_WASM_QHULL_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing libqhull_r headers for the Emscripten Qhull build"
)
set(
  NELSON_WASM_QHULL_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static Qhull reentrant library compiled with Emscripten"
)
set(
  NELSON_WASM_ZLIB_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing zlib.h for the Emscripten zlib build"
)
set(
  NELSON_WASM_ZLIB_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static zlib library compiled with Emscripten"
)
set(
  NELSON_WASM_HDF5_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing hdf5.h for the Emscripten HDF5 build"
)
set(
  NELSON_WASM_HDF5_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static HDF5 library compiled with Emscripten"
)
set(
  NELSON_WASM_HDF5_HL_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static HDF5 high-level library compiled with Emscripten"
)
set(
  NELSON_WASM_NETCDF_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing netcdf.h for the Emscripten netCDF build"
)
set(
  NELSON_WASM_NETCDF_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static netCDF library compiled with Emscripten"
)
set(
  NELSON_WASM_MATIO_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing matio.h for the Emscripten MATIO build"
)
set(
  NELSON_WASM_MATIO_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static MATIO library compiled with Emscripten"
)
set(
  NELSON_WASM_LIBXML2_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing libxml2 headers for the Emscripten libxml2 build"
)
set(
  NELSON_WASM_LIBXML2_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static libxml2 library compiled with Emscripten"
)
set(
  NELSON_WASM_LIBXSLT_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing libxslt headers for the Emscripten libxslt build"
)
set(
  NELSON_WASM_LIBXSLT_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static libxslt library compiled with Emscripten"
)
set(
  NELSON_WASM_LIBXSLT_EXSLT_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static libexslt library compiled with Emscripten"
)
set(
  NELSON_WASM_SNDFILE_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing sndfile.h for the Emscripten libsndfile build"
)
set(
  NELSON_WASM_SNDFILE_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static libsndfile library compiled with Emscripten"
)
set(
  NELSON_WASM_ARROW_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing Arrow headers for the Emscripten Parquet build"
)
set(
  NELSON_WASM_ARROW_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static Arrow library compiled with Emscripten"
)
set(
  NELSON_WASM_ARROW_BUNDLED_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static Arrow bundled-dependencies library compiled with Emscripten"
)
set(
  NELSON_WASM_PARQUET_INCLUDE_DIR
  ""
  CACHE
    PATH
    "Directory containing Parquet headers for the Emscripten build"
)
set(
  NELSON_WASM_PARQUET_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static Parquet library compiled with Emscripten"
)
set(
  NELSON_WASM_THRIFT_LIBRARY
  ""
  CACHE
    FILEPATH
    "Static Apache Thrift library used by the Emscripten Parquet build"
)
if(NELSON_WASM_BUILD)
  set(_nelson_wasm_dependency_root "${CMAKE_SOURCE_DIR}/temp/wasm-deps")
  set(_nelson_wasm_dependency_suffix "")
  if(NELSON_WASM_THREADS)
    set(_nelson_wasm_dependency_suffix "-pthread")
  endif()
  set(_nelson_wasm_openblas_dependency_dir "openblas")
  if(NELSON_WASM_THREADS)
    set(_nelson_wasm_openblas_dependency_dir "openblas-pthread")
  endif()
  set(_nelson_wasm_openblas_default_include
    "${_nelson_wasm_dependency_root}/${_nelson_wasm_openblas_dependency_dir}/include"
  )
  set(_nelson_wasm_openblas_default_library
    "${_nelson_wasm_dependency_root}/${_nelson_wasm_openblas_dependency_dir}/lib/libopenblas.a"
  )
  set(_nelson_wasm_openblas_other_include
    "${_nelson_wasm_dependency_root}/openblas/include"
  )
  set(_nelson_wasm_openblas_other_library
    "${_nelson_wasm_dependency_root}/openblas/lib/libopenblas.a"
  )
  if(NELSON_WASM_THREADS)
    set(_nelson_wasm_openblas_other_include
      "${_nelson_wasm_dependency_root}/openblas/include"
    )
    set(_nelson_wasm_openblas_other_library
      "${_nelson_wasm_dependency_root}/openblas/lib/libopenblas.a"
    )
  else()
    set(_nelson_wasm_openblas_other_include
      "${_nelson_wasm_dependency_root}/openblas-pthread/include"
    )
    set(_nelson_wasm_openblas_other_library
      "${_nelson_wasm_dependency_root}/openblas-pthread/lib/libopenblas.a"
    )
  endif()
  if((NOT NELSON_WASM_BLAS_INCLUDE_DIR OR NELSON_WASM_BLAS_INCLUDE_DIR STREQUAL "${_nelson_wasm_openblas_other_include}")
    AND EXISTS "${_nelson_wasm_openblas_default_include}/cblas.h"
  )
    set(
      NELSON_WASM_BLAS_INCLUDE_DIR
      "${_nelson_wasm_openblas_default_include}"
      CACHE
        PATH
        "Directory containing cblas.h for the Emscripten OpenBLAS build"
      FORCE
    )
  endif()
  if((NOT NELSON_WASM_BLAS_LIBRARY OR NELSON_WASM_BLAS_LIBRARY STREQUAL "${_nelson_wasm_openblas_other_library}")
    AND EXISTS "${_nelson_wasm_openblas_default_library}"
  )
    set(
      NELSON_WASM_BLAS_LIBRARY
      "${_nelson_wasm_openblas_default_library}"
      CACHE
        FILEPATH
        "Static BLAS library compiled with Emscripten"
      FORCE
    )
  endif()
  set(_nelson_wasm_pcre2_dir "pcre2${_nelson_wasm_dependency_suffix}")
  if(NOT PCRE_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_pcre2_dir}/include/pcre2.h"
  )
    set(
      PCRE_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_pcre2_dir}/include"
      CACHE
        PATH
        "Directory containing pcre2.h for the Emscripten build"
      FORCE
    )
  endif()
  if(NOT PCRE_WIDE_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_pcre2_dir}/lib/libpcre2-32.a"
  )
    set(
      PCRE_WIDE_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_pcre2_dir}/lib/libpcre2-32.a"
      CACHE
        FILEPATH
        "Static PCRE2-32 library compiled with Emscripten"
      FORCE
    )
  endif()
  set(_nelson_wasm_fftw_dir "fftw${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_fftwf_dir "fftwf${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_qhull_dir "qhull${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_qhull_other_dir "")
  if(NELSON_WASM_NATIVE_EXCEPTIONS)
    set(_nelson_wasm_qhull_other_dir "${_nelson_wasm_qhull_dir}")
    set(_nelson_wasm_qhull_dir "${_nelson_wasm_qhull_dir}-wasm-exceptions")
  endif()
  set(_nelson_wasm_zlib_dir "zlib${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_hdf5_dir "hdf5${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_netcdf_dir "netcdf${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_matio_dir "matio${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_libxml2_dir "libxml2${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_libxslt_dir "libxslt${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_sndfile_dir "libsndfile${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_sundials_dir "sundials${_nelson_wasm_dependency_suffix}")
  set(_nelson_wasm_arrow_parquet_dir
    "arrow-parquet${_nelson_wasm_dependency_suffix}"
  )
  if(NOT NELSON_WASM_FFTW_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_fftw_dir}/include/fftw3.h"
  )
    set(
      NELSON_WASM_FFTW_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_fftw_dir}/include"
      CACHE
        PATH
        "Directory containing fftw3.h for the Emscripten FFTW build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_FFTW_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_fftw_dir}/lib/libfftw3.a"
  )
    set(
      NELSON_WASM_FFTW_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_fftw_dir}/lib/libfftw3.a"
      CACHE
        FILEPATH
        "Static double-precision FFTW library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_FFTWF_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_fftwf_dir}/lib/libfftw3f.a"
  )
    set(
      NELSON_WASM_FFTWF_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_fftwf_dir}/lib/libfftw3f.a"
      CACHE
        FILEPATH
        "Static single-precision FFTW library compiled with Emscripten"
      FORCE
    )
  endif()
  if((NOT NELSON_WASM_QHULL_INCLUDE_DIR OR (NELSON_WASM_NATIVE_EXCEPTIONS AND _nelson_wasm_qhull_other_dir AND NELSON_WASM_QHULL_INCLUDE_DIR STREQUAL "${_nelson_wasm_dependency_root}/${_nelson_wasm_qhull_other_dir}/include"))
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_qhull_dir}/include/libqhull_r/libqhull_r.h"
  )
    set(
      NELSON_WASM_QHULL_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_qhull_dir}/include"
      CACHE
        PATH
        "Directory containing libqhull_r headers for the Emscripten Qhull build"
      FORCE
    )
  endif()
  if((NOT NELSON_WASM_QHULL_LIBRARY OR (NELSON_WASM_NATIVE_EXCEPTIONS AND _nelson_wasm_qhull_other_dir AND NELSON_WASM_QHULL_LIBRARY STREQUAL "${_nelson_wasm_dependency_root}/${_nelson_wasm_qhull_other_dir}/lib/libqhull_r.a"))
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_qhull_dir}/lib/libqhull_r.a"
  )
    set(
      NELSON_WASM_QHULL_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_qhull_dir}/lib/libqhull_r.a"
      CACHE
        FILEPATH
        "Static Qhull reentrant library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_ZLIB_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_zlib_dir}/include/zlib.h"
  )
    set(
      NELSON_WASM_ZLIB_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_zlib_dir}/include"
      CACHE
        PATH
        "Directory containing zlib.h for the Emscripten zlib build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_ZLIB_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_zlib_dir}/lib/libz.a"
  )
    set(
      NELSON_WASM_ZLIB_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_zlib_dir}/lib/libz.a"
      CACHE
        FILEPATH
        "Static zlib library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_HDF5_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_hdf5_dir}/include/hdf5.h"
  )
    set(
      NELSON_WASM_HDF5_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_hdf5_dir}/include"
      CACHE
        PATH
        "Directory containing hdf5.h for the Emscripten HDF5 build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_HDF5_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_hdf5_dir}/lib/libhdf5.a"
  )
    set(
      NELSON_WASM_HDF5_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_hdf5_dir}/lib/libhdf5.a"
      CACHE
        FILEPATH
        "Static HDF5 library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_HDF5_HL_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_hdf5_dir}/lib/libhdf5_hl.a"
  )
    set(
      NELSON_WASM_HDF5_HL_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_hdf5_dir}/lib/libhdf5_hl.a"
      CACHE
        FILEPATH
        "Static HDF5 high-level library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_NETCDF_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_netcdf_dir}/include/netcdf.h"
  )
    set(
      NELSON_WASM_NETCDF_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_netcdf_dir}/include"
      CACHE
        PATH
        "Directory containing netcdf.h for the Emscripten netCDF build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_NETCDF_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_netcdf_dir}/lib/libnetcdf.a"
  )
    set(
      NELSON_WASM_NETCDF_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_netcdf_dir}/lib/libnetcdf.a"
      CACHE
        FILEPATH
        "Static netCDF library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_MATIO_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_matio_dir}/include/matio.h"
  )
    set(
      NELSON_WASM_MATIO_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_matio_dir}/include"
      CACHE
        PATH
        "Directory containing matio.h for the Emscripten MATIO build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_MATIO_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_matio_dir}/lib/libmatio.a"
  )
    set(
      NELSON_WASM_MATIO_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_matio_dir}/lib/libmatio.a"
      CACHE
        FILEPATH
        "Static MATIO library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_LIBXML2_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxml2_dir}/include/libxml2/libxml/parser.h"
  )
    set(
      NELSON_WASM_LIBXML2_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxml2_dir}/include/libxml2"
      CACHE
        PATH
        "Directory containing libxml2 headers for the Emscripten libxml2 build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_LIBXML2_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxml2_dir}/lib/libxml2.a"
  )
    set(
      NELSON_WASM_LIBXML2_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxml2_dir}/lib/libxml2.a"
      CACHE
        FILEPATH
        "Static libxml2 library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_LIBXSLT_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxslt_dir}/include/libxslt/xslt.h"
  )
    set(
      NELSON_WASM_LIBXSLT_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxslt_dir}/include"
      CACHE
        PATH
        "Directory containing libxslt headers for the Emscripten libxslt build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_LIBXSLT_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxslt_dir}/lib/libxslt.a"
  )
    set(
      NELSON_WASM_LIBXSLT_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxslt_dir}/lib/libxslt.a"
      CACHE
        FILEPATH
        "Static libxslt library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_LIBXSLT_EXSLT_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxslt_dir}/lib/libexslt.a"
  )
    set(
      NELSON_WASM_LIBXSLT_EXSLT_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_libxslt_dir}/lib/libexslt.a"
      CACHE
        FILEPATH
        "Static libexslt library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_SNDFILE_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_sndfile_dir}/include/sndfile.h"
  )
    set(
      NELSON_WASM_SNDFILE_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_sndfile_dir}/include"
      CACHE
        PATH
        "Directory containing sndfile.h for the Emscripten libsndfile build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_SNDFILE_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_sndfile_dir}/lib/libsndfile.a"
  )
    set(
      NELSON_WASM_SNDFILE_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_sndfile_dir}/lib/libsndfile.a"
      CACHE
        FILEPATH
        "Static libsndfile library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_ARROW_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/include/arrow/api.h"
  )
    set(
      NELSON_WASM_ARROW_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/include"
      CACHE
        PATH
        "Directory containing Arrow headers for the Emscripten Parquet build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_PARQUET_INCLUDE_DIR
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/include/parquet/api/reader.h"
  )
    set(
      NELSON_WASM_PARQUET_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/include"
      CACHE
        PATH
        "Directory containing Parquet headers for the Emscripten build"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_ARROW_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libarrow.a"
  )
    set(
      NELSON_WASM_ARROW_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libarrow.a"
      CACHE
        FILEPATH
        "Static Arrow library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_PARQUET_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libparquet.a"
  )
    set(
      NELSON_WASM_PARQUET_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libparquet.a"
      CACHE
        FILEPATH
        "Static Parquet library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_ARROW_BUNDLED_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libarrow_bundled_dependencies.a"
  )
    set(
      NELSON_WASM_ARROW_BUNDLED_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libarrow_bundled_dependencies.a"
      CACHE
        FILEPATH
        "Static Arrow bundled-dependencies library compiled with Emscripten"
      FORCE
    )
  endif()
  if(NOT NELSON_WASM_THRIFT_LIBRARY
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libthrift.a"
  )
    set(
      NELSON_WASM_THRIFT_LIBRARY
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_arrow_parquet_dir}/lib/libthrift.a"
      CACHE
        FILEPATH
        "Static Apache Thrift library used by the Emscripten Parquet build"
      FORCE
    )
  endif()
  # The SUNDIALS backend (ode_solvers) reads NELSON_SUNDIALS_INCLUDE_DIR and one
  # NELSON_SUNDIALS_<COMPONENT>_LIBRARY per component; point them straight at the
  # Emscripten build under temp/wasm-deps/sundials so no system search is needed.
  if(NELSON_PORTABLE_WITH_SUNDIALS
    AND EXISTS
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_sundials_dir}/include/idas/idas.h"
  )
    set(
      NELSON_SUNDIALS_INCLUDE_DIR
      "${_nelson_wasm_dependency_root}/${_nelson_wasm_sundials_dir}/include"
      CACHE
        PATH
        "SUNDIALS include directory (Emscripten build)"
      FORCE
    )
    foreach(_nelson_sundials_component
      core
      cvodes
      idas
      nvecserial
      sunmatrixdense
      sunlinsoldense
      sunnonlinsolnewton
    )
      string(TOUPPER
        "sundials_${_nelson_sundials_component}"
        _nelson_sundials_var
      )
      set(
        NELSON_${_nelson_sundials_var}_LIBRARY
        "${_nelson_wasm_dependency_root}/${_nelson_wasm_sundials_dir}/lib/libsundials_${_nelson_sundials_component}.a"
        CACHE
          FILEPATH
          "Static sundials_${_nelson_sundials_component} (Emscripten build)"
        FORCE
      )
    endforeach()
  endif()
endif()
