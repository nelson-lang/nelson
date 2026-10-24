#[=======================================================================[.rst:
Portable module profile
-----------------------

Apply the positive portable module boundary and WebAssembly compiler options.
#]=======================================================================]
# Portable build
# ==============================================================================
if(NELSON_WASM_BUILD)
  set(
    NELSON_PORTABLE_BUILD
    ON
    CACHE
      BOOL
      "Build the portable static Nelson runtime"
    FORCE
  )
endif()

if(NELSON_PORTABLE_BUILD)
  message(STATUS "Nelson portable build enabled.")
  set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libraries" FORCE)
  set(WITHOUT_FILEWATCHER ON)
  set(WITHOUT_OPENMP ON)
  set(WITHOUT_TBB ON)
  set(
    WITH_WEB_GUI_MODULE
    OFF
    CACHE
      BOOL
      "Build the WEB_GUI module and the nelson-webview executable"
    FORCE
  )
  set(
    WITH_GPU_ENGINE_MODULE
    OFF
    CACHE
      BOOL
      "Build the optional GPU (WebGPU/Dawn) compute module"
    FORCE
  )
  set(
    WITHOUT_COMPILER_MODULE
    ON
    CACHE
      BOOL
      "Build without the optional application compiler"
    FORCE
  )
  foreach(_mod IN LISTS NELSON_OPTIONAL_MODULES)
    set(WITHOUT_${_mod}_MODULE ON)
  endforeach()

  # The interpreter always needs real text conversion and message templates.
  # Their fallback libraries only preserve the ABI for native reduced builds.
  set(WITHOUT_CHARACTERS_ENCODING_MODULE OFF)
  set(WITHOUT_LOCALIZATION_MODULE OFF)
  # categorical is part of the portable base list (table/data-analysis
  # overloads, histogram, bar); its loader must be registered as well.
  set(WITHOUT_CATEGORICAL_MODULE OFF)

  if(NELSON_WASM_BUILD)
    # Keep the ABI-only conversion fallback until ICU is available as an
    # Emscripten static dependency. The module data is still packaged by the
    # portable target so tests and stream fixtures can resolve modulepath().
    set(WITHOUT_CHARACTERS_ENCODING_MODULE ON)
    set(WITHOUT_TEXT_EDITOR_MODULE OFF)
    set(WITHOUT_GIF ON)
    set(WITHOUT_TIFF ON)
    if(NELSON_WASM_THREADS)
      add_compile_options("-pthread")
      add_link_options("-pthread")
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_PLOTS)
    set(WITHOUT_GRAPHICS_MODULE OFF)
    set(WITHOUT_GRAPHICS_IO_MODULE OFF)
    set(WITHOUT_JSON_MODULE OFF)
    set(WITHOUT_SPECIAL_FUNCTIONS_MODULE OFF)
    set(WITHOUT_TRIGONOMETRIC_FUNCTIONS_MODULE OFF)
  endif()

  if(NELSON_PORTABLE_WITH_NFLOW)
    set(WITHOUT_NFLOW_BLOCKS_MODULE OFF)
    set(WITHOUT_NFLOW_ENGINE_MODULE OFF)
    if(NELSON_WASM_BUILD)
      set(WITHOUT_NFLOW_FMI_MODULE OFF)
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_CONTROL_SYSTEM)
    set(WITHOUT_F2C_MODULE OFF)
    set(WITHOUT_SLICOT_MODULE OFF)
    set(WITHOUT_CONTROL_SYSTEM_MODULE OFF)
    set(WITHOUT_DATA_ANALYSIS_MODULE OFF)
    set(WITHOUT_POLYNOMIAL_FUNCTIONS_MODULE OFF)
    set(WITHOUT_VALIDATORS_MODULE OFF)
  endif()

  if(NELSON_PORTABLE_WITH_ODE_SOLVERS)
    # ode_solvers shares its optional backend registry with nflow_blocks.
    set(WITHOUT_NFLOW_BLOCKS_MODULE OFF)
    set(WITHOUT_ODE_SOLVERS_MODULE OFF)
    # SUNDIALS (CVODES/IDAS) enables the DAE backend that NFlow acausal models
    # need. It is opt-in (NELSON_PORTABLE_WITH_SUNDIALS) because it requires a
    # static SUNDIALS built for the target; otherwise only the built-in portable
    # RHS solvers are part of this boundary, aligned between native and wasm.
    if(NELSON_PORTABLE_WITH_SUNDIALS)
      set(
        NELSON_SUNDIALS_MODE
        ON
        CACHE
          STRING
          "SUNDIALS backend mode for ode_solvers: AUTO, ON, or OFF"
        FORCE
      )
    else()
      set(
        NELSON_SUNDIALS_MODE
        OFF
        CACHE
          STRING
          "SUNDIALS backend mode for ode_solvers: AUTO, ON, or OFF"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_NMM)
    set(WITHOUT_FILE_ARCHIVER_MODULE OFF)
    set(WITHOUT_JSON_MODULE OFF)
  endif()

  if(NELSON_PORTABLE_WITH_STATISTICS)
    set(WITHOUT_RANDOM_MODULE OFF)
    set(WITHOUT_STATISTICS_MODULE OFF)
  endif()

  if(NELSON_PORTABLE_WITH_IMAGE_PROCESSING)
    set(WITHOUT_IMAGE_PROCESSING_MODULE OFF)
  endif()

  if(NELSON_PORTABLE_WITH_OPTIMIZATION)
    set(WITHOUT_OPTIMIZATION_MODULE OFF)
  endif()

  if(NELSON_PORTABLE_WITH_FFTW)
    if(LGPL_ONLY)
      message(FATAL_ERROR
        "NELSON_PORTABLE_WITH_FFTW cannot be used with LGPL_ONLY because "
        "distributing FFTW-linked builds is governed by FFTW licensing terms."
      )
    endif()
    set(WITHOUT_FFTW_MODULE OFF)
    set(
      NELSON_FFTW_STATIC_BACKEND
      ON
      CACHE
        BOOL
        "Link FFTW symbols directly instead of loading runtime libraries"
      FORCE
    )
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_FFTW_LIBRARY OR NOT NELSON_WASM_FFTWF_LIBRARY)
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_FFTW requires Emscripten FFTW archives. "
          "Run tools/wasm/build-fftw.sh or set NELSON_WASM_FFTW_LIBRARY and "
          "NELSON_WASM_FFTWF_LIBRARY."
        )
      endif()
      set(
        NELSON_FFTW_INCLUDE_DIR
        "${NELSON_WASM_FFTW_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing fftw3.h"
        FORCE
      )
      set(
        NELSON_FFTW_LIBRARY
        "${NELSON_WASM_FFTW_LIBRARY}"
        CACHE
          FILEPATH
          "Static double-precision FFTW library"
        FORCE
      )
      set(
        NELSON_FFTWF_LIBRARY
        "${NELSON_WASM_FFTWF_LIBRARY}"
        CACHE
          FILEPATH
          "Static single-precision FFTW library"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_SIGNAL_PROCESSING)
    set(WITHOUT_SIGNAL_PROCESSING_MODULE OFF)
    set(WITHOUT_POLYNOMIAL_FUNCTIONS_MODULE OFF)
    set(WITHOUT_SPECIAL_FUNCTIONS_MODULE OFF)
    set(WITHOUT_TRIGONOMETRIC_FUNCTIONS_MODULE OFF)
    if(NOT NELSON_PORTABLE_WITH_FFTW)
      message(FATAL_ERROR
        "NELSON_PORTABLE_WITH_SIGNAL_PROCESSING requires "
        "NELSON_PORTABLE_WITH_FFTW because spectral signal functions depend "
        "on fft/ifft."
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_GEOMETRY)
    set(WITHOUT_GEOMETRY_MODULE OFF)
    set(WITHOUT_POLYNOMIAL_FUNCTIONS_MODULE OFF)
    set(WITHOUT_SPECIAL_FUNCTIONS_MODULE OFF)
    set(WITHOUT_TRIGONOMETRIC_FUNCTIONS_MODULE OFF)
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_QHULL_LIBRARY)
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_GEOMETRY requires Emscripten Qhull archives. "
          "Run tools/wasm/build-qhull.sh or set NELSON_WASM_QHULL_LIBRARY."
        )
      endif()
      set(
        Qhull_INCLUDE_DIR
        "${NELSON_WASM_QHULL_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing Qhull headers"
        FORCE
      )
      set(
        Qhull_R_LIBRARY
        "${NELSON_WASM_QHULL_LIBRARY}"
        CACHE
          FILEPATH
          "Static Qhull reentrant library"
        FORCE
      )
      set(
        Qhull_CPP_LIBRARY
        ""
        CACHE
          FILEPATH
          "Static Qhull C++ library"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_HDF5)
    set(WITHOUT_HDF5_MODULE OFF)
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_HDF5_LIBRARY OR NOT NELSON_WASM_ZLIB_LIBRARY)
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_HDF5 requires Emscripten HDF5 and zlib archives. "
          "Run tools/wasm/build-zlib.sh and tools/wasm/build-hdf5.sh, or set "
          "NELSON_WASM_HDF5_LIBRARY and NELSON_WASM_ZLIB_LIBRARY."
        )
      endif()
      set(
        HDF5_INCLUDE_DIRS
        "${NELSON_WASM_HDF5_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing hdf5.h"
        FORCE
      )
      set(
        HDF5_LIBRARIES
        "${NELSON_WASM_HDF5_LIBRARY};${NELSON_WASM_ZLIB_LIBRARY}"
        CACHE
          STRING
          "Static HDF5 libraries"
        FORCE
      )
      if(NELSON_WASM_HDF5_HL_LIBRARY)
        set(
          HDF5_LIBRARIES
          "${NELSON_WASM_HDF5_HL_LIBRARY};${NELSON_WASM_HDF5_LIBRARY};${NELSON_WASM_ZLIB_LIBRARY}"
          CACHE
            STRING
            "Static HDF5 libraries"
          FORCE
        )
        set(
          HDF5_HL_LIBRARIES
          "${NELSON_WASM_HDF5_HL_LIBRARY}"
          CACHE
            STRING
            "Static HDF5 high-level libraries"
          FORCE
        )
      endif()
      set(
        HDF5_FOUND
        TRUE
        CACHE
          BOOL
          "HDF5 found for the portable Wasm build"
        FORCE
      )
      set(
        ZLIB_INCLUDE_DIR
        "${NELSON_WASM_ZLIB_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing zlib.h"
        FORCE
      )
      set(
        ZLIB_LIBRARY
        "${NELSON_WASM_ZLIB_LIBRARY}"
        CACHE
          FILEPATH
          "Static zlib library"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_WEBTOOLS AND NELSON_WASM_BUILD)
    # webread/websave/webwrite through the browser (synchronous XMLHttpRequest
    # in the engine Worker); libcurl and libgit2 are not part of the build.
    set(WITHOUT_LIBCURL ON)
    set(WITHOUT_LIBGIT2 ON)
    set(WITHOUT_WEBTOOLS_MODULE OFF)
    set(WITHOUT_JSON_MODULE OFF)
  endif()
  if(NELSON_PORTABLE_WITH_PREFERENCES)
    set(WITHOUT_PREFERENCES_MODULE OFF)
    set(WITHOUT_HDF5_MODULE OFF)
    # Preferences persist to a .nh5 (HDF5) store; the GUI settings dialog is
    # compiled out automatically when the Qt gui module is absent (as in wasm),
    # leaving getpref/setpref/addpref/ispref/rmpref fully available.
    if(NOT NELSON_PORTABLE_WITH_HDF5)
      message(FATAL_ERROR
        "NELSON_PORTABLE_WITH_PREFERENCES requires NELSON_PORTABLE_WITH_HDF5 "
        "because preferences are stored in a .nh5 (HDF5) file."
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_NETCDF)
    set(WITHOUT_HDF5_MODULE OFF)
    set(WITHOUT_NETCDF_MODULE OFF)
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_NETCDF_LIBRARY
        OR NOT NELSON_WASM_HDF5_LIBRARY
        OR NOT NELSON_WASM_HDF5_HL_LIBRARY
        OR NOT NELSON_WASM_ZLIB_LIBRARY
      )
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_NETCDF requires Emscripten netCDF, HDF5, "
          "HDF5 HL, and zlib archives. Run tools/wasm/build-hdf5.sh and "
          "tools/wasm/build-netcdf.sh, or set the NELSON_WASM_* variables."
        )
      endif()
      set(
        NETCDF_FOUND
        TRUE
        CACHE
          BOOL
          "netCDF found for the portable Wasm build"
        FORCE
      )
      set(
        NETCDF_INCLUDE_DIRS
        "${NELSON_WASM_NETCDF_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing netcdf.h"
        FORCE
      )
      set(
        NETCDF_LIBRARIES
        "${NELSON_WASM_NETCDF_LIBRARY};${NELSON_WASM_HDF5_HL_LIBRARY};${NELSON_WASM_HDF5_LIBRARY};${NELSON_WASM_ZLIB_LIBRARY}"
        CACHE
          STRING
          "Static netCDF libraries"
        FORCE
      )
      set(
        NETCDF_LIBRARY_DIRS
        ""
        CACHE
          STRING
          "Static netCDF library directories"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_MATIO)
    set(WITHOUT_MATIO_MODULE OFF)
    set(WITHOUT_HDF5_MODULE OFF)
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_MATIO_LIBRARY
        OR NOT NELSON_WASM_HDF5_LIBRARY
        OR NOT NELSON_WASM_HDF5_HL_LIBRARY
        OR NOT NELSON_WASM_ZLIB_LIBRARY
      )
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_MATIO requires Emscripten MATIO, HDF5, "
          "HDF5 HL, and zlib archives. Run tools/wasm/build-hdf5.sh and "
          "tools/wasm/build-matio.sh, or set the NELSON_WASM_* variables."
        )
      endif()
      set(
        MATIO_FOUND
        TRUE
        CACHE
          BOOL
          "MATIO found for the portable Wasm build"
        FORCE
      )
      set(
        MATIO_INCLUDE_DIRS
        "${NELSON_WASM_MATIO_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing matio.h"
        FORCE
      )
      set(
        MATIO_LIBRARIES
        "${NELSON_WASM_MATIO_LIBRARY};${NELSON_WASM_HDF5_HL_LIBRARY};${NELSON_WASM_HDF5_LIBRARY};${NELSON_WASM_ZLIB_LIBRARY}"
        CACHE
          STRING
          "Static MATIO libraries"
        FORCE
      )
      set(
        MATIO_INCLUDE_DIR
        "${NELSON_WASM_MATIO_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing matio.h"
        FORCE
      )
      set(
        MATIO_LIBRARY
        "${NELSON_WASM_MATIO_LIBRARY}"
        CACHE
          FILEPATH
          "Static MATIO library"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_PARQUET)
    set(WITHOUT_PARQUET_MODULE OFF)
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_ARROW_LIBRARY
        OR NOT NELSON_WASM_PARQUET_LIBRARY
        OR NOT NELSON_WASM_ARROW_BUNDLED_LIBRARY
        OR NOT NELSON_WASM_THRIFT_LIBRARY
        OR NOT NELSON_WASM_ARROW_INCLUDE_DIR
        OR NOT NELSON_WASM_PARQUET_INCLUDE_DIR
      )
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_PARQUET requires the Emscripten Arrow, "
          "Parquet, and Thrift archives. Run tools/wasm/build-arrow-parquet.sh "
          "or set the NELSON_WASM_ARROW_*, NELSON_WASM_PARQUET_*, and "
          "NELSON_WASM_THRIFT_LIBRARY variables."
        )
      endif()
      set(
        ARROW_INCLUDE_DIRS
        "${NELSON_WASM_ARROW_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing Arrow headers"
        FORCE
      )
      set(
        PARQUET_INCLUDE_DIRS
        "${NELSON_WASM_PARQUET_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing Parquet headers"
        FORCE
      )
      set(
        ARROW_LIBRARIES
        "${NELSON_WASM_ARROW_BUNDLED_LIBRARY};${NELSON_WASM_ARROW_LIBRARY};${NELSON_WASM_THRIFT_LIBRARY};${NELSON_WASM_ZLIB_LIBRARY}"
        CACHE
          STRING
          "Static Arrow and its Emscripten dependencies"
        FORCE
      )
      set(
        PARQUET_LIBRARIES
        "${NELSON_WASM_PARQUET_LIBRARY}"
        CACHE
          STRING
          "Static Parquet library"
        FORCE
      )
      set(
        ARROW_FOUND
        TRUE
        CACHE
          BOOL
          "Arrow found for the portable Wasm build"
        FORCE
      )
      set(
        PARQUET_FOUND
        TRUE
        CACHE
          BOOL
          "Parquet found for the portable Wasm build"
        FORCE
      )
      set(
        ARROW_LIBRARY_DIRS
        ""
        CACHE
          STRING
          "Static Arrow library directories"
        FORCE
      )
      set(
        PARQUET_LIBRARY_DIRS
        ""
        CACHE
          STRING
          "Static Parquet library directories"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_XML)
    set(WITHOUT_JSON_MODULE OFF)
    set(WITHOUT_XML_MODULE OFF)
    set(WITHOUT_HELP_TOOLS_MODULE OFF)
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_LIBXML2_LIBRARY
        OR NOT NELSON_WASM_LIBXSLT_LIBRARY
        OR NOT NELSON_WASM_LIBXSLT_EXSLT_LIBRARY
      )
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_XML requires Emscripten libxml2 and libxslt "
          "archives. Run tools/wasm/build-libxslt.sh or set the "
          "NELSON_WASM_LIBXML2_* and NELSON_WASM_LIBXSLT_* variables."
        )
      endif()
      set(
        LIBXML2_INCLUDE_DIR
        "${NELSON_WASM_LIBXML2_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing libxml2 headers"
        FORCE
      )
      set(
        LIBXML2_LIBRARY
        "${NELSON_WASM_LIBXML2_LIBRARY}"
        CACHE
          FILEPATH
          "Static libxml2 library"
        FORCE
      )
      set(
        LIBXSLT_INCLUDE_DIR
        "${NELSON_WASM_LIBXSLT_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing libxslt headers"
        FORCE
      )
      set(
        LIBXSLT_EXSLT_INCLUDE_DIR
        "${NELSON_WASM_LIBXSLT_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing libexslt headers"
        FORCE
      )
      set(
        LIBXSLT_LIBRARY
        "${NELSON_WASM_LIBXSLT_LIBRARY}"
        CACHE
          FILEPATH
          "Static libxslt library"
        FORCE
      )
      set(
        LIBXSLT_EXSLT_LIBRARY
        "${NELSON_WASM_LIBXSLT_EXSLT_LIBRARY}"
        CACHE
          FILEPATH
          "Static libexslt library"
        FORCE
      )
      set(
        LibXml2_USE_STATIC_LIBS
        TRUE
        CACHE
          BOOL
          "Use static libxml2 for the portable Wasm build"
        FORCE
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_SPREADSHEET)
    set(WITHOUT_SPREADSHEET_MODULE OFF)
    set(WITHOUT_JSON_MODULE OFF)
    set(WITHOUT_XML_MODULE OFF)
    if(NELSON_WASM_BUILD AND NOT NELSON_PORTABLE_WITH_XML)
      message(FATAL_ERROR
        "NELSON_PORTABLE_WITH_SPREADSHEET requires NELSON_PORTABLE_WITH_XML "
        "for XML table functions and workbook support."
      )
    endif()
  endif()

  if(NELSON_PORTABLE_WITH_AUDIO)
    set(WITHOUT_AUDIO_MODULE OFF)
    set(
      NELSON_AUDIO_FILE_ONLY_BACKEND
      ON
      CACHE
        BOOL
        "Build the audio module without native audio-device streams"
      FORCE
    )
    if(NELSON_WASM_BUILD)
      if(NOT NELSON_WASM_SNDFILE_LIBRARY)
        message(FATAL_ERROR
          "NELSON_PORTABLE_WITH_AUDIO requires Emscripten libsndfile archives. "
          "Run tools/wasm/build-libsndfile.sh or set NELSON_WASM_SNDFILE_LIBRARY."
        )
      endif()
      set(
        SNDFILE_FOUND
        TRUE
        CACHE
          BOOL
          "libsndfile found for the portable Wasm build"
        FORCE
      )
      set(
        SNDFILE_INCLUDE_DIRS
        "${NELSON_WASM_SNDFILE_INCLUDE_DIR}"
        CACHE
          PATH
          "Directory containing sndfile.h"
        FORCE
      )
      set(
        SNDFILE_LIBRARIES
        "${NELSON_WASM_SNDFILE_LIBRARY}"
        CACHE
          FILEPATH
          "Static libsndfile library"
        FORCE
      )
      set(
        PORTAUDIO_INCLUDE_DIRS
        ""
        CACHE
          STRING
          "PortAudio disabled for portable file-only audio"
        FORCE
      )
      set(
        PORTAUDIO_LIBRARIES
        ""
        CACHE
          STRING
          "PortAudio disabled for portable file-only audio"
        FORCE
      )
      set(
        PORTAUDIO_FOUND
        FALSE
        CACHE
          BOOL
          "PortAudio disabled for portable file-only audio"
        FORCE
      )
      set(
        TAGLIB_FOUND
        FALSE
        CACHE
          BOOL
          "TagLib disabled for portable file-only audio"
        FORCE
      )
    endif()
  elseif(NELSON_PORTABLE_AUDIO_WITH_FLAC
    OR NELSON_PORTABLE_AUDIO_WITH_OGG
    OR NELSON_PORTABLE_AUDIO_WITH_MPEG
  )
    message(FATAL_ERROR
      "Portable audio codec capabilities require NELSON_PORTABLE_WITH_AUDIO=ON. "
      "Only enable NELSON_PORTABLE_AUDIO_WITH_FLAC, "
      "NELSON_PORTABLE_AUDIO_WITH_OGG, or NELSON_PORTABLE_AUDIO_WITH_MPEG "
      "when the portable audio module is included and libsndfile is linked "
      "with the matching codec dependencies."
    )
  endif()

  if(NELSON_PORTABLE_ENABLE_TESTS)
    set(WITHOUT_ASSERT_FUNCTIONS_MODULE OFF)
    set(WITHOUT_DATA_ANALYSIS_MODULE OFF)
    set(WITHOUT_VALIDATORS_MODULE OFF)
    set(WITHOUT_TESTS_MANAGER_MODULE OFF)
  endif()

  if(NELSON_WASM_BUILD AND NOT CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
    message(FATAL_ERROR
      "NELSON_WASM_BUILD requires the Emscripten CMake toolchain. Configure with emcmake."
    )
  endif()

  add_compile_definitions(NELSON_STATIC_GATEWAYS)
endif()

if(NELSON_WASM_BUILD)
  if(NELSON_WASM_JSPI AND NOT NELSON_WASM_NATIVE_EXCEPTIONS)
    message(FATAL_ERROR
      "NELSON_WASM_JSPI requires NELSON_WASM_NATIVE_EXCEPTIONS to avoid JavaScript exception trampolines."
    )
  endif()
  if(NELSON_WASM_JSPI)
    add_compile_definitions(NELSON_WASM_JSPI)
  endif()
  if(NELSON_WASM_NATIVE_EXCEPTIONS)
    set(_nelson_wasm_exception_flag -fwasm-exceptions)
  else()
    set(_nelson_wasm_exception_flag -fexceptions)
  endif()
  add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:${_nelson_wasm_exception_flag}>"
    -msimd128
  )
  add_link_options(${_nelson_wasm_exception_flag}
    -msimd128
  )
endif()

if(NELSON_PORTABLE_BUILD AND NELSON_PORTABLE_ENABLE_TESTS)
  enable_testing()
endif()
