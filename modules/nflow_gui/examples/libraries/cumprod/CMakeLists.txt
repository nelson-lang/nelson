cmake_minimum_required(VERSION 3.16)
project(nflow_cumprod VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ---------------------------------------------------------------------------
# Locate the nflow source tree.
# Set NFLOW_ROOT on the command line if building out-of-tree:
#   cmake -DNFLOW_ROOT=/path/to/nflow ..
# ---------------------------------------------------------------------------
if(NOT DEFINED NFLOW_ROOT)
  # Assume this CMakeLists.txt lives at
  #   <nflow>/modules/nflow_gui/examples/libraries/cumprod/
  get_filename_component(NFLOW_ROOT
    "${CMAKE_CURRENT_SOURCE_DIR}/../../../../.."
    ABSOLUTE
  )
  message(STATUS "NFLOW_ROOT auto-detected: ${NFLOW_ROOT}")
endif()

# BlockPlugin.h / SimEngineTypes.hpp and the runtime helpers now live in nflow_blocks.
set(NFLOW_BLOCKS_INCLUDE "${NFLOW_ROOT}/modules/nflow_blocks/src/include")
set(NFLOW_LIB_DIR "${NFLOW_ROOT}/bin/x64") # adjust for your platform/build

# ---------------------------------------------------------------------------
# nlohmann/json – required by SimEngineTypes.hpp
# Find it as a package or fall back to a local copy via FetchContent.
# ---------------------------------------------------------------------------
find_package(nlohmann_json QUIET)
if(NOT nlohmann_json_FOUND)
  include(FetchContent)
  FetchContent_Declare(
    nlohmann_json
    URL
    https://github.com/nlohmann/json/releases/download/v3.11.3/json.tar.xz
  )
  FetchContent_MakeAvailable(nlohmann_json)
endif()

# ---------------------------------------------------------------------------
# Plugin target
# ---------------------------------------------------------------------------
add_library(nflow_cumprod SHARED
  cumprod.cpp
)

# Use the same include path as the main nflow build
target_include_directories(nflow_cumprod
  PRIVATE "${NFLOW_BLOCKS_INCLUDE}"
)

target_link_libraries(nflow_cumprod
  PRIVATE nlohmann_json::nlohmann_json
)

# Helpers (getInput, getState, updated, …) are defined in SimEngineHelpers.cpp
# which is compiled into the nlsNflow_blocks shared library.  Link against it so
# the linker resolves those symbols.
if(WIN32)
  set(_nls_lib "${NFLOW_LIB_DIR}/libnlsNflow_blocks.lib")
elseif(APPLE)
  set(_nls_lib "${NFLOW_LIB_DIR}/libnlsNflow_blocks.dylib")
else()
  set(_nls_lib "${NFLOW_LIB_DIR}/libnlsNflow_blocks.so")
endif()

if(EXISTS "${_nls_lib}")
  target_link_libraries(nflow_cumprod PRIVATE "${_nls_lib}")
else()
  message(WARNING
    "nlsNflow_blocks not found at '${_nls_lib}'.\n"
    "Set NFLOW_LIB_DIR or build nflow first, then re-run cmake."
  )
endif()

# Strip the default 'lib' prefix on Linux/macOS so the filename matches the
# "native" field in library.json (nflow_cumprod.dll / .so / .dylib).
set_target_properties(nflow_cumprod PROPERTIES PREFIX "")

# ---------------------------------------------------------------------------
# Install – copy the library next to library.json for easy testing
# ---------------------------------------------------------------------------
install(
  TARGETS nflow_cumprod
  LIBRARY DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}"
  RUNTIME DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}"
)
