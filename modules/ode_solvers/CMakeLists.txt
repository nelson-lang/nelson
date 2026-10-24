# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name ode_solvers)
set(module_library_builtin_name nlsOde_solvers_builtin)
# ==============================================================================
set(
  NELSON_SUNDIALS_MODE
  "AUTO"
  CACHE
    STRING
    "SUNDIALS backend mode for ode_solvers: AUTO, ON, or OFF"
)
set_property(CACHE NELSON_SUNDIALS_MODE PROPERTY STRINGS AUTO ON OFF)
string(TOUPPER "${NELSON_SUNDIALS_MODE}" NELSON_SUNDIALS_MODE)
if(NOT NELSON_SUNDIALS_MODE STREQUAL "AUTO"
  AND NOT NELSON_SUNDIALS_MODE STREQUAL "ON"
  AND NOT NELSON_SUNDIALS_MODE STREQUAL "OFF"
)
  message(FATAL_ERROR "NELSON_SUNDIALS_MODE must be AUTO, ON, or OFF.")
endif()

set(NELSON_WITH_SUNDIALS OFF)
set(NELSON_SUNDIALS_LIBRARIES)

if(NOT NELSON_SUNDIALS_MODE STREQUAL "OFF")
  set(_nelson_sundials_roots)
  if(DEFINED NELSON_WINDOWS_THIRDPARTY_ROOT)
    list(APPEND _nelson_sundials_roots
      "${NELSON_WINDOWS_THIRDPARTY_ROOT}/sundials"
    )
  endif()
  list(APPEND _nelson_sundials_roots "${CMAKE_PREFIX_PATH}")

  find_path(NELSON_SUNDIALS_INCLUDE_DIR
    NAMES
    cvodes/cvodes.h
    idas/idas.h
    nvector/nvector_serial.h
    PATHS
    ${_nelson_sundials_roots}
    PATH_SUFFIXES
    include
    NO_DEFAULT_PATH
  )
  if(NOT NELSON_SUNDIALS_INCLUDE_DIR)
    find_path(NELSON_SUNDIALS_INCLUDE_DIR
      NAMES
      cvodes/cvodes.h
      idas/idas.h
      nvector/nvector_serial.h
      PATH_SUFFIXES
      include
    )
  endif()

  set(_nelson_sundials_required_libs
    sundials_core
    sundials_cvodes
    sundials_idas
    sundials_nvecserial
    sundials_sunmatrixdense
    sundials_sunlinsoldense
    sundials_sunnonlinsolnewton
  )
  set(_nelson_sundials_missing)
  foreach(_lib_name IN LISTS _nelson_sundials_required_libs)
    string(TOUPPER "${_lib_name}" _lib_var_name)
    find_library(NELSON_${_lib_var_name}_LIBRARY
      NAMES
      ${_lib_name}
      PATHS
      ${_nelson_sundials_roots}
      PATH_SUFFIXES
      lib
      NO_DEFAULT_PATH
    )
    if(NOT NELSON_${_lib_var_name}_LIBRARY)
      find_library(NELSON_${_lib_var_name}_LIBRARY
        NAMES
        ${_lib_name}
        PATH_SUFFIXES
        lib
      )
    endif()
    if(NELSON_${_lib_var_name}_LIBRARY)
      list(APPEND NELSON_SUNDIALS_LIBRARIES ${NELSON_${_lib_var_name}_LIBRARY})
    else()
      list(APPEND _nelson_sundials_missing ${_lib_name})
    endif()
  endforeach()

  if(EMSCRIPTEN AND NELSON_SUNDIALS_INCLUDE_DIR)
    get_filename_component(_nelson_sundials_target_prefix
      "${NELSON_SUNDIALS_INCLUDE_DIR}"
      DIRECTORY
    )
  endif()

  foreach(_lib_name
    sundials_sunmatrixsparse
    sundials_sunlinsolspgmr
    sundials_sunlinsolspfgmr
    sundials_sunlinsolspbcgs
    sundials_sunlinsolsptfqmr
    sundials_sunlinsolpcg
    sundials_sunlinsolklu
    sundials_kinsol
  )
    string(TOUPPER "${_lib_name}" _lib_var_name)
    if(EMSCRIPTEN AND _nelson_sundials_target_prefix)
      # The configured target prefix is outside Emscripten's sysroot. Search
      # only that prefix directly, without admitting host library paths.
      find_library(NELSON_${_lib_var_name}_LIBRARY
        NAMES
        ${_lib_name}
        PATHS
        "${_nelson_sundials_target_prefix}/lib"
        NO_DEFAULT_PATH
        NO_CMAKE_FIND_ROOT_PATH
      )
    endif()
    find_library(NELSON_${_lib_var_name}_LIBRARY
      NAMES
      ${_lib_name}
      PATHS
      ${_nelson_sundials_roots}
      PATH_SUFFIXES
      lib
      NO_DEFAULT_PATH
    )
    if(NOT NELSON_${_lib_var_name}_LIBRARY)
      find_library(NELSON_${_lib_var_name}_LIBRARY
        NAMES
        ${_lib_name}
        PATH_SUFFIXES
        lib
      )
    endif()
    if(_lib_name STREQUAL "sundials_sunlinsolklu"
      AND NELSON_${_lib_var_name}_LIBRARY
    )
      # sunlinsol_klu.h includes <klu.h> provided by SuiteSparse: both are required.
      find_path(NELSON_KLU_INCLUDE_DIR
        NAMES
        klu.h
        PATH_SUFFIXES
        suitesparse
      )
      if(NOT EXISTS "${NELSON_SUNDIALS_INCLUDE_DIR}/sunlinsol/sunlinsol_klu.h"
        OR NOT NELSON_KLU_INCLUDE_DIR
      )
        set(NELSON_${_lib_var_name}_LIBRARY "")
      endif()
    endif()
    if(NELSON_${_lib_var_name}_LIBRARY)
      list(APPEND NELSON_SUNDIALS_LIBRARIES ${NELSON_${_lib_var_name}_LIBRARY})
      if(_lib_name STREQUAL "sundials_sunmatrixsparse")
        set(NELSON_WITH_SUNDIALS_SPARSE_MATRIX ON)
      elseif(_lib_name STREQUAL "sundials_sunlinsolspgmr")
        set(NELSON_WITH_SUNDIALS_SPGMR ON)
      elseif(_lib_name STREQUAL "sundials_sunlinsolspfgmr")
        set(NELSON_WITH_SUNDIALS_SPFGMR ON)
      elseif(_lib_name STREQUAL "sundials_sunlinsolspbcgs")
        set(NELSON_WITH_SUNDIALS_SPBCGS ON)
      elseif(_lib_name STREQUAL "sundials_sunlinsolsptfqmr")
        set(NELSON_WITH_SUNDIALS_SPTFQMR ON)
      elseif(_lib_name STREQUAL "sundials_sunlinsolpcg")
        set(NELSON_WITH_SUNDIALS_PCG ON)
      elseif(_lib_name STREQUAL "sundials_sunlinsolklu")
        set(NELSON_WITH_SUNDIALS_KLU ON)
      elseif(_lib_name STREQUAL "sundials_kinsol")
        set(NELSON_WITH_SUNDIALS_KINSOL ON)
      endif()
    endif()
  endforeach()

  if(NELSON_SUNDIALS_INCLUDE_DIR AND NOT _nelson_sundials_missing)
    set(NELSON_WITH_SUNDIALS ON)
    message(STATUS "SUNDIALS backend enabled for ode_solvers.")
  elseif(NELSON_SUNDIALS_MODE STREQUAL "ON")
    message(FATAL_ERROR
      "SUNDIALS backend requested but dependencies are missing: ${_nelson_sundials_missing}"
    )
  else()
    message(STATUS "SUNDIALS backend disabled for ode_solvers.")
  endif()
endif()
# ==============================================================================
# Core library — native-callback CVODES/IDAS (OdeCore) plus the CVODES / IDAS
# implementations of NFlow's ISolverBackend. No interpreter dependency and no
# LINK to NFlow (only its header-only ISolverBackend contract): NFlow pulls the
# backends in via registerSundialsSolverBackends(), so the direction stays
# NFlow -> ode_solvers and this core is reusable by any C++ caller.
# ==============================================================================
set(module_library_name nlsOde_solvers)
nelson_module_sources_from_glob(
  ODE_SOLVERS_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

if(ODE_SOLVERS_SRC)
  add_library(${module_library_name} ${ODE_SOLVERS_SRC})
  target_include_directories(${module_library_name}
    PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
      $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE
      # Header-only: the CVODES / IDAS backends implement NFlow's ISolverBackend
      # (SolverBackend.hpp / SimDiagnostic.hpp). No link to nflow_blocks — NFlow
      # pulls these backends in via registerSundialsSolverBackends().
      ${CMAKE_CURRENT_SOURCE_DIR}/../nflow_blocks/src/include
  )
  if(NELSON_WITH_SUNDIALS)
    target_compile_definitions(${module_library_name}
      PRIVATE NELSON_WITH_SUNDIALS
    )
    target_include_directories(${module_library_name}
      PRIVATE ${NELSON_SUNDIALS_INCLUDE_DIR}
    )
    target_link_libraries(${module_library_name}
      PRIVATE ${NELSON_SUNDIALS_LIBRARIES}
    )
    if(NOT WIN32)
      # SUNDIALS headers from an MPI-enabled build pull in <mpi.h>, which lives
      # outside the default include path (e.g. the OpenMPI include directory).
      if(NOT MPI_FOUND)
        # without the mpi module nothing searched MPI yet: the distribution SUNDIALS may need it
        find_package(MPI QUIET COMPONENTS C CXX)
      endif()
      target_include_directories(${module_library_name}
        PRIVATE
          ${MPI_CXX_INCLUDE_DIRS}
          ${MPI_INCLUDE_PATH}
      )
      target_link_libraries(${module_library_name} PRIVATE ${MPI_CXX_LIBRARIES})
    endif()
  endif()
  nelson_install_library(${module_library_name})
endif()

# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  ODE_SOLVERS_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

if(ODE_SOLVERS_BUILTIN)
  add_library(${module_library_builtin_name} ${ODE_SOLVERS_BUILTIN})

  target_include_directories(${module_library_builtin_name}
    PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
      $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
    PRIVATE
      ${EIGEN3_INCLUDE_DIR}
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/src/include
  )

  if(NELSON_WITH_SUNDIALS)
    target_compile_definitions(${module_library_builtin_name}
      PRIVATE NELSON_WITH_SUNDIALS
    )
    if(NELSON_WITH_SUNDIALS_SPGMR)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_SPGMR
      )
    endif()
    if(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_SPARSE_MATRIX
      )
    endif()
    if(NELSON_WITH_SUNDIALS_SPFGMR)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_SPFGMR
      )
    endif()
    if(NELSON_WITH_SUNDIALS_SPBCGS)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_SPBCGS
      )
    endif()
    if(NELSON_WITH_SUNDIALS_SPTFQMR)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_SPTFQMR
      )
    endif()
    if(NELSON_WITH_SUNDIALS_PCG)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_PCG
      )
    endif()
    if(NELSON_WITH_SUNDIALS_KLU)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_KLU
      )
      target_include_directories(${module_library_builtin_name}
        PRIVATE ${NELSON_KLU_INCLUDE_DIR}
      )
    endif()
    if(NELSON_WITH_SUNDIALS_KINSOL)
      target_compile_definitions(${module_library_builtin_name}
        PRIVATE NELSON_WITH_SUNDIALS_KINSOL
      )
    endif()
    target_include_directories(${module_library_builtin_name}
      PRIVATE ${NELSON_SUNDIALS_INCLUDE_DIR}
    )
  endif()

  target_link_libraries(${module_library_builtin_name}
    PRIVATE
      nlsError_manager
      nlsNelson_manager
      nlsI18n
      nlsCommons
      nlsTypes
      nlsInterpreter
      nlsOde_solvers
  )

  if(NELSON_WITH_SUNDIALS)
    target_link_libraries(${module_library_builtin_name}
      PRIVATE ${NELSON_SUNDIALS_LIBRARIES}
    )
    if(NOT WIN32)
      # SUNDIALS headers from an MPI-enabled build pull in <mpi.h>, which lives
      # outside the default include path (e.g. the OpenMPI include directory).
      target_include_directories(${module_library_builtin_name}
        PRIVATE
          ${MPI_CXX_INCLUDE_DIRS}
          ${MPI_INCLUDE_PATH}
      )
      target_link_libraries(${module_library_builtin_name}
        PRIVATE ${MPI_CXX_LIBRARIES}
      )
    endif()
  endif()

  nelson_install_library(${module_library_builtin_name})
endif()
# ==============================================================================
nelson_install_module_data(
  ${module_name}
  EXTRA_DIR_PATTERNS
  "examples:*.m,*.json"
)
