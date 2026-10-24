# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name optimization)
set(module_library_name nlsOptimization)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
set(NELSON_OPTIMIZATION_WITH_HIGHS FALSE)
if(WIN32)
  set(_NELSON_HIGHS_ROOT "${NELSON_WINDOWS_THIRDPARTY_ROOT}/highs")

  set(HIGHS_INCLUDE_DIR "${_NELSON_HIGHS_ROOT}/include")
  set(HIGHS_LIBRARY "${_NELSON_HIGHS_ROOT}/lib/highs.lib")
  set(HIGHS_Z_LIBRARY "${_NELSON_HIGHS_ROOT}/lib/z.lib")
  set(HIGHS_RUNTIME_DLL "${_NELSON_HIGHS_ROOT}/bin/highs.dll")
  set(HIGHS_Z_RUNTIME_DLL "${_NELSON_HIGHS_ROOT}/bin/z.dll")

  if(NOT EXISTS "${HIGHS_INCLUDE_DIR}/highs/interfaces/highs_c_api.h"
    OR NOT EXISTS "${HIGHS_LIBRARY}"
  )
    message(FATAL_ERROR "HiGHS third-party package is required on Windows.")
  endif()
  set(NELSON_OPTIMIZATION_WITH_HIGHS TRUE)
else()
  # Some distributions ship a HiGHS CMake package whose exported targets
  # reference a CLI binary (highs::highs-bin) that the package does not
  # install, which raises a FATAL_ERROR from the targets file as soon as
  # find_package() includes it (QUIET does not suppress it). WITHOUT_HIGHS
  # lets such a broken system package be skipped without uninstalling it.
  option(WITHOUT_HIGHS
    "Disable the HiGHS backend of the optimization module (linprog/intlinprog)"
    OFF
  )
  if(NOT WITHOUT_HIGHS)
    find_package(HiGHS CONFIG QUIET)
    if(TARGET HiGHS::highs
      OR TARGET highs::highs
      OR TARGET HiGHS::HiGHS
      OR HiGHS_LIBRARIES
    )
      set(NELSON_OPTIMIZATION_WITH_HIGHS TRUE)
    endif()
    # Some distributions ship a HiGHS package whose imported target carries a
    # bogus include path (e.g. "/include") that does not exist, which makes
    # CMake fail at generate time. Drop non-existent include directories from
    # the imported targets; the HiGHS headers are then found through the system
    # include path (highs/interfaces/highs_c_api.h under /usr/include).
    foreach(_highs_target HiGHS::highs highs::highs HiGHS::HiGHS)
      if(TARGET ${_highs_target})
        get_target_property(_highs_includes
          ${_highs_target}
          INTERFACE_INCLUDE_DIRECTORIES
        )
        if(_highs_includes)
          set(_highs_valid_includes "")
          foreach(_highs_dir ${_highs_includes})
            if(EXISTS "${_highs_dir}")
              list(APPEND _highs_valid_includes "${_highs_dir}")
              # Some packages (e.g. Debian/Ubuntu) collapse the import prefix
              # to empty, leaving only "/usr/include" as a valid include path.
              # The public header highs/interfaces/highs_c_api.h then includes
              # "lp_data/HighsCallbackStruct.h" relative to the highs/ root, so
              # that directory must be on the include path too.
              if(EXISTS "${_highs_dir}/highs")
                list(APPEND _highs_valid_includes "${_highs_dir}/highs")
              endif()
            endif()
          endforeach()
          list(REMOVE_DUPLICATES _highs_valid_includes)
          set_target_properties(${_highs_target}
            PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${_highs_valid_includes}"
          )
        endif()
      endif()
    endforeach()
  endif()
endif()

nelson_module_sources_from_glob(
  OPTIMIZATION_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)

add_library(${module_library_name} ${OPTIMIZATION_SRC})

target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${EIGEN3_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../sparse/src/include
)

target_link_libraries(${module_library_name}
  PRIVATE
    nlsCharacters_encoding
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsSparse
    nlsStream_manager
    nlsTypes
)

if(NELSON_OPTIMIZATION_WITH_HIGHS)
  target_compile_definitions(${module_library_name} PRIVATE WITH_HIGHS)
  if(WIN32)
    target_include_directories(${module_library_name}
      PRIVATE
        "${HIGHS_INCLUDE_DIR}"
        "${HIGHS_INCLUDE_DIR}/highs"
    )
    target_link_libraries(${module_library_name}
      PRIVATE
        "${HIGHS_LIBRARY}"
        "${HIGHS_Z_LIBRARY}"
    )
  elseif(TARGET HiGHS::highs)
    target_link_libraries(${module_library_name} PRIVATE HiGHS::highs)
  elseif(TARGET highs::highs)
    target_link_libraries(${module_library_name} PRIVATE highs::highs)
  elseif(TARGET HiGHS::HiGHS)
    target_link_libraries(${module_library_name} PRIVATE HiGHS::HiGHS)
  elseif(HiGHS_LIBRARIES)
    target_include_directories(${module_library_name}
      PRIVATE ${HiGHS_INCLUDE_DIRS}
    )
    target_link_libraries(${module_library_name} PRIVATE ${HiGHS_LIBRARIES})
  endif()
endif()

nelson_module_sources_from_glob(
  OPTIMIZATION_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)

add_library(${module_library_builtin_name} ${OPTIMIZATION_BUILTIN})

target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)

target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes
)

nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
