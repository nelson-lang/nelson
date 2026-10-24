# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Helper functions to reduce boilerplate in Nelson module CMakeLists.txt files.
# ==============================================================================
include_guard(GLOBAL)
include(GNUInstallDirs)
# ==============================================================================
# nelson_install_library(<target>)
#
# Standard install rule for a Nelson library target.
# ==============================================================================
function(nelson_install_library _target)
  if(WIN32)
    set(_nelson_export_macro "${_target}")
    string(REGEX
      REPLACE
      "^no-"
      ""
      _nelson_export_macro
      "${_nelson_export_macro}"
    )
    string(TOUPPER "${_nelson_export_macro}" _nelson_export_macro)
    string(REPLACE "-" "_" _nelson_export_macro "${_nelson_export_macro}")
    set(_nelson_export_macro "${_nelson_export_macro}_EXPORTS")

    target_compile_definitions(${_target} PRIVATE ${_nelson_export_macro})
    set_target_properties(${_target}
      PROPERTIES
        PREFIX "lib"
        IMPORT_PREFIX "lib"
    )

    # Add the module's existing resources.rc (version info) to the DLL.
    if(_target MATCHES "_builtin$")
      set(_nelson_rc "${CMAKE_CURRENT_SOURCE_DIR}/builtin/c/resources.rc")
    else()
      set(_nelson_rc "${CMAKE_CURRENT_SOURCE_DIR}/src/c/resources.rc")
    endif()
    if(EXISTS "${_nelson_rc}")
      target_sources(${_target} PRIVATE "${_nelson_rc}")
    endif()
  endif()

  # Track builtin targets so executable targets can depend on them
  if(_target MATCHES "_builtin$")
    set_property(GLOBAL APPEND PROPERTY NELSON_ALL_BUILTIN_TARGETS ${_target})
    if(NELSON_PORTABLE_BUILD)
      string(MAKE_C_IDENTIFIER "${_target}" _nelson_gateway_id)
      target_compile_definitions(${_target}
        PRIVATE NELSON_STATIC_GATEWAY_ID=${_nelson_gateway_id}
      )
      set_property(
        GLOBAL APPEND
        PROPERTY
          NELSON_STATIC_GATEWAY_IDS
          ${_nelson_gateway_id}
      )
    endif()
  endif()

  # Public headers are installed per module, matching the
  # $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}> declared by
  # each module CMakeLists.  module_name is set by the calling module;
  # fall back to the module directory name if it is not defined.
  set(_nelson_module_name "${module_name}")
  if(NOT _nelson_module_name)
    get_filename_component(_nelson_module_name
      "${CMAKE_CURRENT_SOURCE_DIR}"
      NAME
    )
  endif()

  install(
    TARGETS ${_target}
    EXPORT ${PROJECT_NAME}-targets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION
      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${_nelson_module_name}
  )
endfunction()
# ==============================================================================
# nelson_add_frontend_build(<target> <build_target>
#   PACKAGE_DIR <dir>
#   [BUILD_SCRIPT <script>]
#   [DISPLAY_NAME <name>]
#   [NODE_VERSION_FILE <file>]
# )
#
# Standard Node.js frontend build target for module frontends.  It prefers the
# repository .nvmrc major version, uses npm from the selected Node.js directory
# when available, installs dependencies with npm ci, then runs the build script.
# ==============================================================================
function(nelson_add_frontend_build _target _build_target)
  cmake_parse_arguments(ARG
    ""
    "PACKAGE_DIR;BUILD_SCRIPT;DISPLAY_NAME;NODE_VERSION_FILE"
    ""
    ${ARGN}
  )

  if(NOT TARGET ${_target})
    message(FATAL_ERROR
      "nelson_add_frontend_build target does not exist: ${_target}"
    )
  endif()
  if(NOT ARG_PACKAGE_DIR)
    message(FATAL_ERROR "nelson_add_frontend_build requires PACKAGE_DIR")
  endif()
  if(NOT EXISTS "${ARG_PACKAGE_DIR}/package.json")
    message(FATAL_ERROR
      "Frontend package.json not found: ${ARG_PACKAGE_DIR}/package.json"
    )
  endif()
  if(NOT EXISTS "${ARG_PACKAGE_DIR}/package-lock.json")
    message(FATAL_ERROR
      "Frontend package-lock.json not found: ${ARG_PACKAGE_DIR}/package-lock.json"
    )
  endif()
  if(NOT ARG_BUILD_SCRIPT)
    set(ARG_BUILD_SCRIPT build)
  endif()
  if(NOT ARG_DISPLAY_NAME)
    set(ARG_DISPLAY_NAME ${_build_target})
  endif()
  if(NOT ARG_NODE_VERSION_FILE)
    set(ARG_NODE_VERSION_FILE "${CMAKE_SOURCE_DIR}/.nvmrc")
  endif()

  set(_required_major "")
  if(EXISTS "${ARG_NODE_VERSION_FILE}")
    file(READ "${ARG_NODE_VERSION_FILE}" _required_version)
    string(STRIP "${_required_version}" _required_version)
    string(REGEX MATCH "^[0-9]+" _required_major "${_required_version}")
  endif()

  set(_node_bin_dir "")
  if(_required_major AND DEFINED ENV{HOME})
    file(GLOB _nvm_node_candidates
      "$ENV{HOME}/.nvm/versions/node/v${_required_major}.*/bin/node"
    )
    if(_nvm_node_candidates)
      list(SORT _nvm_node_candidates COMPARE NATURAL ORDER DESCENDING)
      list(GET _nvm_node_candidates 0 _node_executable)
      get_filename_component(_node_bin_dir "${_node_executable}" DIRECTORY)
    endif()
  endif()

  if(NOT _node_executable)
    unset(_node_executable CACHE)
    find_program(_node_executable NAMES node)
  endif()
  if(NOT _node_executable)
    message(FATAL_ERROR
      "${ARG_DISPLAY_NAME} requires Node.js. Install nvm, then run: nvm install"
    )
  endif()
  get_filename_component(_node_bin_dir "${_node_executable}" DIRECTORY)

  execute_process(
    COMMAND ${_node_executable} --version
    OUTPUT_VARIABLE _node_version
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  string(REGEX MATCH "^v?([0-9]+)" _node_version_match "${_node_version}")
  set(_node_major "${CMAKE_MATCH_1}")
  if(_required_major AND NOT _node_major STREQUAL _required_major)
    message(FATAL_ERROR
      "${ARG_DISPLAY_NAME} requires Node.js ${_required_major}.x from .nvmrc, found ${_node_version}. Run: nvm install && nvm use"
    )
  endif()

  unset(_npm_executable CACHE)
  find_program(_npm_executable
    NAMES
    npm.cmd
    npm
    HINTS
    "${_node_bin_dir}"
    NO_DEFAULT_PATH
  )
  if(NOT _npm_executable)
    unset(_npm_executable CACHE)
    find_program(_npm_executable NAMES npm.cmd npm)
  endif()
  if(_npm_executable AND NOT EXISTS "${_npm_executable}")
    unset(_npm_executable CACHE)
    find_program(_npm_executable NAMES npm.cmd npm)
  endif()
  if(NOT _npm_executable)
    message(FATAL_ERROR "${ARG_DISPLAY_NAME} requires npm.")
  endif()

  message(STATUS "${ARG_DISPLAY_NAME} Node.js: ${_node_executable}")
  message(STATUS "${ARG_DISPLAY_NAME} npm: ${_npm_executable}")

  if(WIN32)
    set(_npm_command ${_npm_executable})
    set(_npm_cli "${_node_bin_dir}/node_modules/npm/bin/npm-cli.js")
    if(EXISTS "${_npm_cli}")
      set(_npm_command ${_node_executable} "${_npm_cli}")
    endif()
    add_custom_target(${_build_target}
      COMMAND ${CMAKE_COMMAND} -E echo
        "Installing npm deps for ${ARG_DISPLAY_NAME}..."
      COMMAND ${_npm_command} --prefix ${ARG_PACKAGE_DIR} ci --no-audit
        --no-fund
      COMMAND ${CMAKE_COMMAND} -E echo
        "Building frontend for ${ARG_DISPLAY_NAME}..."
      COMMAND ${_npm_command} --prefix ${ARG_PACKAGE_DIR} run
        ${ARG_BUILD_SCRIPT}
      WORKING_DIRECTORY ${ARG_PACKAGE_DIR}
      VERBATIM
    )
  else()
    add_custom_target(${_build_target}
      COMMAND ${CMAKE_COMMAND} -E echo
        "Installing npm deps for ${ARG_DISPLAY_NAME}..."
      COMMAND ${CMAKE_COMMAND} -E env "PATH=${_node_bin_dir}:$ENV{PATH}"
        ${_npm_executable} --prefix ${ARG_PACKAGE_DIR} ci --no-audit --no-fund
      COMMAND ${CMAKE_COMMAND} -E echo
        "Building frontend for ${ARG_DISPLAY_NAME}..."
      COMMAND ${CMAKE_COMMAND} -E env "PATH=${_node_bin_dir}:$ENV{PATH}"
        ${_npm_executable} --prefix ${ARG_PACKAGE_DIR} run ${ARG_BUILD_SCRIPT}
      WORKING_DIRECTORY ${ARG_PACKAGE_DIR}
      VERBATIM
    )
  endif()

  add_dependencies(${_target} ${_build_target})
endfunction()
# ==============================================================================
# nelson_install_module_data(<module_name>
#   [EXTRA_TEST_PATTERNS  pat1 pat2 ...]
#   [EXTRA_DIRS           dir1 dir2 ...]
#   [EXTRA_DIR_PATTERNS   "dir1:pat1;pat2" "dir2:pat1" ...]
# )
#
# Standard install rules for Nelson module data files (loader.m, etc/, tests/,
# help/, functions/).  Accepts additional test file patterns and extra
# directories with their file patterns.
# ==============================================================================
function(nelson_install_module_data _module_name)
  cmake_parse_arguments(ARG
    ""
    ""
    "EXTRA_TEST_PATTERNS;EXTRA_DIRS;EXTRA_DIR_PATTERNS"
    ${ARGN}
  )

  set(_dest "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${_module_name}")

  # loader.m
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/loader.m")
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/loader.m DESTINATION "${_dest}")
  endif()

  # etc/*.m
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/etc")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/etc
      DESTINATION "${_dest}"
      FILES_MATCHING PATTERN "*.m"
    )
  endif()

  # tests/*.m (always) plus extra patterns
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/tests")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
      DESTINATION "${_dest}"
      FILES_MATCHING PATTERN "*.m"
    )
    foreach(_pat IN LISTS ARG_EXTRA_TEST_PATTERNS)
      install(
        DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
        DESTINATION "${_dest}"
        FILES_MATCHING PATTERN "${_pat}"
      )
    endforeach()
    # Test fixtures ship in full (every file type, not just *.m): installed-build
    # tests read their data from tests/fixtures/ by convention, so a test without
    # its fixtures is dead weight. Kept small and canonical under tests/fixtures/.
    if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures")
      install(
        DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures
        DESTINATION "${_dest}/tests"
      )
    endif()
  endif()

  # help/: ship the compiled archives (*.nhz), the XML documentation sources
  # (*.xml) AND the help images (svg/png/jpg/gif).  The XML is read at runtime
  # by the modules' help-example and documentation tests (test_help_xml_examples,
  # the graphics property coverage tests, the compiler tutorial tests, ...); the
  # images back offline help rendering and the doc-build / help-image tests
  # (test_graphics_help_examples_have_images, buildhelpmd, buildhelptypst), which
  # run against the installed layout.  Shipping them keeps the installed
  # behaviour identical to the source tree instead of skipping those tests.
  # Markdown front-matter (md/) is not needed at runtime and stays excluded.
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/help")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
      DESTINATION "${_dest}"
      FILES_MATCHING
        PATTERN "*.nhz"
        PATTERN "*.xml"
        PATTERN "*.svg"
        PATTERN "*.png"
        PATTERN "*.jpg"
        PATTERN "*.jpeg"
        PATTERN "*.gif"
        PATTERN "md"
        EXCLUDE
    )
  endif()

  # functions/*.m
  if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module_name}/functions")
    install(
      DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_module_name}/functions
      DESTINATION "${_dest}"
      FILES_MATCHING PATTERN "*.m"
    )
  endif()

  # examples/*.m and the manifest are part of the canonical module layout.
  if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/examples")
    install(
      DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/examples
      DESTINATION "${_dest}"
      FILES_MATCHING
        PATTERN "*.m"
        PATTERN "*.json"
    )
  endif()

  # Extra directories: "dirname" -> install with PATTERN "*.*"
  #
  # When the spec names a nested path (e.g. "tests/mat"), install(DIRECTORY)
  # keeps only the LAST component ("mat") under DESTINATION, which would drop
  # the "tests/" parent and land the files at <module>/mat instead of
  # <module>/tests/mat.  Append the spec's parent path to DESTINATION so the
  # installed layout mirrors the source layout and the shipped tests find
  # their fixtures.
  foreach(_dir IN LISTS ARG_EXTRA_DIRS)
    if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}")
      get_filename_component(_dir_parent "${_dir}" DIRECTORY)
      set(_dir_dest "${_dest}")
      if(_dir_parent)
        set(_dir_dest "${_dest}/${_dir_parent}")
      endif()
      install(
        DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}
        DESTINATION "${_dir_dest}"
        FILES_MATCHING PATTERN "*.*"
      )
    endif()
  endforeach()

  # Extra directory + patterns: "dir:pat1,pat2".  Patterns are COMMA-separated,
  # not ";"-separated: a ";" inside an argument is split by CMake into separate
  # list elements at the call boundary, which would strip the "dir:" prefix off
  # every pattern but the first and silently drop them.  Commas survive intact.
  foreach(_spec IN LISTS ARG_EXTRA_DIR_PATTERNS)
    string(FIND "${_spec}" ":" _pos)
    if(_pos GREATER -1)
      string(SUBSTRING "${_spec}" 0 ${_pos} _dir)
      math(EXPR _start "${_pos} + 1")
      string(SUBSTRING "${_spec}" ${_start} -1 _pats_str)
      string(REPLACE "," ";" _pats "${_pats_str}")
      # Preserve the spec's parent path in DESTINATION (see note above), so
      # "tests/mat:*.mat" installs to <module>/tests/mat, not <module>/mat.
      get_filename_component(_dir_parent "${_dir}" DIRECTORY)
      set(_dir_dest "${_dest}")
      if(_dir_parent)
        set(_dir_dest "${_dest}/${_dir_parent}")
      endif()
      foreach(_pat IN LISTS _pats)
        if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}")
          install(
            DIRECTORY ${CMAKE_SOURCE_DIR}/modules/${_module_name}/${_dir}
            DESTINATION "${_dir_dest}"
            FILES_MATCHING PATTERN "${_pat}"
          )
        endif()
      endforeach()
    endif()
  endforeach()
endfunction()
# ==============================================================================
# nelson_module_sources_from_glob(<output_var>
#   DIRS dir1 [dir2 ...]
#   [EXCLUDE regex1 ...]
# )
#
# Glob .cpp and .c files from directories, filtering out dllMain and optional
# extra patterns.
# ==============================================================================
function(nelson_module_sources_from_glob _outvar)
  cmake_parse_arguments(ARG "" "" "DIRS;EXCLUDE" ${ARGN})
  set(_all_src)
  foreach(_dir IN LISTS ARG_DIRS)
    file(GLOB _cpp "${_dir}/*.cpp")
    file(GLOB _cxx "${_dir}/*.cxx")
    file(GLOB _cc "${_dir}/*.cc")
    file(GLOB _c "${_dir}/*.c")
    list(APPEND _all_src ${_cpp} ${_cxx} ${_cc} ${_c})
  endforeach()
  # Always exclude dllMain, including the legacy lowercase spelling.
  list(FILTER _all_src EXCLUDE REGEX "dll[Mm]ain\\.(cpp|c)$")
  foreach(_ex IN LISTS ARG_EXCLUDE)
    list(FILTER _all_src EXCLUDE REGEX "${_ex}")
  endforeach()
  set(${_outvar} "${_all_src}" PARENT_SCOPE)
endfunction()
