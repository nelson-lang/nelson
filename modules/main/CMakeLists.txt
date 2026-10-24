# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name main)
# ==============================================================================
set(_main_common_sources
  "${CMAKE_CURRENT_SOURCE_DIR}/commons/NelsonRunner.cpp"
)
if(WIN32)
  list(APPEND _main_common_sources
    "${CMAKE_CURRENT_SOURCE_DIR}/commons/StartNelsonDynamicFunction.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/nelsonc/Packager.manifest"
  )
endif()

function(_nelson_configure_main_executable _target _output_name)
  if(WIN32)
    set_target_properties(${_target} PROPERTIES OUTPUT_NAME "${_output_name}")
    target_link_options(${_target} PRIVATE /STACK:62914560)
  endif()
endfunction()

if(TARGET nlsCompiler_builtin)
  add_subdirectory(nelsonc)
endif()

if(APPLE)
  function(_nelson_stage_macos_application
    _target
    _bundle_name
    _bundle_display_name
    _bundle_identifier
  )
    set(_bundle "${BIN_DIRECTORY}/${_bundle_name}.app")
    set(_bundle_icon "${_bundle}/Contents/Resources/Nelson.icns")
    set(_info_plist "${CMAKE_BINARY_DIR}/temp/${_target}-Info.plist")
    set(NELSON_BUNDLE_DISPLAY_NAME "${_bundle_display_name}")
    set(NELSON_BUNDLE_EXECUTABLE "${_target}")
    set(NELSON_BUNDLE_IDENTIFIER "${_bundle_identifier}")
    set(NELSON_FILE_ASSOCIATIONS "")
    if(_target STREQUAL "nelson-gui-exec")
      file(READ "${CMAKE_SOURCE_DIR}/CMake/macOS/FileAssociations.plist"
        NELSON_FILE_ASSOCIATIONS
      )
    endif()
    file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/temp")
    configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/macos/Info.plist.in"
      "${_info_plist}"
      @ONLY
    )
    add_custom_command(
      TARGET ${_target}
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E remove_directory "${_bundle}"
      COMMAND ${CMAKE_COMMAND} -E make_directory "${_bundle}/Contents/MacOS"
        "${_bundle}/Contents/Resources"
      COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:${_target}>"
        "${_bundle}/Contents/MacOS/${_target}"
      COMMAND ${CMAKE_COMMAND} -E copy "${_info_plist}"
        "${_bundle}/Contents/Info.plist"
      COMMAND ${CMAKE_COMMAND}
        -DSOURCE_PNG=${CMAKE_SOURCE_DIR}/desktop/icons/hicolor/512x512/apps/nelson.png
        -DOUTPUT_ICNS=${_bundle_icon} -P
        ${CMAKE_SOURCE_DIR}/CMake/macOS/generate_icns.cmake
      COMMENT "Creating ${_bundle_name}.app launcher"
      VERBATIM
    )
    set_property(
      TARGET ${_target}
      APPEND
      PROPERTY
        ADDITIONAL_CLEAN_FILES
        "${_bundle}"
        "${_info_plist}"
    )
  endfunction()
endif()
# ==============================================================================
file(GLOB MAINCLI_SRC
  "${CMAKE_CURRENT_SOURCE_DIR}/nelson_cli/*.cpp"
  ${_main_common_sources}
)

add_executable(nelson-cli-exec ${MAINCLI_SRC})
_nelson_configure_main_executable(nelson-cli-exec nelson-cli)
target_include_directories(nelson-cli-exec
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/commons
)
if(WIN32)
  set(_nelson_cli_rc "${CMAKE_CURRENT_SOURCE_DIR}/nelson_cli/resource.rc")
  if(EXISTS "${_nelson_cli_rc}")
    target_sources(nelson-cli-exec PRIVATE "${_nelson_cli_rc}")
  endif()
endif()

if(NOT WITHOUT_HELP_TOOLS_MODULE)
  set(HELP_TOOLS_LIBRARIES nlsHelp_tools nlsHelp_tools_builtin)
endif()

target_link_libraries(nelson-cli-exec
  PRIVATE
    nlsEngine
    nlsModules_manager
    nlsModules_manager_builtin
    nlsMemory_manager
    nlsMemory_manager_builtin
    ${HELP_TOOLS_LIBRARIES}
)
# ==============================================================================
include("${CMAKE_SOURCE_DIR}/CMake/PortableMainTarget.cmake")
# ==============================================================================
if(NOT NELSON_PORTABLE_BUILD)
  file(GLOB MAINLINT_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_lint/*.cpp")
  add_executable(nelson-lint-exec ${MAINLINT_SRC})
  _nelson_configure_main_executable(nelson-lint-exec nelson-lint)
  set_target_properties(nelson-lint-exec PROPERTIES OUTPUT_NAME "nelson-lint")
  target_include_directories(nelson-lint-exec
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
  )
  target_link_libraries(nelson-lint-exec
    PRIVATE
      nlsInterpreter
      nlsTypes
      nlsCharacters_encoding
      nlsStream_manager
  )
  # ==============================================================================
  file(GLOB MAINLSP_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_lsp/*.cpp")
  add_executable(nelson-lsp-exec ${MAINLSP_SRC})
  _nelson_configure_main_executable(nelson-lsp-exec nelson-lsp)
  set_target_properties(nelson-lsp-exec PROPERTIES OUTPUT_NAME "nelson-lsp")
  target_include_directories(nelson-lsp-exec
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
  )
  target_link_libraries(nelson-lsp-exec
    PRIVATE
      nlsInterpreter
      nlsTypes
      nlsCharacters_encoding
      nlsStream_manager
  )
  # ==============================================================================
  file(GLOB MAINFORMAT_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nelson_format/*.cpp")
  add_executable(nelson-format-exec ${MAINFORMAT_SRC})
  _nelson_configure_main_executable(nelson-format-exec nelson-format)
  set_target_properties(nelson-format-exec
    PROPERTIES OUTPUT_NAME "nelson-format"
  )
  target_include_directories(nelson-format-exec
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
  )
  target_link_libraries(nelson-format-exec
    PRIVATE
      nlsInterpreter
      nlsTypes
      nlsCharacters_encoding
      nlsStream_manager
      nlsError_manager
  )
endif()
# ==============================================================================
if(NOT WITHOUT_GUI_MODULE)
  file(GLOB MAINADVCLI_SRC
    "${CMAKE_CURRENT_SOURCE_DIR}/nelson_adv_cli/*.cpp"
    ${_main_common_sources}
  )
  add_executable(nelson-adv-cli-exec ${MAINADVCLI_SRC})
  _nelson_configure_main_executable(nelson-adv-cli-exec nelson-adv-cli)
  target_include_directories(nelson-adv-cli-exec
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/commons
  )
  if(WIN32)
    set(_nelson_adv_cli_rc
      "${CMAKE_CURRENT_SOURCE_DIR}/nelson_adv_cli/resource.rc"
    )
    if(EXISTS "${_nelson_adv_cli_rc}")
      target_sources(nelson-adv-cli-exec PRIVATE "${_nelson_adv_cli_rc}")
    endif()
  endif()
  target_link_libraries(nelson-adv-cli-exec PRIVATE nlsEngine)
endif()
# ==============================================================================
if(NOT WITHOUT_GUI_MODULE)
  file(GLOB MAINGUI_SRC
    "${CMAKE_CURRENT_SOURCE_DIR}/nelson_gui/*.cpp"
    ${_main_common_sources}
  )
  if(WIN32)
    add_executable(nelson-gui-exec WIN32 ${MAINGUI_SRC})
  else()
    add_executable(nelson-gui-exec ${MAINGUI_SRC})
  endif()
  _nelson_configure_main_executable(nelson-gui-exec nelson-gui)
  target_include_directories(nelson-gui-exec
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/commons
  )
  if(WIN32)
    set(_nelson_gui_rc "${CMAKE_CURRENT_SOURCE_DIR}/nelson_gui/resource.rc")
    if(EXISTS "${_nelson_gui_rc}")
      target_sources(nelson-gui-exec PRIVATE "${_nelson_gui_rc}")
    endif()
  endif()
  target_link_libraries(nelson-gui-exec PRIVATE nlsEngine)
  if(APPLE)
    _nelson_stage_macos_application(
      nelson-gui-exec
      "Nelson"
      "Nelson"
      "${NELSON_APP_ID}"
    )
  endif()
endif()
# ==============================================================================
# Nelson WebView is opt-in (OFF by default) until the web_gui module + frontend
# bundle are built. Enable with -DWITH_WEB_GUI_MODULE=ON. See
# modules/web_gui/INTEGRATION.md.
if(WITH_WEB_GUI_MODULE)
  file(GLOB MAINWEB_SRC
    "${CMAKE_CURRENT_SOURCE_DIR}/nelson_web/*.cpp"
    ${_main_common_sources}
  )
  if(WIN32)
    add_executable(nelson-webview-exec WIN32 ${MAINWEB_SRC})
  else()
    add_executable(nelson-webview-exec ${MAINWEB_SRC})
  endif()
  _nelson_configure_main_executable(nelson-webview-exec nelson-webview)
  target_include_directories(nelson-webview-exec
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/../engine/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/../nelson_manager/src/include
      ${CMAKE_CURRENT_SOURCE_DIR}/commons
  )
  if(WIN32)
    set(_nelson_web_rc "${CMAKE_CURRENT_SOURCE_DIR}/nelson_web/resource.rc")
    if(EXISTS "${_nelson_web_rc}")
      target_sources(nelson-webview-exec PRIVATE "${_nelson_web_rc}")
    endif()
  endif()
  target_link_libraries(nelson-webview-exec
    PRIVATE
      nlsEngine
      nlsNelson_manager
      nlsWeb_gui
  )
  if(WIN32)
    add_custom_command(
      TARGET nelson-webview-exec
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E remove -f ${BIN_DIRECTORY}/nelson-web.exe
      COMMENT "Removing stale nelson-web executable"
    )
  endif()
  if(APPLE)
    _nelson_stage_macos_application(
      nelson-webview-exec
      "Nelson WebView"
      "Nelson WebView"
      "${NELSON_APP_ID}.webview"
    )
  endif()
  if(UNIX)
    add_custom_command(
      TARGET nelson-webview-exec
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E remove -f ${BIN_DIRECTORY}/nelson-web
      COMMAND ${CMAKE_COMMAND} -E remove -f ${BIN_DIRECTORY}/nelson-web-exec
      COMMAND ${CMAKE_COMMAND} -E remove -f ${BIN_DIRECTORY}/nelson-webview
      COMMAND ${CMAKE_COMMAND} -E remove_directory
        "${BIN_DIRECTORY}/Nelson Web.app"
      COMMAND ${CMAKE_COMMAND} -E create_symlink nelson
        ${BIN_DIRECTORY}/nelson-webview
      COMMENT "Creating nelson-webview launcher"
    )
  endif()
endif()
# ==============================================================================
if(NOT WITHOUT_WEBTOOLS_MODULE)
  set(GET_MODULE_SKELETON_ACTION
    COMMAND
    ${BIN_DIRECTORY}/nelson-cli
    --no-ipc
    --no-user-modules
    --quiet
    --file
    "${BIN_DIRECTORY}/../../tools/clone_module_skeleton/clone.m"
  )
  set(BUILD_ALL_GET_MODULE_SKELETON_ACTION ${GET_MODULE_SKELETON_ACTION})
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  set(BUILDHELP_ACTION
    COMMAND
    ${CMAKE_COMMAND}
    -E
    env
    'QTDIR_BINARIES=${QTDIR_BINARIES}'
    ${BIN_DIRECTORY}/nelson-cli
    --no-ipc
    --no-user-modules
    --quiet
    --file
    "${BIN_DIRECTORY}/../../tools/buildhelp/buildhelp.m"
  )
  set(BUILD_ALL_BUILDHELP_ACTION ${BUILDHELP_ACTION})
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  set(TESTS_MINIMAL_ACTION
    COMMAND
    ${BIN_DIRECTORY}/nelson-cli
    --no-user-modules
    --no-ipc
    --quiet
    --file
    "${BIN_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.m"
  )
  set(BUILD_ALL_TESTS_MINIMAL_ACTION ${TESTS_MINIMAL_ACTION})
endif()
# ==============================================================================
include("${CMAKE_SOURCE_DIR}/CMake/PortableMainTests.cmake")
# ==============================================================================
list(APPEND executableList nelson-cli-exec)
if(NOT NELSON_PORTABLE_BUILD)
  list(APPEND executableList
    nelson-lint-exec
    nelson-lsp-exec
    nelson-format-exec
  )
endif()
if(TARGET nelsonc)
  list(APPEND executableList nelsonc nelson-app)
endif()
if(TARGET nelson-app-windowed)
  list(APPEND executableList nelson-app-windowed)
endif()
if(NOT WITHOUT_GUI_MODULE)
  list(APPEND executableList
    nelson-gui-exec
    nelson-adv-cli-exec
  )
endif()
if(WITH_WEB_GUI_MODULE)
  list(APPEND executableList
    nelson-webview-exec
  )
endif()
add_custom_target(Name ALL
  COMMAND ${BUILD_ALL_LOCALIZATION_ACTION}
  DEPENDS ${executableList}
)
# ==============================================================================
# Ensure all builtin DLLs are built when building any executable target.
# The builtin libraries are loaded at runtime via AddGateway (not linked), so
# they are not automatically pulled in by the linker dependency chain.
get_property(_nelson_all_builtins GLOBAL PROPERTY NELSON_ALL_BUILTIN_TARGETS)
if(_nelson_all_builtins)
  foreach(_exec IN LISTS executableList)
    add_dependencies(${_exec} ${_nelson_all_builtins})
  endforeach()
endif()
# ==============================================================================
if(NOT WITHOUT_LOCALIZATION_MODULE)
  add_custom_target(update_localization
    COMMAND ${BIN_DIRECTORY}/nelson-cli --no-user-modules --no-ipc --quiet
      --file "${BIN_DIRECTORY}/../../tools/i18n/i18nExtractor.m"
    DEPENDS nelson-cli-exec
  )
endif()
# ==============================================================================
if(NOT WITHOUT_WEBTOOLS_MODULE)
  add_custom_target(get_module_skeleton
    COMMAND ${GET_MODULE_SKELETON_ACTION}
    DEPENDS nelson-cli-exec
  )
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  add_custom_target(buildhelp
    COMMAND ${BUILDHELP_ACTION}
    DEPENDS nelson-cli-exec
    COMMENT "Building help documentation"
  )

  if(BUILD_HELP)
    add_custom_target(build_with_help ALL
      DEPENDS buildhelp
      COMMENT "Triggering help build as part of the default build"
    )
  endif()
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
  add_custom_target(missing_help
    COMMAND ${BIN_DIRECTORY}/nelson-adv-cli --no-user-modules --no-ipc --quiet
      --file "${BIN_DIRECTORY}/../../tools/missing_help/missing_help.m"
    DEPENDS nelson-cli-exec
  )
endif()
# ==============================================================================
if(NOT WITHOUT_TESTS_MANAGER_MODULE)
  add_custom_target(tests_all
    COMMAND ${BIN_DIRECTORY}/nelson-adv-cli --quiet --no-ipc --file
      "${BIN_DIRECTORY}/../../tools/tests_all/runtests_all.m"
    DEPENDS nelson-cli-exec
  )
  add_custom_target(benchmark_all
    COMMAND ${BIN_DIRECTORY}/nelson-adv-cli --quiet --no-ipc --file
      "${BIN_DIRECTORY}/../../tools/benchmark_all/runbenchmark_all.m"
    DEPENDS nelson-cli-exec
  )
  add_custom_target(tests_all_no_display
    COMMAND ${BIN_DIRECTORY}/nelson-cli --quiet --no-ipc --file
      "${BIN_DIRECTORY}/../../tools/tests_all/runtests_all.m"
    DEPENDS nelson-cli-exec
  )
  add_custom_target(benchmark_all_no_display
    COMMAND ${BIN_DIRECTORY}/nelson-cli --quiet --no-ipc --file
      "${BIN_DIRECTORY}/../../tools/benchmark_all/runbenchmark_all.m"
    DEPENDS nelson-cli-exec
  )
  add_custom_target(tests_minimal
    COMMAND ${BIN_DIRECTORY}/nelson-cli --no-user-modules --no-ipc --quiet
      --file "${BIN_DIRECTORY}/../../tools/tests_minimal/runtests_minimal.m"
    DEPENDS nelson-cli-exec
  )
endif()
# ==============================================================================
include(GNUInstallDirs)

set(_script_perms
  WORLD_EXECUTE
  WORLD_READ
  GROUP_EXECUTE
  GROUP_READ
  OWNER_EXECUTE
  OWNER_READ
  OWNER_WRITE
)

if(NOT NELSON_WASM_BUILD AND NOT NELSON_PORTABLE_BUILD)
  install(
    FILES ${BIN_DIRECTORY}/nelson
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS ${_script_perms}
  )
  install(
    FILES ${BIN_DIRECTORY}/nelson-cli
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS ${_script_perms}
  )
else()
  install(
    FILES
      "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-runtime.mjs"
      "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker.mjs"
      "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-worker-client.mjs"
      "${CMAKE_SOURCE_DIR}/tools/wasm/nelson-workspace-store.mjs"
      "${BIN_DIRECTORY}/nelson-portable.wasm"
      "${BIN_DIRECTORY}/nelson-portable.data"
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    OPTIONAL
  )
endif()

if(NOT WITHOUT_GUI_MODULE)
  install(
    FILES ${BIN_DIRECTORY}/nelson-adv-cli
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS ${_script_perms}
  )
  install(
    FILES ${BIN_DIRECTORY}/nelson-gui
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS ${_script_perms}
  )
endif()

if(WITH_WEB_GUI_MODULE AND UNIX)
  install(
    FILES ${BIN_DIRECTORY}/nelson-webview
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    PERMISSIONS ${_script_perms}
  )
endif()

install(TARGETS nelson-cli-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
if(NOT NELSON_PORTABLE_BUILD)
  install(TARGETS nelson-lint-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(TARGETS nelson-lsp-exec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(
    TARGETS nelson-format-exec
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endif()
if(NOT WITHOUT_GUI_MODULE)
  install(
    TARGETS
      nelson-adv-cli-exec
      nelson-gui-exec
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endif()
if(WITH_WEB_GUI_MODULE)
  install(
    TARGETS nelson-webview-exec
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
  if(APPLE)
    install(
      DIRECTORY "${BIN_DIRECTORY}/Nelson WebView.app"
      DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
  endif()
endif()
# ==============================================================================
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/help
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}"
  FILES_MATCHING
    PATTERN "*.nhz"
    PATTERN "*.xml"
    PATTERN "md"
    EXCLUDE
)
# ==============================================================================
# The main module has no Nelson test suites of its own; its tests/fixtures tree
# only hosts fixtures consumed by the compiler and modules_manager suites via
# nelsonroot()/modules/main/tests/fixtures. Ship those fixtures (every file
# type, not just *.m: the suites also fileread .txt and native sources).
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures
  DESTINATION "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules/${module_name}/tests"
)
