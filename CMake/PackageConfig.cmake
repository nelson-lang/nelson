# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VENDOR "Allan CORNET")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
  "Nelson is a matrix/array programming language for engineering and\
 scientific computing. It provides an interpreter, plotting tools and\
 numerical libraries such as Eigen."
)
set(CPACK_PACKAGE_VERSION_MAJOR ${Nelson_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${Nelson_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${Nelson_VERSION_MAINTENANCE})
set(CPACK_PACKAGE_VERSION
  "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}.${Nelson_VERSION_BUILD}"
)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Nelson-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_CONTACT "nelson.numerical.computation@gmail.com")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://nelson-lang.github.io/nelson-website/")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/lgpl-3.0.md")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")
# ==============================================================================
# Target triple for package filename
# ==============================================================================
if(DEFINED CMAKE_CXX_COMPILER_TARGET)
  set(TARGET_TRIPLE "${CMAKE_CXX_COMPILER_TARGET}")
elseif(DEFINED CMAKE_SYSTEM_TARGET)
  set(TARGET_TRIPLE "${CMAKE_SYSTEM_TARGET}")
else()
  set(TARGET_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}")
endif()
# ==============================================================================
# Platform-specific CPack configuration
# ==============================================================================
if(UNIX)
  if(APPLE)
    # ========================================================================
    # macOS â€“ DragNDrop (.dmg) packaging
    # ========================================================================
    set(CPACK_GENERATOR "DragNDrop")
    set(CPACK_PACKAGE_FILE_NAME
      "Nelson-${CPACK_PACKAGE_VERSION}-macOS-${TARGET_TRIPLE}"
    )
    # -- DMG settings
    set(CPACK_DMG_VOLUME_NAME "Nelson ${CPACK_PACKAGE_VERSION}")
    set(CPACK_DMG_FORMAT
      "ULFO"
    ) # LZFSE-compressed read-only; fast to open, small (macOS 10.11+)
    set(
      NELSON_DMG_LAYOUT_BACKEND
      "DMGBUILD"
      CACHE
        STRING
        "macOS DMG layout backend: DMGBUILD, FINDER, or NONE"
    )
    set_property(
      CACHE NELSON_DMG_LAYOUT_BACKEND
      PROPERTY
        STRINGS
        DMGBUILD
        FINDER
        NONE
    )
    set(
      NELSON_DMGBUILD_PYTHONPATH
      ""
      CACHE
        STRING
        "Extra PYTHONPATH used to import dmgbuild for DMG layout"
    )
    # -- Generate .icns from the project PNG icon (only on macOS)
    set(_nelson_icns "${CMAKE_BINARY_DIR}/Nelson.icns")
    set(_nelson_source_png
      "${CMAKE_SOURCE_DIR}/desktop/icons/hicolor/512x512/apps/nelson.png"
    )
    if(NOT EXISTS "${_nelson_source_png}")
      set(_nelson_source_png "${CMAKE_SOURCE_DIR}/resources/fibonacci.png")
    endif()
    add_custom_command(
      OUTPUT "${_nelson_icns}"
      COMMAND ${CMAKE_COMMAND} -DSOURCE_PNG=${_nelson_source_png}
        -DOUTPUT_ICNS=${_nelson_icns} -P
        ${CMAKE_SOURCE_DIR}/CMake/macOS/generate_icns.cmake
      DEPENDS "${_nelson_source_png}"
      COMMENT "Generating Nelson.icns for macOS packaging"
      VERBATIM
    )
    if(ENABLE_PACKAGING)
      add_custom_target(nelson_icns ALL DEPENDS "${_nelson_icns}")
    else()
      add_custom_target(nelson_icns DEPENDS "${_nelson_icns}")
    endif()
    set(CPACK_PACKAGE_ICON "${_nelson_icns}")
    string(TOUPPER "${NELSON_DMG_LAYOUT_BACKEND}" _nelson_dmg_layout_backend)
    if(ENABLE_PACKAGING AND _nelson_dmg_layout_backend STREQUAL "DMGBUILD")
      find_package(Python3 COMPONENTS Interpreter REQUIRED)
      set(_dmgbuild_env)
      if(NELSON_DMGBUILD_PYTHONPATH)
        list(APPEND _dmgbuild_env "PYTHONPATH=${NELSON_DMGBUILD_PYTHONPATH}")
      endif()
      execute_process(
        COMMAND ${CMAKE_COMMAND} -E env ${_dmgbuild_env} "${Python3_EXECUTABLE}"
          -c "import dmgbuild"
        RESULT_VARIABLE _dmgbuild_import_result
        OUTPUT_QUIET ERROR_QUIET
      )
      if(NOT _dmgbuild_import_result EQUAL 0)
        # Robust fallback: never drop the DMG background silently.  When dmgbuild
        # is unavailable, warn loudly and fall back to the Finder (AppleScript)
        # layout, which still applies the background image.  The Finder backend
        # needs Automation permission for the build tool to control Finder; in a
        # headless CI install dmgbuild instead (pip install dmgbuild, or set
        # NELSON_DMGBUILD_PYTHONPATH) to keep the reproducible headless layout.
        message(WARNING
          "NELSON_DMG_LAYOUT_BACKEND=DMGBUILD but the Python package 'dmgbuild' "
          "is not importable. Falling back to the Finder AppleScript layout so "
          "the DMG background is still applied. Install dmgbuild "
          "('pip install dmgbuild' or set NELSON_DMGBUILD_PYTHONPATH) for the "
          "reproducible headless layout."
        )
        set(_nelson_dmg_layout_backend "FINDER")
        set(CPACK_DMG_DS_STORE_SETUP_SCRIPT
          "${CMAKE_SOURCE_DIR}/CMake/macOS/DMGSetup.scpt"
        )
        set(CPACK_DMG_BACKGROUND_IMAGE
          "${CMAKE_SOURCE_DIR}/CMake/macOS/dmg_background.png"
        )
      else()
        set(CPACK_DMG_DS_STORE_SETUP_SCRIPT
          "${CMAKE_SOURCE_DIR}/CMake/macOS/DMGVolumeIcon.scpt"
        )
      endif()
    elseif(ENABLE_PACKAGING AND _nelson_dmg_layout_backend STREQUAL "FINDER")
      set(CPACK_DMG_DS_STORE_SETUP_SCRIPT
        "${CMAKE_SOURCE_DIR}/CMake/macOS/DMGSetup.scpt"
      )
      # -- DMG background image (static, checked into the repo)
      set(CPACK_DMG_BACKGROUND_IMAGE
        "${CMAKE_SOURCE_DIR}/CMake/macOS/dmg_background.png"
      )
    elseif(ENABLE_PACKAGING AND _nelson_dmg_layout_backend STREQUAL "NONE")
      set(CPACK_DMG_DS_STORE_SETUP_SCRIPT
        "${CMAKE_SOURCE_DIR}/CMake/macOS/DMGVolumeIcon.scpt"
      )
    elseif(ENABLE_PACKAGING)
      message(FATAL_ERROR
        "Unsupported NELSON_DMG_LAYOUT_BACKEND: ${NELSON_DMG_LAYOUT_BACKEND}"
      )
    endif()
    # -- Configure Info.plist
    file(READ "${CMAKE_SOURCE_DIR}/CMake/macOS/FileAssociations.plist"
      NELSON_FILE_ASSOCIATIONS
    )
    configure_file(
      "${CMAKE_SOURCE_DIR}/CMake/macOS/Info.plist.in"
      "${CMAKE_BINARY_DIR}/Info.plist"
      @ONLY
    )
    # -- .app bundle skeleton (launcher, Info.plist, icon) is installed
    #    in cpack_fixup_bundle.cmake.in at CPack time only, so that
    #    "cmake --install ." works without requiring bundle paths.
    #
    # Application content goes into Contents/Resources
    # (the normal install rules already install into CMAKE_INSTALL_PREFIX;
    #  we set CPACK_PACKAGING_INSTALL_PREFIX so the install tree ends up
    #  inside the .app bundle's Resources directory.)
    set(CPACK_PACKAGING_INSTALL_PREFIX "/Nelson.app/Contents/Resources")
    set(CPACK_DMG_SLA_USE_RESOURCE_FILE_LICENSE ON)
    # -- Symlink to /Applications in the DMG root (standard macOS convention)
    set(CPACK_DMG_DISABLE_APPLICATIONS_SYMLINK OFF)
    # -- Configure the CPack pre-build fixup script.
    #    CPACK_PRE_BUILD_SCRIPTS runs after all install rules have been
    #    executed but before CPack creates the package.  At that point
    #    the variable CPACK_TEMPORARY_INSTALL_DIRECTORY is available
    #    and the full .app bundle tree exists in the staging area.
    configure_file(
      "${CMAKE_SOURCE_DIR}/CMake/macOS/cpack_fixup_bundle.cmake.in"
      "${CMAKE_BINARY_DIR}/cpack_fixup_bundle.cmake"
      @ONLY
    )
    configure_file(
      "${CMAKE_SOURCE_DIR}/CMake/macOS/cpack_dmg_file_icon.cmake.in"
      "${CMAKE_BINARY_DIR}/cpack_dmg_file_icon.cmake"
      @ONLY
    )
    if(ENABLE_PACKAGING AND _nelson_dmg_layout_backend STREQUAL "DMGBUILD")
      configure_file(
        "${CMAKE_SOURCE_DIR}/CMake/macOS/dmgbuild_settings.py.in"
        "${CMAKE_BINARY_DIR}/dmgbuild_settings.py"
        @ONLY
      )
      configure_file(
        "${CMAKE_SOURCE_DIR}/CMake/macOS/cpack_dmgbuild_layout.cmake.in"
        "${CMAKE_BINARY_DIR}/cpack_dmgbuild_layout.cmake"
        @ONLY
      )
    endif()
    list(APPEND CPACK_PRE_BUILD_SCRIPTS
      "${CMAKE_BINARY_DIR}/cpack_fixup_bundle.cmake"
    )
    if(ENABLE_PACKAGING AND _nelson_dmg_layout_backend STREQUAL "DMGBUILD")
      list(APPEND CPACK_POST_BUILD_SCRIPTS
        "${CMAKE_BINARY_DIR}/cpack_dmgbuild_layout.cmake"
      )
    endif()
    list(APPEND CPACK_POST_BUILD_SCRIPTS
      "${CMAKE_BINARY_DIR}/cpack_dmg_file_icon.cmake"
    )
    if(ENABLE_PACKAGING)
      message(STATUS "macOS DMG packaging enabled (DragNDrop)")
      message(STATUS "  Run: cmake --build . -- package")
    endif()
  else()
    # ========================================================================
    # Linux / other UNIX
    # ========================================================================
    set(CPACK_GENERATOR "TGZ")
    set(CPACK_PACKAGE_FILE_NAME
      "nelson-Unknown-${TARGET_TRIPLE}-v${CPACK_PACKAGE_VERSION}"
    )
    find_file(DEBIAN_FOUND debian_version debconf.conf PATHS /etc)
    find_file(FEDORA_FOUND fedora-release PATHS /etc)
    find_file(REDHAT_FOUND redhat-release inittab.RH PATHS /etc)
    find_file(UBUNTU_EXTRA legal issue PATHS /etc)

    if(UBUNTU_EXTRA)
      file(STRINGS ${UBUNTU_EXTRA} UBUNTU_FOUND REGEX Ubuntu)
      if(UBUNTU_FOUND)
        set(CMAKE_OS_NAME "Ubuntu" CACHE STRING "Operating system name" FORCE)
        set(DEBIAN_FOUND FALSE)
      endif()
    endif()

    if(UBUNTU_FOUND)
      execute_process(
        COMMAND lsb_release -rs
        OUTPUT_VARIABLE UBUNTU_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      set(CPACK_GENERATOR "DEB")
      set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Allan CORNET")
      set(CPACK_DEBIAN_PACKAGE_HOMEPAGE
        "https://nelson-lang.github.io/nelson-website/"
      )
      set(CPACK_PACKAGE_FILE_NAME
        "nelson-Ubuntu-${UBUNTU_VERSION}-${TARGET_TRIPLE}-v${CPACK_PACKAGE_VERSION}"
      )
      # the runner architecture, amd64 or arm64: apt refuses a package built for another one
      execute_process(
        COMMAND dpkg --print-architecture
        OUTPUT_VARIABLE CPACK_DEBIAN_PACKAGE_ARCHITECTURE
        OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      if(UBUNTU_VERSION VERSION_EQUAL "26.04")
        # WebKitGTK is loaded at run time by the native WebView window, apt installs it by default
        set(CPACK_DEBIAN_PACKAGE_RECOMMENDS "cmake, libwebkit2gtk-4.1-0")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "libfftw3-bin, libfftw3-double3, libfftw3-single3"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qt6-declarative-dev"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-templates"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-controls"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-window"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-dialogs"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtqml-workerscript"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-layouts"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libasound2t64 (>= 1.0.16)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libc6 (>= 2.43)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libcurl4t64 (>= 7.16.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libffi8 (>= 3.4)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgcc-s1 (>= 4.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgif7 (>= 5.1)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgit2-1.9 (>= 1.9.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgomp1 (>= 4.2.1)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libhdf5-310 (>= 1.14.3)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libicu78 (>= 78.1-1~)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libjack-jackd2-0 (>= 1.9.10+20150825) | libjack-0.125"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libmatio14 (>= 1.5.30)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenblas0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libarpack2t64"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenmpi40 (>= 5.0.10)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libpcre2-32-0 (>= 10.22)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libportaudio2 (>= 19+svn20101113)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6core6t64 (>= 6.10.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6gui6 (>= 6.9.1)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6printsupport6 (>= 6.1.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6qml6 (>= 6.6.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6quick6 (>= 6.6.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6svg6 (>= 6.6.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6widgets6 (>= 6.3.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsndfile1 (>= 1.0.20)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libstdc++6 (>= 14)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtag2 (>= 2.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtbb12 (>= 2021.4.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtiff6 (>= 4.0.3)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libxml2-16 (>= 2.14.1)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libxslt1.1 (>= 1.1.25)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, zlib1g (>= 1:1.1.4)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqhull-r8.0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsqlite3-0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libnetcdf22"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libarrow2300"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libparquet2300"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libwebp7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-core7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-cvodes7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-idas6"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-nvecserial7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-sunmatrix5"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-sunlinsol5"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-kinsol7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libhighs1"
        )
      endif()
      if(UBUNTU_VERSION VERSION_EQUAL "24.04")
        # WebKitGTK is loaded at run time by the native WebView window, apt installs it by default
        set(CPACK_DEBIAN_PACKAGE_RECOMMENDS "cmake, libwebkit2gtk-4.1-0")
        # NOTE: libfftw3-bin, libfftw3-double3, libfftw3-single3 are separate deps
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "libfftw3-bin, libfftw3-double3, libfftw3-single3"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenblas0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libarpack2t64"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libasound2t64 (>= 1.2.11)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libc6 (>= 2.39)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libcurl4t64 (>= 8.5.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libffi8 (>= 3.4)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgcc-s1 (>= 14-20240412-0ubuntu1)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgit2-1.7 (>= 1.7.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgomp1 (>= 14)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libhdf5-103-1t64 (>=1.10.10+repack-3.1ubuntu4)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libicu74 (>= 74.2~)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libjack-jackd2-0 (>= 1.9.10+20150825) | libjack-0.125"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libmatio11 (>= 1.5.26)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenblas0  (>= 0.3.26)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libopenmpi3t64 (>= 4.1.6)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libportaudio2 (>= 19.6.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libpcre2-32-0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6core6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6gui6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6printsupport6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6qml6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6quick6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6svg6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqt6widgets6 (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsndfile1 (>= 1.2.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libstdc++6 (>= 14)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtag1v5 (>= 1.13.1)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libxml2 (>= 2.9.14)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libxslt1.1 (>= 1.1.0)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, zlib1g (>= 1:1.3)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qt6-declarative-dev (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-templates (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-controls (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-window (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-dialogs (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtqml-workerscript (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, qml6-module-qtquick-layouts (>= 6.4.2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtbb12 (>= 2021.11.0-2ubuntu2)"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libgif7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libtiff6"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libqhull-r8.0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsqlite3-0"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libnetcdf19"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libwebp7"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-cvodes6"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-idas5"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-nvecserial6"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-sunmatrix4"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-sunlinsol3"
        )
        set(CPACK_DEBIAN_PACKAGE_DEPENDS
          "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libsundials-kinsol6"
        )
      endif()

      if(NOT UBUNTU_VERSION VERSION_EQUAL "26.04"
        AND NOT UBUNTU_VERSION VERSION_EQUAL "24.04"
      )
        message(STATUS
          "Ubuntu ${UBUNTU_VERSION}: DEB dependency list not available, using TGZ."
        )
        set(CPACK_GENERATOR "TGZ")
        set(CPACK_PACKAGE_FILE_NAME
          "nelson-Ubuntu-${UBUNTU_VERSION}-${TARGET_TRIPLE}-v${CPACK_PACKAGE_VERSION}"
        )
      endif()
    endif()

    if(DEBIAN_FOUND AND NOT UBUNTU_FOUND)
      message(STATUS "Debian detected â€“ packaging as TGZ.")
      set(CPACK_GENERATOR "TGZ")
      set(CPACK_PACKAGE_FILE_NAME
        "nelson-Debian-${TARGET_TRIPLE}-v${CPACK_PACKAGE_VERSION}"
      )
    endif()
  endif()
else()
  # Non-UNIX (should not normally be reached on Linux/macOS builds)
  message(STATUS "Non-UNIX platform â€“ packaging as ZIP.")
  set(CPACK_GENERATOR "ZIP")
  set(CPACK_PACKAGE_FILE_NAME
    "nelson-Unknown-${TARGET_TRIPLE}-v${CPACK_PACKAGE_VERSION}"
  )
endif()
# ==============================================================================
include(CPack)
if(APPLE AND TARGET package AND TARGET nelson_icns)
  add_dependencies(package nelson_icns)
endif()
