//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Realizing a native figure window can throw even when a QApplication and a
// screen exist: a macOS background process without window-server access throws an
// Objective-C exception from AppKit, and unwinding it through Qt aborts the
// interpreter. This suite cannot stand up such an environment (a detached,
// non-activated process), so the guard is pinned as a source-level contract: every
// path that realizes a native window must latch the failure and recover.
//=============================================================================
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
//=============================================================================
namespace {
//=============================================================================
std::string
readSource(const std::filesystem::path& path)
{
    std::ifstream stream(path);
    std::ostringstream contents;
    contents << stream.rdbuf();
    return contents.str();
}
//=============================================================================
bool
contains(const std::string& source, const std::string& expected, const char* description)
{
    if (source.find(expected) != std::string::npos) {
        return true;
    }
    std::cerr << "Missing native-presentation contract: " << description << '\n';
    return false;
}
//=============================================================================
bool
ordered(const std::string& source, const std::string& first, const std::string& second,
    const char* description)
{
    const size_t firstPos = source.find(first);
    const size_t secondPos = source.find(second);
    if (firstPos != std::string::npos && secondPos != std::string::npos && firstPos < secondPos) {
        return true;
    }
    std::cerr << "Invalid native-presentation order: " << description << '\n';
    return false;
}
//=============================================================================
std::string
sliceFunction(const std::string& source, const std::string& signature, const std::string& next)
{
    const size_t start = source.find(signature);
    if (start == std::string::npos) {
        return "";
    }
    const size_t end = source.find(next, start + signature.size());
    if (end == std::string::npos) {
        return source.substr(start);
    }
    return source.substr(start, end - start);
}
//=============================================================================
} // namespace
//=============================================================================
int
main()
{
    const std::filesystem::path root(NELSON_SOURCE_ROOT);
    const std::string window = readSource(root / "modules/graphics_qt/src/cpp/GOWindow.cpp");
    const std::string windowHeader
        = readSource(root / "modules/graphics_qt/src/include/GOWindow.hpp");
    const std::string placement
        = readSource(root / "modules/graphics_qt/src/cpp/GOWindowPlacement.cpp");

    const std::string presentHost
        = sliceFunction(window, "GOWindow::presentHost()", "GOWindow::prepareHostClose");
    const std::string canShow = sliceFunction(window, "canShowFigureWidgets()", "//=====");
    const std::string presentNative = sliceFunction(
        placement, "GOWindow::presentNativeWindow()", "GOWindow::isTransientPlacement()");

    bool valid = true;

    // The latch is a process-wide, publicly reachable switch so every figure
    // window (and canShowFigureWidgets) can consult the same state.
    valid &= contains(windowHeader, "nativeWindowPresentationAvailable();",
        "the header exposes the native-presentation availability query");
    valid &= contains(windowHeader, "disableNativeWindowPresentation();",
        "the header exposes the native-presentation disable latch");

    // Once presentation is known to fail, no path even attempts it again.
    valid &= contains(canShow, "GOWindow::nativeWindowPresentationAvailable()",
        "canShowFigureWidgets stops showing widgets once presentation is disabled");
    valid &= contains(presentHost, "!nativeWindowPresentationAvailable()",
        "presentHost returns early once native presentation is disabled");
    valid &= contains(presentNative, "!nativeWindowPresentationAvailable()",
        "presentNativeWindow returns early once native presentation is disabled");

    // The realizing paths must catch and latch, never propagate.
    valid &= contains(
        presentHost, "catch (...)", "presentHost guards the docked/native presentation");
    valid &= contains(presentHost, "disableNativeWindowPresentation()",
        "presentHost latches the failure instead of propagating it");
    valid &= contains(
        presentNative, "show();", "presentNativeWindow realizes the native window via show()");
    valid &= contains(
        presentNative, "catch (...)", "presentNativeWindow guards the native window realization");
    valid &= ordered(presentNative, "show();", "catch (...)",
        "the window realization (show) sits inside the guarded block");
    valid &= contains(presentNative, "disableNativeWindowPresentation()",
        "presentNativeWindow latches the failure for the rest of the session");
    valid &= contains(presentNative, "QWidget::destroy()",
        "presentNativeWindow tears the half-built native window down on failure");

    return valid ? 0 : 1;
}
//=============================================================================
