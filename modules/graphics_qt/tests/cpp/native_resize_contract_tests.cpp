//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
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
    std::cerr << "Missing native-resize contract: " << description << '\n';
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
    std::cerr << "Invalid native-resize order: " << description << '\n';
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
    const std::string baseFigure
        = readSource(root / "modules/graphics_qt/src/cpp/BaseFigureQt.cpp");
    const std::string baseFigureHeader
        = readSource(root / "modules/graphics_qt/src/include/BaseFigureQt.hpp");

    const std::string resizeEvent = sliceFunction(
        window, "GOWindow::resizeEvent(QResizeEvent* event)", "GOWindow::ensureBaseFigureWidget()");
    const std::string nativeEvent = sliceFunction(window, "GOWindow::nativeEvent",
        "#endif\n//=============================================================================\n/"
        "/============================================================================="
        "\nvoid\nGOWindow::resizeEvent");
    const std::string service = sliceFunction(window,
        "GOWindow::serviceNativeResize(bool force, const QSize& oldWindowSize)", "#if QT_VERSION");
    const std::string timerEvent = sliceFunction(
        baseFigure, "BaseFigureQt::timerEvent(QTimerEvent* event)", "BaseFigureQt::getFrame()");
    const std::string baseResizeEvent = sliceFunction(baseFigure,
        "BaseFigureQt::resizeEvent(QResizeEvent* e)", "BaseFigureQt::flushPendingResize()");

    bool valid = true;
    valid &= contains(windowHeader, "resizeEvent(QResizeEvent* event) override",
        "the top-level figure window observes Qt resize events");
    valid &= ordered(resizeEvent, "QMainWindow::resizeEvent(event)", "serviceNativeResize(false,",
        "native resize service runs after Qt has updated the central widget geometry");
    valid &= contains(resizeEvent, "event->oldSize()",
        "the fallback path computes the new drawing size from the native window resize");
    valid &= contains(nativeEvent, "WM_ENTERSIZEMOVE", "Windows native drag loop entry is tracked");
    valid &= contains(nativeEvent, "WM_SIZE", "Windows native resize messages provide client size");
    valid &= contains(nativeEvent, "pendingNativeResizeClientSize",
        "the native client size is retained for the non-OpenGL fallback");
    valid &= contains(nativeEvent, "WM_EXITSIZEMOVE", "Windows native drag loop exit is tracked");
    valid &= contains(nativeEvent, "WM_CAPTURECHANGED",
        "Windows native drag cancellation by capture loss closes the resize loop");
    valid &= contains(
        nativeEvent, "WM_CANCELMODE", "Windows native drag cancellation closes the resize loop");
    valid &= contains(nativeEvent, "serviceNativeResize(true)",
        "the final native drag size is flushed on loop exit");
    valid &= contains(service, "syncFigureSizeFromWidget(goFig, qtchild, this, oldWindowSize",
        "figures without an OpenGL drawing surface still update their model size");
    valid &= contains(service, "if (!force && !inNativeResizeLoop)",
        "ordinary Qt resize events do not use the native Windows resize pump");
    valid &= contains(service, "pendingNativeResizeClientSize",
        "the non-OpenGL fallback consumes the retained native client size");
    valid &= contains(service, "baseFigure->flushPendingResize()",
        "coalesced OpenGL resize events are flushed during native sizing");
    valid &= contains(service, "processPendingCallbacksAndTimers()",
        "SizeChangedFcn callbacks are pumped while Windows is in its sizing loop");
    valid &= contains(baseResizeEvent, "parentWindow->serviceNativeResize(false)",
        "the OpenGL child flushes after receiving its own resize event");
    valid &= contains(baseFigureHeader, "flushPendingResize()",
        "BaseFigureQt exposes the resize flush used by the native Windows path");
    valid &= contains(timerEvent, "flushPendingResize()",
        "the regular coalescing timer and the native path share one flush implementation");
    return valid ? 0 : 1;
}
//=============================================================================
