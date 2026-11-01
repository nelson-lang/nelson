//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NotebookEditor.hpp"
#include "WebDesktopBridge.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
// libnlsWeb_gui loads on first use; this module never links it and nelson-cli never loads it.
using OpenNotebookFn = bool (*)(void*, const wchar_t*);
using CloseNotebookHostFn = void (*)();
library_handle webGuiLibrary = nullptr;
OpenNotebookFn openNotebookFn = nullptr;
CloseNotebookHostFn closeNotebookHostFn = nullptr;
bool webGuiLibraryTried = false;
//=============================================================================
void
loadWebGuiBackend()
{
    if (webGuiLibraryTried) {
        return;
    }
    webGuiLibraryTried = true;
    const std::wstring filename = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory()
        + L"/libnlsWeb_gui" + get_dynamic_library_extensionW();
    webGuiLibrary = load_dynamic_libraryW(filename);
    if (webGuiLibrary == nullptr) {
        return;
    }
    openNotebookFn
        = reinterpret_cast<OpenNotebookFn>(get_function(webGuiLibrary, "NelsonWebGuiOpenNotebook"));
    closeNotebookHostFn = reinterpret_cast<CloseNotebookHostFn>(
        get_function(webGuiLibrary, "NelsonWebGuiCloseNotebookHost"));
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NotebookGateway::openNotebookEditor(Evaluator* eval, const std::wstring& filename)
{
    // The web desktop takes the document itself; the Qt desktop hosts it in the Notebook window.
    bool opened = WebDesktopBridge::getInstance()->openWebNotebook(filename);
    if (!opened
        && NelsonConfiguration::getInstance()->getNelsonEngineMode() == NELSON_ENGINE_MODE::GUI) {
        loadWebGuiBackend();
        if (openNotebookFn != nullptr) {
            opened = openNotebookFn(eval, filename.c_str());
        }
    }
    return opened;
}
//=============================================================================
void
Nelson::NotebookGateway::closeNotebookWindow()
{
    if (closeNotebookHostFn != nullptr) {
        closeNotebookHostFn();
    }
}
//=============================================================================
