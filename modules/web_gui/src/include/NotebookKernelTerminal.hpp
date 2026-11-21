//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Interface.hpp"
#include "nlsWeb_gui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NotebookKernel;
//=============================================================================
// Nelson::Interface for the notebook kernel. getLine() blocks until the kernel's
// stdin reader hands it the next cell's code; the output methods are captured and
// forwarded to the kernel as notebook.output notifications, never written to the
// raw stdout that carries the JSON-RPC frames.
class NLSWEB_GUI_IMPEXP NotebookKernelTerminal : public Interface
{
public:
    explicit NotebookKernelTerminal(NotebookKernel* kernel);
    ~NotebookKernelTerminal() override;

    std::string
    getLine(const std::string& prompt) override;
    std::wstring
    getLine(const std::wstring& prompt) override;
    std::wstring
    getInput(const std::wstring& prompt) override;

    size_t
    getTerminalWidth() override;
    size_t
    getTerminalHeight() override;

    void
    outputMessage(const std::string& msg) override;
    void
    outputMessage(const std::wstring& msg) override;
    void
    errorMessage(const std::string& msg) override;
    void
    errorMessage(const std::wstring& msg) override;
    void
    warningMessage(const std::string& msg) override;
    void
    warningMessage(const std::wstring& msg) override;

    void
    clearTerminal() override;
    bool
    isAtPrompt() override;
    void
    interruptGetLineByEvent() override;

private:
    NotebookKernel* m_kernel;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
