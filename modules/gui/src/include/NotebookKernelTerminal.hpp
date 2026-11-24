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
#include "FrameTerminalBase.hpp"
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NotebookKernel;
//=============================================================================
// Nelson::Interface for the notebook kernel. getLine() blocks until the kernel's
// stdin reader hands it the next cell's code; output is forwarded to the kernel as
// notebook.output notifications, and an error line also marks the running cell
// failed. The shared frame-terminal boilerplate lives in FrameTerminalBase.
class NLSGUI_IMPEXP NotebookKernelTerminal : public FrameTerminalBase
{
public:
    explicit NotebookKernelTerminal(NotebookKernel* kernel);
    ~NotebookKernelTerminal() override;

protected:
    std::wstring
    readLine() override;
    void
    emitText(const std::string& stream, const std::wstring& msg) override;
    std::wstring
    readInput(const std::wstring& prompt) override;
    void
    onErrorLine(const std::wstring& msg) override;
    bool
    interpreterIdle() override;
    void
    wakeInterpreter() override;

private:
    NotebookKernel* m_kernel;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
