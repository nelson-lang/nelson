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
class DapServer;
//=============================================================================
// Nelson::Interface for the Debug Adapter server. getLine() asks the server for
// the next line to run (the launched program, or a debugger command while paused
// at a breakpoint); the output methods are forwarded as DAP `output` events,
// never written to the raw stdout that carries the DAP frames.
class NLSWEB_GUI_IMPEXP DapTerminal : public Interface
{
public:
    explicit DapTerminal(DapServer* server);
    ~DapTerminal() override;

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
    DapServer* m_server;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
