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
#include "nlsWeb_gui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class DapServer;
//=============================================================================
// Nelson::Interface for the Debug Adapter server. getLine() asks the server for
// the next line to run (the launched program, or a debugger command while paused
// at a breakpoint); output is forwarded as DAP `output` events. There is no
// interactive input() over the frame channel, so the base default is used. The
// shared frame-terminal boilerplate lives in FrameTerminalBase.
class NLSWEB_GUI_IMPEXP DapTerminal : public FrameTerminalBase
{
public:
    explicit DapTerminal(DapServer* server);
    ~DapTerminal() override;

protected:
    std::wstring
    readLine() override;
    void
    emitText(const std::string& stream, const std::wstring& msg) override;
    bool
    interpreterIdle() override;
    void
    wakeInterpreter() override;

private:
    DapServer* m_server;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
