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
// Shared Nelson::Interface boilerplate for the frame-channel terminals (the
// notebook kernel and the Debug Adapter server). Both forward every Interface
// call to their owner; only a few operations differ, so a derived class provides
// those as hooks and this base implements the rest: the string/wstring overloads,
// the fixed terminal size, the empty clear, and the idle / wake forwarding. Output
// is never written to the raw stdout that carries the JSON-RPC / DAP frames.
class NLSWEB_GUI_IMPEXP FrameTerminalBase : public Interface
{
public:
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

protected:
    // The next line to run (a notebook cell, the launched program, a debugger
    // command). getLine() blocks on it.
    virtual std::wstring
    readLine()
        = 0;
    // Writes one captured output line to the owner, tagged "stdout" or "stderr".
    virtual void
    emitText(const std::string& stream, const std::wstring& msg)
        = 0;
    // Interactive input(): the default has none (DAP); the notebook kernel asks
    // the client over its frame channel.
    virtual std::wstring
    readInput(const std::wstring& prompt);
    // Extra handling for an error line (the notebook kernel marks its cell failed);
    // the base still emits the text to "stderr" afterwards.
    virtual void
    onErrorLine(const std::wstring& msg);
    // Whether the interpreter is idle at the prompt, and a wake for the reader.
    virtual bool
    interpreterIdle()
        = 0;
    virtual void
    wakeInterpreter()
        = 0;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
