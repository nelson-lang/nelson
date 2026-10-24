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
#include <condition_variable>
#include <deque>
#include <functional>
#include <mutex>
#include <atomic>
#include "Interface.hpp"
#include "nlsWeb_gui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class WebGuiServer;
//=============================================================================
// Web frontend implementation of Nelson::Interface. Output is pushed to the
// xterm.js console over WebUI; input blocks in getLine() until the RPC layer
// enqueues a line via pushInputLine().
//=============================================================================
class NLSWEB_GUI_IMPEXP WebGuiTerminal : public Interface
{
public:
    WebGuiTerminal(WebGuiServer* server);
    ~WebGuiTerminal() override;

    std::string
    getLine(const std::string& prompt) override;
    std::wstring
    getLine(const std::wstring& prompt) override;
    std::wstring
    getInput(const std::wstring& prompt) override;

    // Called by the RPC dispatcher on `term.input`.
    void
    pushInputLine(const std::wstring& line);

    // Run a callable on the interpreter thread and block until it finishes.
    // Nelson's interpreter and its Qt graphics objects live on this thread; RPC
    // handlers run on the WebUI thread and must marshal any interpreter/graphics
    // access here. Tasks execute while the interpreter is idle at the prompt.
    // Returns false if the interpreter did not become idle within timeoutMs.
    // A timed-out task is cancelled and will never execute later.
    bool
    runOnInterpreterThread(const std::function<void()>& fn, int timeoutMs = 15000);

    // Prompt synchronization for browsers that connect after Nelson's first
    // getLine. resendPrompt() is for non-RPC paths; currentPrompt() is returned
    // as regular RPC data by `term.sync`.
    void
    resendPrompt();
    std::wstring
    currentPrompt();

    size_t
    getTerminalWidth() override;
    size_t
    getTerminalHeight() override;
    void
    setTerminalSize(size_t cols, size_t rows);

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
    bool
    outputStyledText(const std::wstring& msg, const Nelson::TextStyle& style) override;
    bool
    outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
        const std::wstring& plainFallback, const std::wstring& contextName = L"",
        long long contextValue = 0) override;
    // Large matrices are shown shortened with a link that reveals the full
    // display, like the Qt terminal (the browser console has the same
    // expandable-link mechanism).
    // inserthtml: xterm renders text only, so the markup goes to a dedicated
    // desktop panel instead of the console flow.
    bool
    insertHtml(const std::wstring& html) override;
    bool
    supportsTruncatedMatrixDisplay() override;
    bool
    outputTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
        const std::wstring& expandedText, const std::wstring& variableName) override;
    // The browser console parses OSC 8 hyperlinks, so error/warning locations are
    // emitted as clickable links that open the editor at the file and line.
    bool
    supportsHyperlinks() override;

    void
    clearTerminal() override;
    bool
    isAtPrompt() override;
    void
    interruptGetLineByEvent() override;

private:
    WebGuiServer* m_server;
    std::mutex m_mutex;
    std::condition_variable m_cv;
    std::deque<std::wstring> m_pending;
    std::atomic<bool> m_interrupt { false };
    std::atomic<bool> m_atPrompt { false };
    std::wstring m_currentPrompt;
    size_t m_cols { 80 };
    size_t m_rows { 25 };

    std::wstring
    waitForLine(const std::wstring& prompt);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
