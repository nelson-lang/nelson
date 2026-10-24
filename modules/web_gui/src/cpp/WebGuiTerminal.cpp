//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiTerminal.hpp"
#include "AnsiSgr.hpp"
#include "WebGuiServer.hpp"
#include "characters_encoding.hpp"
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "MainThreadTaskQueue.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include "NelsonHistory.hpp"
#include <nlohmann/json.hpp>
#include <chrono>
//=============================================================================
namespace Nelson {
//=============================================================================
WebGuiTerminal::WebGuiTerminal(WebGuiServer* server) : m_server(server)
{
    if (m_server != nullptr) {
        m_server->setTerminal(this);
    }
    // Wake waitForLine when a cross-thread task is posted, without raising
    // m_interrupt. An interrupt is handled as a synthetic newline so it can
    // never be mistaken for end-of-input by the REPL.
    MainThreadTaskQueue::getInstance()->setWakeupHook([this]() { m_cv.notify_one(); });
}
//=============================================================================
WebGuiTerminal::~WebGuiTerminal() { MainThreadTaskQueue::getInstance()->setWakeupHook(nullptr); }
//=============================================================================
std::wstring
WebGuiTerminal::waitForLine(const std::wstring& prompt)
{
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_currentPrompt = prompt;
    }
    // Reaching a prompt means the engine finished initialising: open the RPC
    // gate so the frontend's queries can be served.
    m_server->setEngineReady();
    m_server->pushJson("term.prompt", nlohmann::json { { "prompt", wstring_to_utf8(prompt) } });
    std::unique_lock<std::mutex> lock(m_mutex);
    m_atPrompt = true;
    for (;;) {
        m_cv.wait(lock, [this] {
            return !m_pending.empty() || m_interrupt.load()
                || !MainThreadTaskQueue::getInstance()->isEmpty()
                || !CallbackQueue::getInstance()->isEmpty()
                || !EventQueue::getInstance()->isEmpty();
        });

        // Serve cross-thread interpreter tasks (RPC handlers marshalled via
        // runOnInterpreterThread) here, at the idle prompt. Returning a synthetic newline
        // instead would churn the REPL on every periodic RPC (the figure.list reconcile),
        // and its term.prompt would trigger another figure.list: a self-sustaining storm.
        if (!MainThreadTaskQueue::getInstance()->isEmpty()) {
            auto* eval
                = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
            if (eval != nullptr) {
                lock.unlock();
                MainThreadTaskQueue::getInstance()->processTasks(eval);
                lock.lock();
            }
            continue;
        }

        if (m_interrupt.exchange(false)) {
            m_atPrompt = false;
            return std::wstring(1, L'\n');
        }
        // Graphics callbacks (a uicontrol click) were enqueued: return a blank command so
        // the CLI REPL loops back to Evaluator::processCliCallbacks and drains the
        // CallbackQueue, as adv-cli does.
        if (!CallbackQueue::getInstance()->isEmpty() || !EventQueue::getInstance()->isEmpty()) {
            m_atPrompt = false;
            return std::wstring(1, L'\n');
        }
        if (!m_pending.empty()) {
            std::wstring line = m_pending.front();
            m_pending.pop_front();
            m_atPrompt = false;
            return line;
        }
    }
}
//=============================================================================
std::wstring
WebGuiTerminal::getLine(const std::wstring& prompt)
{
    std::wstring line = waitForLine(prompt);
    // Record the command in the history manager, as the CLI and Qt terminals do,
    // skipping interrupts (empty) and the synthetic "\n". history.changed is pushed now
    // so the History panel shows the command while it runs.
    if (!line.empty() && line != L"\n") {
        Nelson::History::addLine(line);
        m_server->pushJson("history.changed", nlohmann::json::object());
    }
    return line;
}
//=============================================================================
std::string
WebGuiTerminal::getLine(const std::string& prompt)
{
    return wstring_to_utf8(waitForLine(utf8_to_wstring(prompt)));
}
//=============================================================================
std::wstring
WebGuiTerminal::getInput(const std::wstring& prompt)
{
    return waitForLine(prompt);
}
//=============================================================================
void
WebGuiTerminal::pushInputLine(const std::wstring& line)
{
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_pending.push_back(line);
    }
    m_cv.notify_one();
}
//=============================================================================
bool
WebGuiTerminal::runOnInterpreterThread(const std::function<void()>& fn, int timeoutMs)
{
    // Use the shared interpreter-thread task queue. It is drained between the
    // statements of a running script and inside pause()/drawnow()/timer waits, not
    // only at the prompt. RPC handlers then reach the interpreter during a command,
    // so a non-interactive launch such as `-e "run(demo)"`, which never reaches the CLI
    // prompt, still serves figure.list, figure.undock and workspace.list. The wakeup
    // hook set in the constructor wakes waitForLine at the prompt.
    return MainThreadTaskQueue::getInstance()->runOnInterpreterThread(
        [fn](Evaluator*) { fn(); }, timeoutMs);
}
//=============================================================================
void
WebGuiTerminal::resendPrompt()
{
    std::wstring prompt;
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (!m_atPrompt.load()) {
            return;
        }
        prompt = m_currentPrompt;
    }
    m_server->pushJson("term.prompt", nlohmann::json { { "prompt", wstring_to_utf8(prompt) } });
}
//=============================================================================
std::wstring
WebGuiTerminal::currentPrompt()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_atPrompt.load() ? m_currentPrompt : std::wstring();
}
//=============================================================================
size_t
WebGuiTerminal::getTerminalWidth()
{
    return m_cols;
}
//=============================================================================
size_t
WebGuiTerminal::getTerminalHeight()
{
    return m_rows;
}
//=============================================================================
void
WebGuiTerminal::setTerminalSize(size_t cols, size_t rows)
{
    if (cols > 0) {
        m_cols = cols;
    }
    if (rows > 0) {
        m_rows = rows;
    }
}
//=============================================================================
void
WebGuiTerminal::outputMessage(const std::wstring& msg)
{
    m_server->pushJson("term.output", nlohmann::json { { "text", wstring_to_utf8(msg) } });
}
//=============================================================================
void
WebGuiTerminal::outputMessage(const std::string& msg)
{
    m_server->pushJson("term.output", nlohmann::json { { "text", msg } });
}
//=============================================================================
void
WebGuiTerminal::errorMessage(const std::wstring& msg)
{
    m_server->pushJson("term.output",
        nlohmann::json { { "text", "\x1b[31m" + wstring_to_utf8(msg) + "\x1b[0m\r\n" } });
}
//=============================================================================
void
WebGuiTerminal::errorMessage(const std::string& msg)
{
    errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
WebGuiTerminal::warningMessage(const std::wstring& msg)
{
    m_server->pushJson("term.output",
        nlohmann::json { { "text", "\x1b[33m" + wstring_to_utf8(msg) + "\x1b[0m\r\n" } });
}
//=============================================================================
void
WebGuiTerminal::warningMessage(const std::string& msg)
{
    warningMessage(utf8_to_wstring(msg));
}
//=============================================================================
// Map a TextStyle (RGB, bold/italic/underline, e.g. from cprintf) to ANSI SGR escapes.
// xterm supports 24-bit truecolor (38;2;r;g;b foreground, 48;2;r;g;b background).
bool
WebGuiTerminal::outputStyledText(const std::wstring& msg, const Nelson::TextStyle& style)
{
    if (style.isPlain()) {
        outputMessage(msg);
        return true;
    }
    m_server->pushJson("term.output",
        nlohmann::json { { "text", buildAnsiStyledMessage(wstring_to_utf8(msg), style) } });
    return true;
}
//=============================================================================
// Emit the label as an OSC 8 hyperlink so xterm renders it clickable; the frontend
// reveals the expanded text on click. A static counter keys each expandable: a data
// member would change the object size (ABI).
bool
WebGuiTerminal::outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
    const std::wstring& plainFallback, const std::wstring& contextName, long long contextValue)
{
    (void)plainFallback;
    (void)contextName;
    (void)contextValue;
    static long long s_expandId = 0;
    const long long id = ++s_expandId;
    m_server->pushJson("term.expandable",
        nlohmann::json { { "id", id }, { "text", wstring_to_utf8(expandedText) } });
    // Inline OSC 8 hyperlink: OSC 8 ; ; <uri> ST <label> OSC 8 ; ; ST, where the
    // string terminator ST is ESC followed by backslash.
    const std::string osc = buildExpandableTextLink(id, wstring_to_utf8(label)) + "\r\n";
    m_server->pushJson("term.output", nlohmann::json { { "text", osc } });
    return true;
}
//=============================================================================
bool
WebGuiTerminal::insertHtml(const std::wstring& html)
{
    m_server->pushJson("html.show", nlohmann::json { { "html", wstring_to_utf8(html) } });
    return true;
}
//=============================================================================
bool
WebGuiTerminal::supportsTruncatedMatrixDisplay()
{
    return true;
}
//=============================================================================
bool
WebGuiTerminal::supportsHyperlinks()
{
    return true;
}
//=============================================================================
// Preview of a large matrix, then the "Display all N rows and M columns" label as a
// link revealing the full text, handled like the expandable texts above.
bool
WebGuiTerminal::outputTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
    const std::wstring& expandedText, const std::wstring& variableName)
{
    outputMessage(truncatedText);
    return outputExpandableText(label, expandedText, expandedText, variableName, 0);
}
//=============================================================================
void
WebGuiTerminal::clearTerminal()
{
    m_server->pushJson("term.clear", nlohmann::json::object());
}
//=============================================================================
bool
WebGuiTerminal::isAtPrompt()
{
    return m_atPrompt.load();
}
//=============================================================================
void
WebGuiTerminal::interruptGetLineByEvent()
{
    m_interrupt = true;
    m_cv.notify_one();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
