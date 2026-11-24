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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <nlohmann/json.hpp>
#include "DapTransport.hpp"
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GuiServer;
class Evaluator;
//=============================================================================
// Standalone: the DAP server owns the session and drives the launched program.
// Attach: the server shares a running interpreter (the notebook kernel) and only
// debugs it; the kernel drives execution and routes the breakpoint prompt here.
enum class DapMode
{
    Standalone,
    Attach
};
//=============================================================================
// Debug Adapter Protocol server for a standalone `.m` session. A reader thread
// parses DAP requests from the transport; each is translated to the engine's
// existing debugger RPC (debugger.* through GuiServer, so interpreter-thread
// marshalling and the pause fast path are reused) or to running the launched
// program. The interpreter runs on its own thread through DapTerminal, which
// calls nextLine(): at a breakpoint it emits the DAP `stopped` event and drains
// the command queue (dbstep / dbcont), exactly as the web terminal does.
//
// The DAP logic is transport-agnostic: a stdio transport serves the standalone
// case today; a TCP transport will serve a notebook session later without any
// change here.
class NLSGUI_IMPEXP DapServer
{
public:
    explicit DapServer(
        std::unique_ptr<IFrameTransport> transport, DapMode mode = DapMode::Standalone);
    ~DapServer();
    DapServer(const DapServer&) = delete;
    DapServer&
    operator=(const DapServer&)
        = delete;

    // The headless figure/RPC server that dispatches debugger.* on the
    // interpreter thread. Set before start().
    void
    setServer(GuiServer* server);

    // Starts the reader thread.
    void
    start();
    void
    stop();

    // Terminal-facing (called on the interpreter thread by DapTerminal):
    // Returns the next line the REPL should run: the launched program, a debugger
    // command (dbstep / dbcont / dbquit) while paused, or a quit request on
    // disconnect. Emits the `stopped` event when a breakpoint is reached.
    std::wstring
    nextLine(Evaluator* eval);
    // Attach mode (notebook kernel): the interpreter has paused at a breakpoint
    // inside a cell. Announces the `stopped` event, serves queries, and blocks
    // until the next debugger command (dbstep / dbcont), which it returns for the
    // interpreter to run. Returns dbcont if the client detaches while paused.
    std::wstring
    runBreakpointPrompt(Evaluator* eval);
    // Forwards interpreter output as a DAP `output` event while a session is live.
    void
    emitOutput(const std::string& category, const std::wstring& text);
    bool
    isInterpreterIdle() const;
    void
    wake();

private:
    void
    readerLoop();
    void
    dispatch(const nlohmann::json& request);

    // DAP wire writers (all serialized through the transport).
    void
    sendResponse(const nlohmann::json& request, bool success, const nlohmann::json& body);
    void
    sendErrorResponse(const nlohmann::json& request, const std::string& message);
    void
    sendEvent(const std::string& event, const nlohmann::json& body);

    // Runs a debugger.* RPC on the interpreter thread via the server; returns its
    // result object, or {"error": ...} on failure.
    nlohmann::json
    debuggerCall(const std::string& method, const nlohmann::json& params);

    // Emits the `stopped` event once per distinct pause (keyed by the debug state
    // revision). Called from nextLine on the interpreter thread.
    void
    emitStoppedIfNew(Evaluator* eval);

    void
    requestShutdown();

    // Attach mode: the TCP client left (disconnect request or socket close).
    // Resumes any active pause and clears the debug state so the kernel runs on
    // and a later reattach starts clean, without stopping the process.
    void
    handleClientDetach();

    GuiServer* m_server { nullptr };
    std::unique_ptr<IFrameTransport> m_transport;
    const DapMode m_mode;
    std::thread m_readerThread;
    std::atomic<bool> m_stop { false };

    std::atomic<long long> m_seq { 1 };

    // Launch / lifecycle state, guarded by m_mutex.
    std::mutex m_mutex;
    std::condition_variable m_cv;
    bool m_shutdown { false };
    std::wstring m_programCode;
    bool m_hasProgram { false };
    bool m_configurationDone { false };
    bool m_programStarted { false };
    bool m_programTerminated { false };

    // Last pause revision for which a `stopped` event was emitted.
    long long m_lastStoppedRevision { -1 };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
