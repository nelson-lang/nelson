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
#include <deque>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <nlohmann/json.hpp>
#include "nlsWeb_gui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class WebGuiServer;
class DapServer;
class NotebookKernelNFlow;
//=============================================================================
// Coordinates the notebook kernel's stdio JSON-RPC channel: a reader thread
// parses Content-Length frames from stdin, cell code is handed to the
// interpreter through the terminal, figure RPC is dispatched by WebGuiServer, and
// responses, captured output and figure pushes are written back as frames. One
// Nelson process, one channel, cell execution and live figures together.
class NLSWEB_GUI_IMPEXP NotebookKernel
{
public:
    NotebookKernel();
    ~NotebookKernel();
    NotebookKernel(const NotebookKernel&) = delete;
    NotebookKernel&
    operator=(const NotebookKernel&)
        = delete;

    // The server that dispatches figure.* methods and whose pushes this kernel
    // forwards as notifications. Set before start().
    void
    setServer(WebGuiServer* server);

    // Optional Debug Adapter server sharing this kernel's interpreter. When set,
    // a cell that pauses at a breakpoint routes its K>> prompt here so an editor
    // can step through it. Null when no --dap-port was given.
    void
    setDapServer(DapServer* dap);

    // Sets stdin/stdout to binary and starts the reader thread.
    void
    start();
    void
    stop();

    // Terminal-facing (called on the interpreter thread):
    // Blocks until the next cell arrives, after first sending the previous cell's
    // execute response. Returns the cell code, or a request to quit on shutdown.
    std::wstring
    nextCellCode();
    // Captures a line of cell output as a notebook.output notification.
    void
    emitOutput(const std::string& stream, const std::wstring& text);
    // Marks the running cell as failed and records the message for its response.
    void
    markCellError(const std::wstring& message);
    bool
    isInterpreterIdle() const;
    // Wakes nextCellCode without delivering a cell (used by interruptGetLineByEvent).
    void
    wake();

    // Push sink target: writes a figure push as a JSON-RPC notification.
    void
    writePush(const std::string& channel, const std::string& payloadJson);

private:
    void
    readerLoop();
    // Reads one Content-Length framed message from stdin; empty on EOF.
    std::string
    readFrame();
    // Writes one Content-Length framed message to stdout (serialized).
    void
    writeFrame(const std::string& body);
    void
    writeResult(const nlohmann::json& id, const nlohmann::json& result);
    void
    writeError(const nlohmann::json& id, int code, const std::string& message);
    void
    writeNotification(const std::string& method, const nlohmann::json& params);
    void
    handleFigureMethod(
        const nlohmann::json& id, const std::string& method, const nlohmann::json& params);
    void
    requestShutdown();

    WebGuiServer* m_server { nullptr };
    DapServer* m_dap { nullptr };
    std::thread m_readerThread;
    std::atomic<bool> m_stop { false };

    // Serializes every write to stdout so frames never interleave.
    std::mutex m_writeMutex;

    // Cell queue fed by the reader, drained by nextCellCode.
    struct Cell
    {
        nlohmann::json id;
        std::wstring code;
    };
    std::mutex m_cellMutex;
    std::condition_variable m_cellCv;
    std::deque<Cell> m_cells;
    bool m_shutdown { false };
    // The cell currently handed to the interpreter, and whether it errored.
    bool m_hasRunningCell { false };
    nlohmann::json m_runningCellId;
    bool m_runningCellError { false };
    std::wstring m_runningCellErrorText;

    // Optional nflow.* routing; null until start(), inert if the build has no nflow.
    std::unique_ptr<NotebookKernelNFlow> m_nflow;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
