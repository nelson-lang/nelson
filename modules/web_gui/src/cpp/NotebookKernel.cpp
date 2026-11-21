//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NotebookKernel.hpp"
#include "WebGuiServer.hpp"
#include "MainThreadTaskQueue.hpp"
#include "characters_encoding.hpp"
#include "Nelson_VERSION.h"
#include <cstdio>
#include <iostream>
#include <string>
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// The interpreter formats a runtime error for a console as "\nError:\n<msg>\n\n".
// The structured execute response wants just the message, so strip that wrapper.
static std::wstring
cleanErrorMessage(const std::wstring& message)
{
    auto trim = [](std::wstring s) {
        const wchar_t* ws = L" \t\r\n";
        const size_t b = s.find_first_not_of(ws);
        if (b == std::wstring::npos) {
            return std::wstring();
        }
        const size_t e = s.find_last_not_of(ws);
        return s.substr(b, e - b + 1);
    };
    std::wstring result = trim(message);
    const std::wstring prefix = L"Error:";
    if (result.rfind(prefix, 0) == 0) {
        result = trim(result.substr(prefix.size()));
    }
    return result;
}
//=============================================================================
NotebookKernel::NotebookKernel() = default;
//=============================================================================
NotebookKernel::~NotebookKernel() { stop(); }
//=============================================================================
void
NotebookKernel::setServer(WebGuiServer* server)
{
    m_server = server;
}
//=============================================================================
void
NotebookKernel::start()
{
#ifdef _WIN32
    // Binary stdio so Content-Length byte counts match and \r\n is never doubled
    // (same requirement as the LSP server).
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
#endif
    m_stop.store(false);
    m_readerThread = std::thread([this]() { readerLoop(); });
}
//=============================================================================
void
NotebookKernel::stop()
{
    m_stop.store(true);
    requestShutdown();
    if (m_readerThread.joinable()) {
        // The reader exits on kernel.shutdown or stdin EOF; past that the thread is
        // detached so a blocking stdin read at process teardown never hangs exit.
        m_readerThread.detach();
    }
}
//=============================================================================
void
NotebookKernel::requestShutdown()
{
    {
        std::lock_guard<std::mutex> lock(m_cellMutex);
        m_shutdown = true;
    }
    m_cellCv.notify_all();
}
//=============================================================================
std::wstring
NotebookKernel::nextCellCode()
{
    // The interpreter has finished the previous cell: send its execute response.
    if (m_hasRunningCell) {
        nlohmann::json result;
        if (m_runningCellError) {
            result = { { "status", "error" }, { "ename", "Error" },
                { "evalue", wstring_to_utf8(m_runningCellErrorText) } };
        } else {
            result = { { "status", "ok" } };
        }
        writeResult(m_runningCellId, result);
        m_hasRunningCell = false;
    }
    auto* taskQueue = MainThreadTaskQueue::getInstance();
    Cell cell;
    {
        std::unique_lock<std::mutex> lock(m_cellMutex);
        // Idle while waiting: figure RPC from the reader thread runs in place.
        taskQueue->setInterpreterIdle(true);
        m_cellCv.wait(lock, [this]() { return m_shutdown || !m_cells.empty(); });
        if (m_cells.empty()) {
            taskQueue->setInterpreterIdle(false);
            return L"exit";
        }
        cell = std::move(m_cells.front());
        m_cells.pop_front();
    }
    taskQueue->setInterpreterIdle(false);
    m_hasRunningCell = true;
    m_runningCellId = cell.id;
    m_runningCellError = false;
    m_runningCellErrorText.clear();
    return cell.code;
}
//=============================================================================
void
NotebookKernel::emitOutput(const std::string& stream, const std::wstring& text)
{
    // Output outside a cell run (startup banner, prompts) is dropped: the raw
    // stdout carries frames only.
    if (!m_hasRunningCell) {
        return;
    }
    writeNotification("notebook.output",
        nlohmann::json { { "stream", stream }, { "text", wstring_to_utf8(text) } });
}
//=============================================================================
void
NotebookKernel::markCellError(const std::wstring& message)
{
    if (!m_hasRunningCell) {
        return;
    }
    m_runningCellError = true;
    // Keep the message the execute response carries clean; the stderr output
    // notification still streams the full formatted text separately.
    m_runningCellErrorText = cleanErrorMessage(message);
}
//=============================================================================
bool
NotebookKernel::isInterpreterIdle() const
{
    return MainThreadTaskQueue::getInstance()->isInterpreterIdle();
}
//=============================================================================
void
NotebookKernel::wake()
{
    m_cellCv.notify_all();
}
//=============================================================================
void
NotebookKernel::writePush(const std::string& channel, const std::string& payloadJson)
{
    // A push is a JSON-RPC notification whose method is the channel name and whose
    // params are the already serialized payload object.
    nlohmann::json params = nlohmann::json::parse(payloadJson, nullptr, false);
    if (params.is_discarded()) {
        params = nlohmann::json::object();
    }
    writeNotification(channel, params);
}
//=============================================================================
std::string
NotebookKernel::readFrame()
{
    std::string line;
    size_t contentLength = 0;
    while (std::getline(std::cin, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        if (line.empty()) {
            break;
        }
        const std::string prefix = "Content-Length:";
        if (line.rfind(prefix, 0) == 0) {
            try {
                contentLength = static_cast<size_t>(std::stoul(line.substr(prefix.size())));
            } catch (...) {
                contentLength = 0;
            }
        }
    }
    if (contentLength == 0) {
        return {};
    }
    std::string body(contentLength, '\0');
    std::cin.read(&body[0], static_cast<std::streamsize>(contentLength));
    if (!std::cin) {
        return {};
    }
    return body;
}
//=============================================================================
void
NotebookKernel::writeFrame(const std::string& body)
{
    std::lock_guard<std::mutex> lock(m_writeMutex);
    const std::string header = "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n";
    std::fwrite(header.data(), 1, header.size(), stdout);
    std::fwrite(body.data(), 1, body.size(), stdout);
    std::fflush(stdout);
}
//=============================================================================
void
NotebookKernel::writeResult(const nlohmann::json& id, const nlohmann::json& result)
{
    writeFrame(nlohmann::json { { "jsonrpc", "2.0" }, { "id", id }, { "result", result } }.dump());
}
//=============================================================================
void
NotebookKernel::writeError(const nlohmann::json& id, int code, const std::string& message)
{
    writeFrame(nlohmann::json { { "jsonrpc", "2.0" }, { "id", id },
        { "error", { { "code", code }, { "message", message } } } }
                   .dump());
}
//=============================================================================
void
NotebookKernel::writeNotification(const std::string& method, const nlohmann::json& params)
{
    writeFrame(
        nlohmann::json { { "jsonrpc", "2.0" }, { "method", method }, { "params", params } }.dump());
}
//=============================================================================
void
NotebookKernel::handleFigureMethod(
    const nlohmann::json& id, const std::string& method, const nlohmann::json& params)
{
    if (m_server == nullptr) {
        writeError(id, -32603, "figure server not available");
        return;
    }
    // Reuse WebGuiRpc::dispatch via the server: build its {id,method,params}
    // envelope, then re-wrap the {id,result|error:{message}} reply as JSON-RPC 2.0.
    const nlohmann::json envelope
        = { { "id", id }, { "method", method }, { "params", params } };
    const std::string reply = m_server->handleRpc(envelope.dump(), 0);
    const nlohmann::json parsed = nlohmann::json::parse(reply, nullptr, false);
    if (parsed.is_discarded()) {
        writeError(id, -32603, "malformed figure dispatch reply");
        return;
    }
    if (parsed.contains("error")) {
        std::string message = "figure RPC error";
        if (parsed["error"].is_object() && parsed["error"].contains("message")) {
            message = parsed["error"].value("message", message);
        }
        writeError(id, -32000, message);
        return;
    }
    writeResult(id, parsed.value("result", nlohmann::json(nullptr)));
}
//=============================================================================
void
NotebookKernel::readerLoop()
{
    while (!m_stop.load()) {
        const std::string frame = readFrame();
        if (frame.empty()) {
            // stdin closed: ask the interpreter loop to quit.
            requestShutdown();
            break;
        }
        const nlohmann::json msg = nlohmann::json::parse(frame, nullptr, false);
        if (msg.is_discarded()) {
            writeError(nlohmann::json(nullptr), -32700, "parse error");
            continue;
        }
        const nlohmann::json id = msg.value("id", nlohmann::json(nullptr));
        const std::string method = msg.value("method", std::string());
        const nlohmann::json params = msg.value("params", nlohmann::json::object());
        if (method == "kernel.initialize") {
            writeResult(id,
                nlohmann::json { { "protocolVersion", 1 },
                    { "engineVersion", NELSON_SEMANTIC_VERSION_STRING },
                    { "capabilities",
                        { { "subscribe", false }, { "output", true }, { "figures", true } } } });
        } else if (method == "kernel.shutdown") {
            writeResult(id, nlohmann::json { { "ok", true } });
            requestShutdown();
            break;
        } else if (method == "notebook.execute") {
            const std::wstring code = utf8_to_wstring(params.value("code", std::string()));
            {
                std::lock_guard<std::mutex> lock(m_cellMutex);
                m_cells.push_back(Cell { id, code });
            }
            m_cellCv.notify_all();
        } else if (method.rfind("figure.", 0) == 0 || method.rfind("uicontrol.", 0) == 0) {
            handleFigureMethod(id, method, params);
        } else {
            writeError(id, -32601, "method not found: " + method);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
