//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DapServer.hpp"
#include "GuiServer.hpp"
#include "RpcReply.hpp"
#include "MainThreadTaskQueue.hpp"
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "RuntimeDispatch.hpp"
#include "Evaluator.hpp"
#include "DebuggerJson.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Only frame 0's locals are populated; a deeper frame's scope (dbup) is a
// follow-up. The scope reference is a fixed non-zero handle.
static constexpr long long LOCAL_SCOPE_REFERENCE = 1;
//=============================================================================
static Evaluator*
mainEvaluator()
{
    return static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
}
//=============================================================================
// Escapes a path for a single-quoted Nelson string (single quotes are doubled).
static std::wstring
quoteForNelson(const std::wstring& text)
{
    std::wstring quoted;
    quoted.reserve(text.size() + 2);
    for (wchar_t c : text) {
        if (c == L'\'') {
            quoted.push_back(L'\'');
        }
        quoted.push_back(c);
    }
    return quoted;
}
//=============================================================================
// Maps an engine stop reason to a DAP `stopped` reason.
static std::string
dapStopReason(const std::string& engineReason)
{
    if (engineReason == "breakpoint") {
        return "breakpoint";
    }
    if (engineReason == "step" || engineReason == "runToLine") {
        return "step";
    }
    if (engineReason == "error") {
        return "exception";
    }
    // pause, keyboard and anything else present as a pause to the client.
    return "pause";
}
//=============================================================================
DapServer::DapServer(std::unique_ptr<IFrameTransport> transport, DapMode mode)
    : m_transport(std::move(transport)), m_mode(mode)
{
}
//=============================================================================
DapServer::~DapServer() { stop(); }
//=============================================================================
void
DapServer::setServer(GuiServer* server)
{
    m_server = server;
}
//=============================================================================
void
DapServer::start()
{
    m_stop.store(false);
    // A posted cross-thread task or interpreter wake-up must wake the prompt loop,
    // like the web terminal, so queries and debugger commands are served while
    // paused. In attach mode the notebook kernel installs the hooks (it must also
    // wake its own cell loop), so the server does not touch them here.
    if (m_mode == DapMode::Standalone) {
        MainThreadTaskQueue::getInstance()->setWakeupHook([this]() { wake(); });
        setInterpreterWakeupHook([this]() { wake(); });
    }
    m_readerThread = std::thread([this]() { readerLoop(); });
}
//=============================================================================
void
DapServer::stop()
{
    m_stop.store(true);
    requestShutdown();
    if (m_readerThread.joinable()) {
        // The reader exits on disconnect or EOF; past that it is detached so a
        // blocking read at process teardown never hangs exit.
        m_readerThread.detach();
    }
}
//=============================================================================
void
DapServer::requestShutdown()
{
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_shutdown = true;
    }
    m_cv.notify_all();
}
//=============================================================================
void
DapServer::wake()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_cv.notify_all();
}
//=============================================================================
void
DapServer::handleClientDetach()
{
    // The debug client left a running notebook kernel. Clear the debug state so
    // later cells run without pausing (there is no client to drive a pause), and a
    // reattach starts clean.
    debuggerCall(
        "setErrorBreakpoints", nlohmann::json { { "error", false }, { "caughtError", false } });
    debuggerCall("removeAllBreakpoints", nlohmann::json { { "file", "" } });
    // If a cell is paused right now, resume it. This queues dbcont exactly like a
    // normal continue, so runBreakpointPrompt wakes and hands it to the interpreter.
    Evaluator* eval = mainEvaluator();
    if (eval != nullptr && eval->isBreakpointActive()) {
        debuggerCall("command", nlohmann::json { { "cmd", "continue" } });
    }
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_lastStoppedRevision = -1;
    }
    m_cv.notify_all();
}
//=============================================================================
bool
DapServer::isInterpreterIdle() const
{
    return MainThreadTaskQueue::getInstance()->isInterpreterIdle();
}
//=============================================================================
void
DapServer::sendResponse(const nlohmann::json& request, bool success, const nlohmann::json& body)
{
    nlohmann::json response = { { "seq", m_seq.fetch_add(1) }, { "type", "response" },
        { "request_seq", request.value("seq", 0) }, { "success", success },
        { "command", request.value("command", std::string()) } };
    if (!body.is_null()) {
        response["body"] = body;
    }
    m_transport->writeFrame(response.dump());
}
//=============================================================================
void
DapServer::sendErrorResponse(const nlohmann::json& request, const std::string& message)
{
    nlohmann::json response = { { "seq", m_seq.fetch_add(1) }, { "type", "response" },
        { "request_seq", request.value("seq", 0) }, { "success", false },
        { "command", request.value("command", std::string()) }, { "message", message } };
    m_transport->writeFrame(response.dump());
}
//=============================================================================
void
DapServer::sendEvent(const std::string& event, const nlohmann::json& body)
{
    nlohmann::json message
        = { { "seq", m_seq.fetch_add(1) }, { "type", "event" }, { "event", event } };
    if (!body.is_null()) {
        message["body"] = body;
    }
    m_transport->writeFrame(message.dump());
}
//=============================================================================
nlohmann::json
DapServer::debuggerCall(const std::string& method, const nlohmann::json& params)
{
    if (m_server == nullptr) {
        return { { "error", "debugger server not available" } };
    }
    const nlohmann::json envelope = { { "id", m_seq.fetch_add(1) },
        { "method", "debugger." + method }, { "params", params } };
    const std::string reply = m_server->handleRpc(envelope.dump(), 0);
    const RpcReply parsed = parseRpcReply(reply);
    if (parsed.kind == RpcReply::Kind::Malformed) {
        return { { "error", "invalid debugger reply" } };
    }
    if (parsed.kind == RpcReply::Kind::Error) {
        return { { "error", parsed.message.empty() ? "debugger error" : parsed.message } };
    }
    return parsed.result.is_null() ? nlohmann::json::object() : parsed.result;
}
//=============================================================================
void
DapServer::readerLoop()
{
    for (;;) {
        const std::string frame = m_transport->readFrame();
        if (frame.empty()) {
            // A reconnecting (TCP) transport reports an empty frame when its client
            // disconnects: resume any pause and wait for the next client instead of
            // ending the whole kernel. A dead listener (or stdio EOF) ends the loop.
            if (m_mode == DapMode::Attach && m_transport->isReconnecting() && m_transport->isAlive()
                && !m_stop.load()) {
                handleClientDetach();
                continue;
            }
            requestShutdown();
            break;
        }
        const nlohmann::json request = nlohmann::json::parse(frame, nullptr, false);
        if (request.is_discarded() || !request.is_object()) {
            continue;
        }
        dispatch(request);
        if (m_stop.load()) {
            break;
        }
    }
}
//=============================================================================
void
DapServer::dispatch(const nlohmann::json& request)
{
    const std::string command = request.value("command", std::string());
    const nlohmann::json arguments = request.value("arguments", nlohmann::json::object());

    if (command == "initialize") {
        nlohmann::json capabilities = { { "supportsConfigurationDoneRequest", true },
            { "supportsConditionalBreakpoints", true },
            { "supportsHitConditionalBreakpoints", true }, { "supportsEvaluateForHovers", true },
            { "supportsTerminateRequest", true },
            { "exceptionBreakpointFilters",
                nlohmann::json::array({ { { "filter", "error" }, { "label", "Uncaught errors" },
                                            { "default", false } },
                    { { "filter", "caughtError" }, { "label", "Caught errors" },
                        { "default", false } } }) } };
        sendResponse(request, true, capabilities);
        sendEvent("initialized", nlohmann::json::object());
        return;
    }
    if (command == "attach") {
        if (m_mode == DapMode::Attach) {
            // Debugging an already-running notebook kernel: nothing to launch, the
            // cells drive execution. Start the session from a clean debug state.
            {
                std::lock_guard<std::mutex> lock(m_mutex);
                m_lastStoppedRevision = -1;
            }
            sendResponse(request, true, nullptr);
            return;
        }
        sendErrorResponse(request, "attach is not supported for a standalone file session");
        return;
    }
    if (command == "launch") {
        if (m_mode == DapMode::Attach) {
            // The notebook kernel owns execution; a launch would start a second
            // program in the shared interpreter. Acknowledge without running.
            sendResponse(request, true, nullptr);
            return;
        }
        const std::wstring program = utf8_to_wstring(arguments.value("program", std::string()));
        const std::wstring cwd = utf8_to_wstring(arguments.value("cwd", std::string()));
        const bool stopOnEntry = arguments.value("stopOnEntry", false);
        std::wstring line;
        if (!cwd.empty()) {
            line = L"cd('" + quoteForNelson(cwd) + L"'); ";
        }
        line += L"run('" + quoteForNelson(program) + L"')";
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_programCode = line;
            m_hasProgram = !program.empty();
        }
        if (stopOnEntry && !program.empty()) {
            // Stop at the first statement: a breakpoint on line 1 is moved forward
            // to the first executable line by the engine.
            debuggerCall("setBreakpoint",
                nlohmann::json { { "file", wstring_to_utf8(program) }, { "line", 1 } });
        }
        sendResponse(request, true, nullptr);
        return;
    }
    if (command == "configurationDone") {
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_configurationDone = true;
        }
        m_cv.notify_all();
        sendResponse(request, true, nullptr);
        return;
    }
    if (command == "setBreakpoints") {
        const std::string file
            = arguments.value("source", nlohmann::json::object()).value("path", std::string());
        debuggerCall("removeAllBreakpoints", nlohmann::json { { "file", file } });
        nlohmann::json breakpoints = nlohmann::json::array();
        if (arguments.contains("breakpoints") && arguments["breakpoints"].is_array()) {
            for (const auto& bp : arguments["breakpoints"]) {
                nlohmann::json params = { { "file", file }, { "line", bp.value("line", 0) } };
                if (bp.contains("condition")) {
                    params["condition"] = bp.value("condition", std::string());
                }
                if (bp.contains("hitCondition")) {
                    params["condition"] = bp.value("hitCondition", std::string());
                }
                nlohmann::json result = debuggerCall("setBreakpoint", params);
                nlohmann::json entry = { { "verified", !result.contains("error") } };
                if (result.contains("line")) {
                    entry["line"] = result["line"];
                } else {
                    entry["line"] = bp.value("line", 0);
                }
                breakpoints.push_back(entry);
            }
        }
        sendResponse(request, true, nlohmann::json { { "breakpoints", breakpoints } });
        return;
    }
    if (command == "setExceptionBreakpoints") {
        bool error = false;
        bool caughtError = false;
        if (arguments.contains("filters") && arguments["filters"].is_array()) {
            for (const auto& filter : arguments["filters"]) {
                if (filter == "error") {
                    error = true;
                } else if (filter == "caughtError") {
                    caughtError = true;
                }
            }
        }
        debuggerCall("setErrorBreakpoints",
            nlohmann::json { { "error", error }, { "caughtError", caughtError } });
        sendResponse(request, true, nlohmann::json { { "breakpoints", nlohmann::json::array() } });
        return;
    }
    if (command == "threads") {
        sendResponse(request, true,
            nlohmann::json {
                { "threads", nlohmann::json::array({ { { "id", 1 }, { "name", "Nelson" } } }) } });
        return;
    }
    if (command == "stackTrace") {
        nlohmann::json state = debuggerCall("state", nlohmann::json::object());
        nlohmann::json frames = nlohmann::json::array();
        long long frameId = 0;
        if (state.contains("stack") && state["stack"].is_array()) {
            for (const auto& level : state["stack"]) {
                const std::string file = level.value("file", std::string());
                nlohmann::json frame = { { "id", frameId }, { "name", level.value("function", "") },
                    { "line", level.value("line", 0) }, { "column", 1 } };
                if (!file.empty()) {
                    frame["source"] = { { "name", file }, { "path", file } };
                }
                frames.push_back(frame);
                frameId++;
            }
        }
        sendResponse(request, true,
            nlohmann::json { { "stackFrames", frames },
                { "totalFrames", static_cast<long long>(frames.size()) } });
        return;
    }
    if (command == "scopes") {
        const long long frameId = arguments.value("frameId", 0);
        nlohmann::json scopes = nlohmann::json::array({ { { "name", "Local" },
            { "variablesReference", frameId == 0 ? LOCAL_SCOPE_REFERENCE : 0 },
            { "expensive", false } } });
        sendResponse(request, true, nlohmann::json { { "scopes", scopes } });
        return;
    }
    if (command == "variables") {
        const long long reference = arguments.value("variablesReference", 0);
        nlohmann::json variables = nlohmann::json::array();
        if (reference == LOCAL_SCOPE_REFERENCE) {
            nlohmann::json locals = debuggerCall("locals", nlohmann::json::object());
            if (locals.contains("variables") && locals["variables"].is_array()) {
                for (const auto& var : locals["variables"]) {
                    variables.push_back(
                        { { "name", var.value("name", "") }, { "value", var.value("value", "") },
                            { "type", var.value("type", "") }, { "variablesReference", 0 } });
                }
            }
        }
        sendResponse(request, true, nlohmann::json { { "variables", variables } });
        return;
    }
    if (command == "continue" || command == "next" || command == "stepIn" || command == "stepOut"
        || command == "pause") {
        std::string cmd = "continue";
        if (command == "next") {
            cmd = "step";
        } else if (command == "stepIn") {
            cmd = "stepIn";
        } else if (command == "stepOut") {
            cmd = "stepOut";
        } else if (command == "pause") {
            cmd = "pause";
        }
        nlohmann::json result = debuggerCall("command", nlohmann::json { { "cmd", cmd } });
        if (result.contains("error")) {
            sendErrorResponse(request, result.value("error", std::string("command failed")));
            return;
        }
        if (command == "continue") {
            sendResponse(request, true, nlohmann::json { { "allThreadsContinued", true } });
        } else {
            sendResponse(request, true, nullptr);
        }
        return;
    }
    if (command == "evaluate") {
        const std::string expr = arguments.value("expression", std::string());
        nlohmann::json result = debuggerCall("evaluate", nlohmann::json { { "expr", expr } });
        const std::string text = result.value("text", std::string());
        sendResponse(
            request, true, nlohmann::json { { "result", text }, { "variablesReference", 0 } });
        return;
    }
    if (command == "disconnect" || command == "terminate") {
        if (m_mode == DapMode::Attach) {
            // Detach from the notebook kernel: resume execution and clear the debug
            // state, but keep the kernel (and its figures) running.
            sendResponse(request, true, nullptr);
            handleClientDetach();
            return;
        }
        // Standalone: unwind an active pause, then end the reader loop.
        debuggerCall("command", nlohmann::json { { "cmd", "quit" } });
        sendResponse(request, true, nullptr);
        requestShutdown();
        m_stop.store(true);
        return;
    }
    // Unknown request: acknowledge so the client is not left waiting.
    sendResponse(request, true, nullptr);
}
//=============================================================================
void
DapServer::emitStoppedIfNew(Evaluator* eval)
{
    const nlohmann::json state = nlohmann::json::parse(debuggerStateJson(eval), nullptr, false);
    if (state.is_discarded() || !state.value("paused", false)) {
        return;
    }
    const long long revision = state.value("revision", 0);
    if (revision == m_lastStoppedRevision) {
        return;
    }
    m_lastStoppedRevision = revision;
    const std::string reason = dapStopReason(state.value("reason", std::string("pause")));
    nlohmann::json body
        = { { "reason", reason }, { "threadId", 1 }, { "allThreadsStopped", true } };
    const std::string errorMessage = state.value("errorMessage", std::string());
    if (reason == "exception" && !errorMessage.empty()) {
        body["description"] = errorMessage;
        body["text"] = errorMessage;
    }
    sendEvent("stopped", body);
}
//=============================================================================
std::wstring
DapServer::runBreakpointPrompt(Evaluator* eval)
{
    auto* taskQueue = MainThreadTaskQueue::getInstance();
    // Paused at a breakpoint (K>> prompt): announce the stop, serve queries, and
    // wait for the next debugger command (dbstep / dbcont). Shared by the
    // standalone session (driven by nextLine) and the notebook kernel (which calls
    // it directly when a cell pauses).
    for (;;) {
        emitStoppedIfNew(eval);
        std::unique_lock<std::mutex> lock(m_mutex);
        taskQueue->setInterpreterIdle(true);
        m_cv.wait(lock, [this, eval, taskQueue]() {
            return m_shutdown || !taskQueue->isEmpty()
                || (eval != nullptr && !eval->commandQueue.isEmpty());
        });
        taskQueue->setInterpreterIdle(false);
        if (m_shutdown) {
            // Standalone aborts the stack; attach resumes the cell so the kernel runs on.
            return m_mode == DapMode::Attach ? L"dbcont" : L"dbquit";
        }
        lock.unlock();
        if (!taskQueue->isEmpty()) {
            // stackTrace / variables / evaluate marshalled from the reader thread.
            taskQueue->processTasksAtPrompt(eval);
            continue;
        }
        std::wstring queued;
        if (eval != nullptr && eval->commandQueue.get(queued)) {
            sendEvent(
                "continued", nlohmann::json { { "threadId", 1 }, { "allThreadsContinued", true } });
            m_lastStoppedRevision = -1;
            return queued;
        }
    }
}
//=============================================================================
std::wstring
DapServer::nextLine(Evaluator* eval)
{
    auto* taskQueue = MainThreadTaskQueue::getInstance();
    for (;;) {
        if (eval != nullptr && eval->isBreakpointActive()) {
            return runBreakpointPrompt(eval);
        }

        // Top-level prompt: hand out the program, report its end, or idle.
        std::unique_lock<std::mutex> lock(m_mutex);
        if (m_programStarted && !m_programTerminated) {
            // The launched program returned to the top level: the session is over.
            m_programTerminated = true;
            lock.unlock();
            sendEvent("thread", nlohmann::json { { "reason", "exited" }, { "threadId", 1 } });
            sendEvent("terminated", nlohmann::json::object());
            sendEvent("exited", nlohmann::json { { "exitCode", 0 } });
            lock.lock();
        }
        if (m_hasProgram && m_configurationDone && !m_programStarted) {
            m_programStarted = true;
            m_lastStoppedRevision = -1;
            std::wstring code = m_programCode;
            lock.unlock();
            sendEvent("thread", nlohmann::json { { "reason", "started" }, { "threadId", 1 } });
            return code;
        }
        taskQueue->setInterpreterIdle(true);
        m_cv.wait(lock, [this, taskQueue]() {
            return m_shutdown || !taskQueue->isEmpty()
                || (m_hasProgram && m_configurationDone && !m_programStarted);
        });
        taskQueue->setInterpreterIdle(false);
        if (m_shutdown) {
            return L"exit";
        }
        lock.unlock();
        if (!taskQueue->isEmpty()) {
            taskQueue->processTasksAtPrompt(eval);
            continue;
        }
    }
}
//=============================================================================
void
DapServer::emitOutput(const std::string& category, const std::wstring& text)
{
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (!m_programStarted || m_programTerminated) {
            return;
        }
    }
    sendEvent(
        "output", nlohmann::json { { "category", category }, { "output", wstring_to_utf8(text) } });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
