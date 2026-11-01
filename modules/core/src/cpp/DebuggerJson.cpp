//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <nlohmann/json.hpp>
#include "DebuggerJson.hpp"
#include "DebugStack.hpp"
#include "DebugValuePreview.hpp"
#include "EditorBreakpoints.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const char*
stopReasonName(DebugStopReason reason)
{
    switch (reason) {
    case DebugStopReason::Breakpoint:
        return "breakpoint";
    case DebugStopReason::Step:
        return "step";
    case DebugStopReason::Keyboard:
        return "keyboard";
    case DebugStopReason::Pause:
        return "pause";
    case DebugStopReason::Error:
        return "error";
    case DebugStopReason::RunToLine:
        return "runToLine";
    case DebugStopReason::None:
    default:
        return "none";
    }
}
//=============================================================================
static nlohmann::json
stateToJson(Evaluator* eval)
{
    nlohmann::json result = { { "running", false }, { "paused", false }, { "currentLine", nullptr },
        { "currentFile", nullptr }, { "currentColumn", 0 }, { "function", "" },
        { "reason", "none" }, { "errorIdentifier", "" }, { "errorMessage", "" },
        { "canStep", true }, { "stack", nlohmann::json::array() }, { "revision", 0 },
        { "breakpointsRevision", 0 } };
    if (eval == nullptr) {
        return result;
    }
    result["revision"] = static_cast<long long>(eval->getDebugStateRevision());
    result["breakpointsRevision"] = static_cast<long long>(eval->getBreakpointsRevision());
    const DebugStopInfo& info = eval->getDebugStopInfo();
    bool paused = eval->isBreakpointActive() && info.reason != DebugStopReason::None;
    result["running"] = eval->isBreakpointActive();
    result["paused"] = paused;
    if (!paused) {
        return result;
    }
    result["reason"] = stopReasonName(info.reason);
    result["canStep"] = info.canStep;
    result["errorIdentifier"] = wstring_to_utf8(info.errorIdentifier);
    result["errorMessage"] = wstring_to_utf8(info.errorMessage);
    result["currentFile"] = wstring_to_utf8(info.filename);
    result["currentLine"] = static_cast<long long>(info.line);
    result["currentColumn"] = static_cast<long long>(info.column);
    result["function"] = info.functionName;
    stackTrace trace;
    DebugStack(eval->callstack, 0, trace);
    nlohmann::json stack = nlohmann::json::array();
    for (const PositionScript& position : trace) {
        stack.push_back({ { "file", wstring_to_utf8(position.getFilename()) },
            { "line", static_cast<long long>(position.getLine()) },
            { "function", wstring_to_utf8(position.getFunctionName()) } });
    }
    if (stack.empty()) {
        stack.push_back({ { "file", wstring_to_utf8(info.filename) },
            { "line", static_cast<long long>(info.line) }, { "function", info.functionName } });
    }
    result["stack"] = stack;
    return result;
}
//=============================================================================
static nlohmann::json
breakpointToJson(const Breakpoint& bp, bool withFile)
{
    nlohmann::json item
        = { { "id", static_cast<long long>(bp.id) }, { "line", static_cast<long long>(bp.line) },
              { "enabled", bp.enabled }, { "condition", wstring_to_utf8(bp.condition) } };
    if (withFile) {
        item["file"] = wstring_to_utf8(bp.filename);
        item["function"] = bp.functionName;
    }
    return item;
}
//=============================================================================
static nlohmann::json
fileBreakpointsToJson(Evaluator* eval, const std::string& fileUtf8)
{
    nlohmann::json lines = nlohmann::json::array();
    nlohmann::json breakpoints = nlohmann::json::array();
    if (eval != nullptr) {
        for (const Breakpoint& bp : eval->getUserBreakpoints(utf8_to_wstring(fileUtf8))) {
            lines.push_back(static_cast<long long>(bp.line));
            breakpoints.push_back(breakpointToJson(bp, false));
        }
    }
    return { { "file", fileUtf8 }, { "lines", lines }, { "breakpoints", breakpoints } };
}
//=============================================================================
static nlohmann::json
allBreakpointsToJson(Evaluator* eval)
{
    nlohmann::json breakpoints = nlohmann::json::array();
    nlohmann::json errorBreakpoints = { { "error", false }, { "caughtError", false } };
    long long revision = 0;
    if (eval != nullptr) {
        for (const Breakpoint& bp : eval->getUserBreakpoints()) {
            breakpoints.push_back(breakpointToJson(bp, true));
        }
        errorBreakpoints["error"] = eval->getStopOnError();
        errorBreakpoints["caughtError"] = eval->getStopOnCaughtError();
        revision = static_cast<long long>(eval->getBreakpointsRevision());
    }
    return { { "breakpoints", breakpoints }, { "errorBreakpoints", errorBreakpoints },
        { "revision", revision } };
}
//=============================================================================
std::string
debuggerStateJson(Evaluator* eval)
{
    return stateToJson(eval).dump();
}
//=============================================================================
std::string
debuggerBreakpointsJson(Evaluator* eval, const std::string& fileUtf8)
{
    return fileBreakpointsToJson(eval, fileUtf8).dump();
}
//=============================================================================
std::string
debuggerAllBreakpointsJson(Evaluator* eval)
{
    return allBreakpointsToJson(eval).dump();
}
//=============================================================================
static nlohmann::json
errorJson(const std::wstring& message)
{
    return { { "error", wstring_to_utf8(message) } };
}
//=============================================================================
static nlohmann::json
rpcCommand(Evaluator* eval, const nlohmann::json& params)
{
    const std::string cmd = params.value("cmd", std::string());
    if (cmd == "pause") {
        bool accepted = eval->requestPause();
        return { { "ok", accepted } };
    }
    if (cmd == "runToLine") {
        std::wstring file = utf8_to_wstring(params.value("file", std::string()));
        size_t line = static_cast<size_t>(params.value("line", 0LL));
        std::wstring errorMessage;
        if (!runToEditorLine(eval, file, line, errorMessage)) {
            return errorJson(errorMessage);
        }
        return { { "ok", true } };
    }
    std::wstring command;
    if (cmd == "step") {
        command = L"dbstep";
    } else if (cmd == "stepIn") {
        command = L"dbstep in";
    } else if (cmd == "stepOut") {
        command = L"dbstep out";
    } else if (cmd == "continue") {
        command = L"dbcont";
    } else if (cmd == "quit") {
        command = L"dbquit";
    } else {
        return errorJson(L"unknown debugger command");
    }
    if (!eval->isBreakpointActive()) {
        return errorJson(L"not paused in the debugger");
    }
    setNextDebugStopQuiet(true);
    eval->addCommandToQueue(command, true);
    return { { "ok", true } };
}
//=============================================================================
std::string
debuggerRpc(Evaluator* eval, const std::string& method, const std::string& paramsJson)
{
    nlohmann::json params = nlohmann::json::parse(paramsJson, nullptr, false);
    if (!params.is_object()) {
        params = nlohmann::json::object();
    }
    if (eval == nullptr) {
        return errorJson(L"no evaluator").dump();
    }
    if (method == "state") {
        return stateToJson(eval).dump();
    }
    const std::string fileUtf8 = params.value("file", std::string());
    const std::wstring file = utf8_to_wstring(fileUtf8);
    const size_t line = static_cast<size_t>(params.value("line", 0LL));
    if (method == "getBreakpoints") {
        return fileBreakpointsToJson(eval, fileUtf8).dump();
    }
    if (method == "listBreakpoints") {
        return allBreakpointsToJson(eval).dump();
    }
    if (method == "toggleBreakpoint") {
        bool added = false;
        std::wstring errorMessage;
        size_t actedLine = toggleEditorBreakpoint(eval, file, line, added, errorMessage);
        nlohmann::json result = fileBreakpointsToJson(eval, fileUtf8);
        result["line"] = static_cast<long long>(actedLine);
        result["added"] = added;
        if (actedLine == 0 && !errorMessage.empty()) {
            result["error"] = wstring_to_utf8(errorMessage);
        }
        return result.dump();
    }
    if (method == "setBreakpoint") {
        std::wstring errorMessage;
        size_t targetLine = line;
        if (eval->findBreakpoint(file, line) == nullptr) {
            targetLine = addEditorBreakpoint(eval, file, line, errorMessage);
            if (targetLine == 0) {
                return errorJson(errorMessage).dump();
            }
        }
        if (params.contains("enabled")) {
            eval->setBreakpointEnabled(file, targetLine, params.value("enabled", true));
        }
        if (params.contains("condition")) {
            std::wstring condition = utf8_to_wstring(params.value("condition", std::string()));
            if (!eval->setBreakpointCondition(file, targetLine, condition, errorMessage)) {
                return errorJson(errorMessage).dump();
            }
        }
        nlohmann::json result = fileBreakpointsToJson(eval, fileUtf8);
        result["line"] = static_cast<long long>(targetLine);
        return result.dump();
    }
    if (method == "removeBreakpoint") {
        eval->removeBreakpoint(file, line);
        return fileBreakpointsToJson(eval, fileUtf8).dump();
    }
    if (method == "removeAllBreakpoints") {
        eval->removeAllBreakpoints(file);
        return allBreakpointsToJson(eval).dump();
    }
    if (method == "setAllEnabled") {
        eval->setAllBreakpointsEnabled(params.value("enabled", true), file);
        return allBreakpointsToJson(eval).dump();
    }
    if (method == "moveBreakpoints") {
        if (params.contains("moves") && params["moves"].is_array()) {
            // apply the moves on a copy of the table: a target may be another move's source
            std::vector<Breakpoint> before = eval->getUserBreakpoints(file);
            std::vector<std::pair<size_t, size_t>> moves;
            for (const auto& move : params["moves"]) {
                moves.emplace_back(static_cast<size_t>(move.value("from", 0LL)),
                    static_cast<size_t>(move.value("to", 0LL)));
            }
            eval->removeAllBreakpoints(file);
            for (const Breakpoint& bp : before) {
                Breakpoint moved = bp;
                for (const auto& move : moves) {
                    if (move.first == bp.line) {
                        moved.line = move.second;
                        break;
                    }
                }
                if (moved.line == 0) {
                    continue;
                }
                std::wstring errorMessage;
                size_t adjusted = moved.line;
                if (eval->adjustBreakpointLine(file, moved.line, adjusted, errorMessage)) {
                    moved.line = adjusted;
                }
                moved.id = 0;
                eval->addBreakpoint(moved);
                std::wstring conditionError;
                eval->setBreakpointCondition(file, moved.line, bp.condition, conditionError);
                eval->setBreakpointEnabled(file, moved.line, bp.enabled);
            }
        }
        return fileBreakpointsToJson(eval, fileUtf8).dump();
    }
    if (method == "setErrorBreakpoints") {
        if (params.contains("error")) {
            eval->setStopOnError(params.value("error", false));
        }
        if (params.contains("caughtError")) {
            eval->setStopOnCaughtError(params.value("caughtError", false));
        }
        return allBreakpointsToJson(eval).dump();
    }
    if (method == "command") {
        return rpcCommand(eval, params).dump();
    }
    if (method == "evaluate") {
        std::wstring expression = utf8_to_wstring(params.value("expr", std::string()));
        std::wstring preview;
        std::wstring errorMessage;
        bool ok = debugValuePreview(eval, expression, preview, errorMessage);
        return nlohmann::json {
            { "ok", ok }, { "text", wstring_to_utf8(preview) },
            { "error", wstring_to_utf8(errorMessage) }
        }.dump();
    }
    return errorJson(L"unknown debugger method: " + utf8_to_wstring(method)).dump();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
