//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebToolsHost.hpp"
#include "DynamicLibrary.hpp"
#include <nlohmann/json.hpp>
#include <map>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using json = nlohmann::json;
    using DispatchSink = void (*)(void* context, const char* responseJson);
    using DispatchFunction
        = int (*)(const char* method, const char* paramsJson, DispatchSink sink, void* context);
    //=============================================================================
    struct WebTool
    {
        std::wstring libraryPath;
        std::string symbol;
        library_handle handle = nullptr;
    };
    //=============================================================================
    struct ToolsState
    {
        std::map<std::string, WebTool> tools;
        WebToolHost host;
    };
    //=============================================================================
    std::mutex&
    stateMutex()
    {
        static auto* mutex = new std::mutex;
        return *mutex;
    }
    //=============================================================================
    ToolsState&
    toolsState()
    {
        static auto* value = new ToolsState;
        return *value;
    }
    //=============================================================================
    std::string
    errorResponse(const std::string& message)
    {
        return json { { "error", { { "message", message } } } }.dump();
    }
    //=============================================================================
    void
    collectResponse(void* context, const char* responseJson)
    {
        if (context != nullptr && responseJson != nullptr) {
            static_cast<std::string*>(context)->assign(responseJson);
        }
    }
    //=============================================================================
    // Resolves the dispatcher under the lock; the call itself runs unlocked
    // (a tool may take seconds, and may push events back through this file).
    DispatchFunction
    resolveDispatcher(const std::string& tool, std::string& error)
    {
        std::lock_guard<std::mutex> lock(stateMutex());
        auto& tools = toolsState().tools;
        auto found = tools.find(tool);
        if (found == tools.end()) {
            error = "unknown web tool: " + tool;
            return nullptr;
        }
        WebTool& entry = found->second;
        if (entry.handle == nullptr) {
            entry.handle = load_dynamic_libraryW(entry.libraryPath);
        }
        if (entry.handle == nullptr) {
            error = "cannot load the library of web tool " + tool;
            return nullptr;
        }
        auto function
            = reinterpret_cast<DispatchFunction>(get_function(entry.handle, entry.symbol));
        if (function == nullptr) {
            error = "web tool symbol not found: " + entry.symbol;
        }
        return function;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
RegisterWebTool(const std::string& tool, const std::wstring& libraryPath, const std::string& symbol)
{
    std::lock_guard<std::mutex> lock(stateMutex());
    WebTool& entry = toolsState().tools[tool];
    if (entry.handle != nullptr && (entry.libraryPath != libraryPath || entry.symbol != symbol)) {
        close_dynamic_library(entry.handle);
        entry.handle = nullptr;
    }
    entry.libraryPath = libraryPath;
    entry.symbol = symbol;
}
//=============================================================================
bool
UnregisterWebTool(const std::string& tool)
{
    std::lock_guard<std::mutex> lock(stateMutex());
    auto& tools = toolsState().tools;
    auto found = tools.find(tool);
    if (found == tools.end()) {
        return false;
    }
    if (found->second.handle != nullptr) {
        close_dynamic_library(found->second.handle);
    }
    tools.erase(found);
    return true;
}
//=============================================================================
bool
IsWebToolRegistered(const std::string& tool)
{
    std::lock_guard<std::mutex> lock(stateMutex());
    return toolsState().tools.count(tool) != 0;
}
//=============================================================================
std::string
DispatchWebTool(const std::string& tool, const std::string& method, const std::string& paramsJson)
{
    std::string error;
    DispatchFunction function = resolveDispatcher(tool, error);
    if (function == nullptr) {
        return errorResponse(error);
    }
    std::string response;
    if (function(method.c_str(), paramsJson.c_str(), collectResponse, &response) == 0
        || response.empty()) {
        return errorResponse("web tool " + tool + " failed on " + method);
    }
    return response;
}
//=============================================================================
void
SetWebToolHost(const WebToolHost& host)
{
    std::lock_guard<std::mutex> lock(stateMutex());
    toolsState().host = host;
}
//=============================================================================
bool
ShowWebToolInHost(const std::string& tool, const std::string& argsJson)
{
    std::function<bool(const std::string&, const std::string&)> show;
    {
        std::lock_guard<std::mutex> lock(stateMutex());
        show = toolsState().host.show;
    }
    return show ? show(tool, argsJson) : false;
}
//=============================================================================
bool
PushWebToolEvent(
    const std::string& tool, const std::string& channel, const std::string& payloadJson)
{
    std::function<void(const std::string&, const std::string&, const std::string&)> push;
    {
        std::lock_guard<std::mutex> lock(stateMutex());
        push = toolsState().host.push;
    }
    if (!push) {
        return false;
    }
    push(tool, channel, payloadJson);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
