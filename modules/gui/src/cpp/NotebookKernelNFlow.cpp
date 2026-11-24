//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NotebookKernelNFlow.hpp"
#include "RpcReply.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NotebookKernelNFlow::NotebookKernelNFlow(
    PushFn push, ResultWriter writeResult, ErrorWriter writeError)
    : m_push(std::move(push))
    , m_writeResult(std::move(writeResult))
    , m_writeError(std::move(writeError))
{
    const std::wstring libName = L"libnlsNflow_gui_builtin" + get_dynamic_library_extensionW();
    const std::wstring dir = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    library_handle handle = load_dynamic_libraryW(dir + L"/" + libName);
    if (handle == nullptr) {
        return;
    }
    m_fnCreate = reinterpret_cast<CreateFn>(get_function(handle, "nlsNFlowKernelCreate"));
    m_fnDispatch = reinterpret_cast<DispatchFn>(get_function(handle, "nlsNFlowKernelDispatch"));
    m_fnDistDir = reinterpret_cast<DistDirFn>(get_function(handle, "nlsNFlowKernelDistDir"));
    m_fnFree = reinterpret_cast<FreeFn>(get_function(handle, "nlsNFlowKernelFreeString"));
    m_fnDestroy = reinterpret_cast<DestroyFn>(get_function(handle, "nlsNFlowKernelDestroy"));
    m_available = m_fnCreate != nullptr && m_fnDispatch != nullptr && m_fnDistDir != nullptr
        && m_fnFree != nullptr && m_fnDestroy != nullptr;
}
//=============================================================================
NotebookKernelNFlow::~NotebookKernelNFlow()
{
    if (m_host != nullptr && m_fnDestroy != nullptr) {
        m_fnDestroy(m_host);
        m_host = nullptr;
    }
}
//=============================================================================
void
NotebookKernelNFlow::pushThunk(void* user, const char* channel, const char* payloadJson)
{
    auto* self = static_cast<NotebookKernelNFlow*>(user);
    if (self->m_push) {
        self->m_push(channel != nullptr ? channel : "", payloadJson != nullptr ? payloadJson : "");
    }
}
//=============================================================================
void
NotebookKernelNFlow::ensureHost()
{
    if (m_host != nullptr || !m_available) {
        return;
    }
    // The host binds to the kernel's evaluator so From/To Workspace reach the
    // same base scope as cells and the REPL. It defers the workspace bridge to
    // the first run (marshalled to the interpreter thread), so creating it here
    // on the reader thread is safe.
    void* eval = NelsonConfiguration::getInstance()->getMainEvaluator();
    m_host = m_fnCreate(eval, &NotebookKernelNFlow::pushThunk, this);
}
//=============================================================================
bool
NotebookKernelNFlow::handles(const std::string& method) const
{
    return m_available && method.rfind("nflow.", 0) == 0;
}
//=============================================================================
void
NotebookKernelNFlow::handle(
    const nlohmann::json& id, const std::string& method, const nlohmann::json& params)
{
    if (!m_available) {
        m_writeError(id, -32601, "nflow not available: " + method);
        return;
    }
    ensureHost();
    if (m_host == nullptr) {
        m_writeError(id, -32603, "nflow host could not be created");
        return;
    }
    const std::string paramsJson = params.dump();
    char* reply = m_fnDispatch(m_host, method.c_str(), paramsJson.c_str());
    const std::string replyStr = reply != nullptr ? std::string(reply) : std::string();
    if (reply != nullptr) {
        m_fnFree(reply);
    }
    // The host answers {"result":<any>} on success or {"error":{"message":...}}
    // on failure; re-wrap it as a JSON-RPC 2.0 result or error.
    const RpcReply env = parseRpcReply(replyStr);
    if (env.kind == RpcReply::Kind::Malformed) {
        m_writeError(id, -32603, "malformed nflow dispatch reply");
        return;
    }
    if (env.kind == RpcReply::Kind::Error) {
        m_writeError(id, -32000, env.message.empty() ? "nflow error" : env.message);
        return;
    }
    m_writeResult(id, env.result);
}
//=============================================================================
void
NotebookKernelNFlow::augmentInitialize(nlohmann::json& result)
{
    result["capabilities"]["nflow"] = m_available;
    if (m_available) {
        result["nflowDistDir"] = distDir();
    }
}
//=============================================================================
std::string
NotebookKernelNFlow::distDir()
{
    if (!m_available) {
        return std::string();
    }
    char* d = m_fnDistDir();
    const std::string result = d != nullptr ? std::string(d) : std::string();
    if (d != nullptr) {
        m_fnFree(d);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
