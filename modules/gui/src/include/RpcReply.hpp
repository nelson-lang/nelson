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
#include <string>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
// Classifies a GuiRpc reply (the { result } | { error:{ message } } object
// handleRpc and the nflow host return) so the figure, debugger and nflow dispatch
// unwrap it the same way instead of each hand-rolling the parse / error-extract.
struct RpcReply
{
    enum class Kind
    {
        Ok,
        Error,
        Malformed
    };
    Kind kind { Kind::Ok };
    nlohmann::json result;
    std::string message;
};
//=============================================================================
// Ok carries value("result", null); Error carries the error message (an object's
// "message" field, or a bare string error), with an empty message left for the
// caller to default; Malformed means the reply did not parse.
inline RpcReply
parseRpcReply(const std::string& reply)
{
    RpcReply out;
    const nlohmann::json parsed = nlohmann::json::parse(reply, nullptr, false);
    if (parsed.is_discarded()) {
        out.kind = RpcReply::Kind::Malformed;
        return out;
    }
    if (parsed.is_object() && parsed.contains("error")) {
        out.kind = RpcReply::Kind::Error;
        const nlohmann::json& error = parsed["error"];
        if (error.is_object() && error.contains("message")) {
            out.message = error.value("message", std::string());
        } else if (error.is_string()) {
            out.message = error.get<std::string>();
        }
        return out;
    }
    out.result = parsed.value("result", nlohmann::json(nullptr));
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
