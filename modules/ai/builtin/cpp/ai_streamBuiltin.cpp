//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ai_streamBuiltin.hpp"
#include "AiStreamChat.hpp"
#include "AssistantSink.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <stdexcept>
#ifndef NLSAI_WITHOUT_WEBVIEW
#include "WebToolsHost.hpp"
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
static double
optionalScalar(const ArrayOf& value, bool& has)
{
    has = !value.isEmpty();
    if (!has) {
        return 0.0;
    }
    return value.asDoubleScalar();
}
//=============================================================================
// txt = __ai_stream__(endpoint, model, prompt, system, numPredict, temperature,
// timeout[, surfaceId]): stream an Ollama reply, pushing each fragment to the
// assistant pane as a 'token' event, and return the full text. surfaceId routes
// the tokens to ONE surface (qt / nflow / web / notebook) like __ai_notify__, so
// the direct backend streams to the same pane the ACP backend does; omitted or
// empty keeps the historical web + broadcast behavior. Errors when streaming is
// not built in, so the caller can fall back to a non-streaming request.
ArrayOfVector
Nelson::AiGateway::ai_streamBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 7, 8);
    nargoutcheck(nLhs, 0, 1);
    if (!aiStreamingAvailable()) {
        Error(L"Nelson:ai:streamingUnavailable", L"Streaming is not available in this build.");
    }
    const std::string endpoint = argIn[0].asUtf8String();
    const std::string model = argIn[1].asUtf8String();
    const std::string prompt = argIn[2].asUtf8String();
    const std::string systemPrompt = argIn[3].asUtf8String();
    bool hasNumPredict = false;
    const long numPredict = (long)optionalScalar(argIn[4], hasNumPredict);
    bool hasTemperature = false;
    const double temperature = optionalScalar(argIn[5], hasTemperature);
    const double timeout = argIn[6].asDoubleScalar();
    std::string surfaceId;
    if (argIn.size() >= 8) {
        surfaceId = argIn[7].asUtf8String();
    }
    const std::size_t evaluatorID = (eval != nullptr) ? eval->getID() : 0;

    const bool toWeb = surfaceId.empty() || surfaceId == "web" || surfaceId == "notebook";
    auto onDelta = [surfaceId, toWeb](const std::string& fragment) {
        nlohmann::json payload;
        payload["text"] = fragment;
        const std::string dumped = payload.dump();
        if (toWeb) {
#ifndef NLSAI_WITHOUT_WEBVIEW
            PushWebToolEvent("assistant", "token", dumped);
#endif
        }
        if (surfaceId.empty()) {
            // Historical broadcast (reaches whichever desktop pane is listening).
            Nelson::AssistantSinkBroadcast("token", dumped);
        } else if (!toWeb) {
            // A specific desktop surface (its pane registered under this surfaceId).
            Nelson::AssistantSinkPushTo(surfaceId, "token", dumped);
        }
    };

    std::string full;
    try {
        full = aiStreamOllamaGenerate(endpoint, model, prompt, systemPrompt, numPredict,
            hasNumPredict, temperature, hasTemperature, timeout, evaluatorID, onDelta);
    } catch (const std::exception& e) {
        Error(L"Nelson:ai:streamError", utf8_to_wstring(e.what()));
    }
    return ArrayOfVector { ArrayOf::characterArray(full) };
}
//=============================================================================
