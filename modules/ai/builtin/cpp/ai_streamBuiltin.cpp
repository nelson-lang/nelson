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
// timeout): stream an Ollama reply, pushing each fragment to the assistant pane
// as a 'token' event, and return the full text. Errors when streaming is not
// built in, so the caller can fall back to a non-streaming request.
ArrayOfVector
Nelson::AiGateway::ai_streamBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 7, 7);
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
    const std::size_t evaluatorID = (eval != nullptr) ? eval->getID() : 0;

    auto onDelta = [](const std::string& fragment) {
#ifndef NLSAI_WITHOUT_WEBVIEW
        nlohmann::json payload;
        payload["text"] = fragment;
        PushWebToolEvent("assistant", "token", payload.dump());
#else
        (void)fragment;
#endif
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
