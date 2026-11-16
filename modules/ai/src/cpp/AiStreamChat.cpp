//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "AiStreamChat.hpp"
#include <stdexcept>
//=============================================================================
#ifdef NELSON_AI_WITH_CURL
#include <curl/curl.h>
#include <nlohmann/json.hpp>
#include "NelsonConfiguration.hpp"
#ifdef _MSC_VER
#pragma comment(lib, "libcurl.lib")
#endif
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef NELSON_AI_WITH_CURL
//=============================================================================
// Per-request streaming state shared between the write and progress callbacks.
struct StreamState
{
    std::string buffer;
    std::string full;
    std::size_t evaluatorID = 0;
    const std::function<void(const std::string&)>* onDelta = nullptr;
};
//=============================================================================
// Pull one text fragment out of a complete ndjson line and deliver it.
static void
consumeLine(StreamState* state, const std::string& line)
{
    if (line.empty()) {
        return;
    }
    nlohmann::json parsed = nlohmann::json::parse(line, nullptr, false);
    if (parsed.is_discarded() || !parsed.is_object()) {
        return;
    }
    auto it = parsed.find("response");
    if (it == parsed.end() || !it->is_string()) {
        return;
    }
    const std::string fragment = it->get<std::string>();
    if (fragment.empty()) {
        return;
    }
    state->full += fragment;
    if (state->onDelta != nullptr && *state->onDelta) {
        (*state->onDelta)(fragment);
    }
}
//=============================================================================
static size_t
writeCallback(char* ptr, size_t size, size_t nmemb, void* userdata)
{
    StreamState* state = static_cast<StreamState*>(userdata);
    if (NelsonConfiguration::getInstance()->getInterruptPending(state->evaluatorID)) {
        return 0;
    }
    state->buffer.append(ptr, size * nmemb);
    size_t newline = state->buffer.find('\n');
    while (newline != std::string::npos) {
        std::string line = state->buffer.substr(0, newline);
        state->buffer.erase(0, newline + 1);
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        consumeLine(state, line);
        newline = state->buffer.find('\n');
    }
    return size * nmemb;
}
//=============================================================================
static int
progressCallback(void* clientp, curl_off_t, curl_off_t, curl_off_t, curl_off_t)
{
    StreamState* state = static_cast<StreamState*>(clientp);
    if (NelsonConfiguration::getInstance()->getInterruptPending(state->evaluatorID)) {
        return 1;
    }
    return 0;
}
//=============================================================================
bool
aiStreamingAvailable()
{
    return true;
}
//=============================================================================
std::string
aiStreamOllamaGenerate(const std::string& endpoint, const std::string& model,
    const std::string& prompt, const std::string& systemPrompt, long numPredict, bool hasNumPredict,
    double temperature, bool hasTemperature, double timeoutSeconds, std::size_t evaluatorID,
    const std::function<void(const std::string&)>& onDelta)
{
    nlohmann::json body;
    body["model"] = model;
    body["prompt"] = prompt;
    if (!systemPrompt.empty()) {
        body["system"] = systemPrompt;
    }
    body["stream"] = true;
    nlohmann::json opts = nlohmann::json::object();
    if (hasNumPredict) {
        opts["num_predict"] = numPredict;
    }
    if (hasTemperature) {
        opts["temperature"] = temperature;
    }
    if (!opts.empty()) {
        body["options"] = opts;
    }
    const std::string payload = body.dump();

    CURL* curlObject = curl_easy_init();
    if (curlObject == nullptr) {
        throw std::runtime_error("cannot initialize HTTP client");
    }
    StreamState state;
    state.evaluatorID = evaluatorID;
    state.onDelta = &onDelta;

    struct curl_slist* headers = nullptr;
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curlObject, CURLOPT_URL, endpoint.c_str());
    curl_easy_setopt(curlObject, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curlObject, CURLOPT_POST, 1L);
    curl_easy_setopt(curlObject, CURLOPT_POSTFIELDS, payload.c_str());
    curl_easy_setopt(curlObject, CURLOPT_POSTFIELDSIZE, (long)payload.size());
    curl_easy_setopt(curlObject, CURLOPT_WRITEFUNCTION, writeCallback);
    curl_easy_setopt(curlObject, CURLOPT_WRITEDATA, &state);
    curl_easy_setopt(curlObject, CURLOPT_NOPROGRESS, 0L);
    curl_easy_setopt(curlObject, CURLOPT_XFERINFOFUNCTION, progressCallback);
    curl_easy_setopt(curlObject, CURLOPT_XFERINFODATA, &state);
    if (timeoutSeconds > 0) {
        curl_easy_setopt(curlObject, CURLOPT_CONNECTTIMEOUT_MS, (long)(timeoutSeconds * 1000));
        curl_easy_setopt(curlObject, CURLOPT_LOW_SPEED_LIMIT, 1L);
        curl_easy_setopt(curlObject, CURLOPT_LOW_SPEED_TIME, (long)timeoutSeconds + 1L);
    }

    CURLcode code = curl_easy_perform(curlObject);
    long responseCode = 0;
    curl_easy_getinfo(curlObject, CURLINFO_RESPONSE_CODE, &responseCode);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curlObject);

    // A trailing fragment without a newline is still part of the reply.
    if (!state.buffer.empty()) {
        consumeLine(&state, state.buffer);
    }
    if (code == CURLE_ABORTED_BY_CALLBACK || code == CURLE_WRITE_ERROR) {
        throw std::runtime_error("interrupted");
    }
    if (code != CURLE_OK) {
        throw std::runtime_error(curl_easy_strerror(code));
    }
    if (responseCode >= 400) {
        throw std::runtime_error("HTTP error " + std::to_string(responseCode));
    }
    return state.full;
}
//=============================================================================
#else
//=============================================================================
bool
aiStreamingAvailable()
{
    return false;
}
//=============================================================================
std::string
aiStreamOllamaGenerate(const std::string&, const std::string&, const std::string&,
    const std::string&, long, bool, double, bool, double, std::size_t,
    const std::function<void(const std::string&)>&)
{
    throw std::runtime_error("streaming not available in this build");
}
//=============================================================================
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
