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
#include "nlsAi_exports.h"
#include <cstddef>
#include <functional>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// True when this build can stream model replies (libcurl available).
NLSAI_IMPEXP bool
aiStreamingAvailable();
//=============================================================================
// Stream an Ollama /api/generate completion (stream:true): onDelta is called
// for each text fragment as it arrives and the full concatenated reply is
// returned. The interrupt flag of evaluatorID aborts the request. Throws
// std::runtime_error on a transport or HTTP failure, or when streaming is not
// available in this build.
NLSAI_IMPEXP std::string
aiStreamOllamaGenerate(const std::string& endpoint, const std::string& model,
    const std::string& prompt, const std::string& systemPrompt, long numPredict,
    bool hasNumPredict, double temperature, bool hasTemperature, double timeoutSeconds,
    std::size_t evaluatorID, const std::function<void(const std::string&)>& onDelta);
//=============================================================================
} // namespace Nelson
//=============================================================================
