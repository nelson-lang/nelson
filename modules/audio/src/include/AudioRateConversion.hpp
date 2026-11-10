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
#include <portaudio.h>
//=============================================================================
namespace Nelson {
//=============================================================================
// After a failed open at requestedRate (device without mixer, e.g. WDM-KS): float parameters
// and device default rate to open instead, caller converts; no probe up front (costs an open)
inline bool
deviceRateForUnsupportedRate(const PaStreamParameters& parameters, bool isInput,
    double requestedRate, PaStreamParameters& deviceParameters, double& deviceRate)
{
    const PaDeviceInfo* info = Pa_GetDeviceInfo(parameters.device);
    if (info == nullptr || info->defaultSampleRate <= 0
        || info->defaultSampleRate == requestedRate) {
        return false;
    }
    deviceParameters = parameters;
    deviceParameters.sampleFormat = paFloat32;
    const PaError supported = isInput
        ? Pa_IsFormatSupported(&deviceParameters, nullptr, info->defaultSampleRate)
        : Pa_IsFormatSupported(nullptr, &deviceParameters, info->defaultSampleRate);
    if (supported != paFormatIsSupported) {
        return false;
    }
    deviceRate = info->defaultSampleRate;
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
