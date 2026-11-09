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
// A device reached without a system mixer (e.g. WDM-KS) only runs its native rates.
// When requestedRate is not one of them, gives the float parameters and the device default
// rate to open instead, the caller converting the samples; false when requestedRate is
// supported as is or the device cannot run its default rate either.
inline bool
deviceRateForUnsupportedRate(const PaStreamParameters& parameters, bool isInput,
    double requestedRate, PaStreamParameters& deviceParameters, double& deviceRate)
{
    const PaStreamParameters* input = isInput ? &parameters : nullptr;
    const PaStreamParameters* output = isInput ? nullptr : &parameters;
    if (Pa_IsFormatSupported(input, output, requestedRate) == paFormatIsSupported) {
        return false;
    }
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
