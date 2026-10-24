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
#include <chrono>
#include <vector>
#include "ArrayOf.hpp"
#include "HandleGenericObject.hpp"
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSAUDIO_IMPEXP PortableAudioplayerObject : public HandleGenericObject
{
public:
    PortableAudioplayerObject();

    bool
    setSamples(const ArrayOf& data, int sampleRate, int bitsPerSample, std::wstring& errorMessage);
    bool
    play(int firstSample, int lastSample, std::wstring& errorMessage);
    bool
    pause();
    bool
    resume();
    bool
    stop();
    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    bool
    set(const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage);
    bool
    isWritableProperty(const std::wstring& propertyName);
    int
    getCurrentSample();
    int
    getTotalSamples() const;
    bool
    getRunning();
    bool
    isProperty(const std::wstring& propertyName) override;
    wstringVector
    properties() override;
    wstringVector
    methods() override;

private:
    void
    updateCurrentSample();
    void
    emitControl(const char* command) const;
    std::vector<double> samples;
    std::chrono::steady_clock::time_point startedAt {};
    int audioId = 0;
    int sampleRate = 0;
    int bitsPerSample = 16;
    int channels = 0;
    int totalSamples = 0;
    int currentSample = 0;
    int firstSample = 0;
    int lastSample = 0;
    bool running = false;
    std::wstring tag;
    ArrayOf userData;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
