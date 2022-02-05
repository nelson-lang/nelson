//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsAudio_exports.h"
#include <portaudio.h>
//=============================================================================
namespace Nelson {
//=============================================================================
#define AUDIOPLAYER_CATEGORY_STR L"audioplayer"
//=============================================================================
class NLSAUDIO_IMPEXP AudioplayerObject : public HandleGenericObject
{
public:
    AudioplayerObject();
    ~AudioplayerObject() override;
    bool
    isWriteableProperty(const std::wstring& propertyName);

    // getter
    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    int
    getTotalSamples();
    bool
    getRunning();

    // setter
    bool
    set(const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage);

    // disp
    bool
    disp(Interface* eval);
    wstringVector
    fieldnames();
    bool
    isProperty(const std::wstring& propertyName) override;
    bool
    isMethod(const std::wstring& methodName) override;

    bool
    setSamples(const ArrayOf& data, int SampleRate, std::wstring& errorMessage);
    bool
    setSamples(const ArrayOf& data, int SampleRate, int BitsPerSample, std::wstring& errorMessage);
    bool
    setSamples(const ArrayOf& data, int SampleRate, int BitsPerSample, int deviceID,
        std::wstring& errorMessage);

    bool
    play(int start = 0, int end = 0);
    bool
    pause();
    bool
    resume();
    bool
    stop();

private:
    wstringVector propertiesNames;
    ArrayOf audioData;
    int _SampleRate;
    int _BitsPerSample;
    int _NumberOfChannels;
    int _DeviceID;
    int _CurrentSample;
    int _TotalSamples;
    bool _Running;
    std::wstring _Tag;
    ArrayOf _UserData;
    std::wstring _Type;
    PaStream* paStream;
    uint32 firstSample;
    uint32 lastSample;

    // setter
    bool
    setSampleRate(int sr);
    bool
    setRunning(bool on);
    bool
    setTag(const std::wstring& tag);
    bool
    setUserData(const Nelson::ArrayOf& userData);

    // getter
    int
    getSampleRate();
    int
    getBitsPerSample();
    int
    getNumberOfChannels();
    int
    getDeviceID();
    int
    getCurrentSample();
    std::wstring
    getTag();
    Nelson::ArrayOf
    getUserData();
    std::wstring
    getType();
    PaStream*
    getStream();

    PaStreamParameters outputStreamParameters;

    // callback
    static int
    paPlayCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags,
        void* userData);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
