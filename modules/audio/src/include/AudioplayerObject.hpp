//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    ~AudioplayerObject();
    bool
    isWriteableProperty(std::wstring propertyName);

    // getter
    bool
    get(std::wstring propertyName, ArrayOf& res);
    int
    getTotalSamples();
    bool
    getRunning();

    // setter
    bool
    set(std::wstring propertyName, ArrayOf propertyValue, std::wstring& errorMessage);

    // disp
    bool
    disp(Evaluator* eval);
    wstringVector
    fieldnames();
    bool
    isProperty(std::wstring propertyName);
    bool
    isMethod(std::wstring methodName);

    bool
    setSamples(ArrayOf data, int SampleRate, std::wstring& errorMessage);
    bool
    setSamples(ArrayOf data, int SampleRate, int BitsPerSample, std::wstring& errorMessage);
    bool
    setSamples(
        ArrayOf data, int SampleRate, int BitsPerSample, int deviceID, std::wstring& errorMessage);

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
    setTag(std::wstring tag);
    bool
    setUserData(Nelson::ArrayOf userData);

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
