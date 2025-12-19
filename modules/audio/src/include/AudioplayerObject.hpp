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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSAUDIO_IMPEXP AudioplayerObject : public HandleGenericObject
{
public:
    AudioplayerObject(bool withEventLoop);
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
    wstringVector
    getProperties() override;
    wstringVector
    getMethods() override;

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
    std::string _Type;
    PaStream* paStream;
    uint32 firstSample;
    uint32 lastSample;

    ArrayOf startFunc;
    ArrayOf stopFunc;
    ArrayOf timerFunc;
    double timerPeriodSeconds;

    bool mainEvaluatorHasEventsLoop = false;

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
    std::string
    getType();
    PaStream*
    getStream();

    ArrayOf
    getStartFcn();
    ArrayOf
    getStopFcn();
    ArrayOf
    getTimerFcn();

    PaStreamParameters outputStreamParameters;

    // callback
    static int
    paPlayCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags,
        void* userData);

    void
    enqueueCallback(const ArrayOf& callbackArrayOf);

    bool
    isFunctionHandleCallbackValid(const ArrayOf& callbackArrayOf);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
