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
#include <atomic>
#include <portaudio.h>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "AudioStream.hpp"
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
    isWritableProperty(const std::wstring& propertyName);

    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    int
    getTotalSamples();
    int
    getSampleRate();
    bool
    getRunning();
    bool
    isStreamFinishing();

    bool
    set(const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage);

    bool
    disp(Interface* eval);
    wstringVector
    fieldnames();
    bool
    isProperty(const std::wstring& propertyName) override;
    bool
    isMethod(const std::wstring& methodName) override;
    wstringVector
    properties() override;
    wstringVector
    methods() override;

    bool
    setSamples(const ArrayOf& data, int SampleRate, std::wstring& errorMessage);
    bool
    setSamples(const ArrayOf& data, int SampleRate, int BitsPerSample, std::wstring& errorMessage);
    bool
    setSamples(const ArrayOf& data, int SampleRate, int BitsPerSample, int deviceID,
        std::wstring& errorMessage);

    // returns the PortAudio error when the stream cannot be started
    PaError
    play(int start = 0, int end = 0);
    bool
    pause();
    PaError
    resume();
    bool
    stop();

private:
    wstringVector propertiesNames;
    wstringVector methodsNames;
    ArrayOf audioData;
    int _SampleRate;
    int _BitsPerSample;
    int _NumberOfChannels;
    int _DeviceID;
    // written by the audio thread
    std::atomic<int> _CurrentSample;
    std::atomic<bool> _Running;
    // StopFcn is due once per play, on natural end or on stop
    std::atomic<bool> stopFcnPending { false };
    int _TotalSamples;
    std::wstring _Tag;
    ArrayOf _UserData;
    std::string _Type;
    AudioStream stream;
    uint32 firstSample;
    uint32 lastSample;

    ArrayOf startFunc;
    ArrayOf stopFunc;
    ArrayOf timerFunc;
    double timerPeriodSeconds;

    bool mainEvaluatorHasEventsLoop = false;

    bool
    setSampleRate(int sr);
    bool
    setTag(const std::wstring& tag);
    bool
    setUserData(const Nelson::ArrayOf& userData);

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

    ArrayOf
    getStartFcn();
    ArrayOf
    getStopFcn();
    ArrayOf
    getTimerFcn();

    PaStreamParameters outputStreamParameters;

    static int
    paPlayCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags,
        void* userData);

    void
    enqueueStopFcnOnce();
    // closes the stream of a natural end (main thread)
    void
    releaseFinishedStream();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
