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
#include <atomic>
#include <mutex>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "AudioplayerObject.hpp"
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSAUDIO_IMPEXP AudiorecorderObject : public HandleGenericObject
{
public:
    AudiorecorderObject(bool withEventLoop);
    ~AudiorecorderObject() override;

    bool
    isWritableProperty(const std::wstring& propertyName);

    bool
    get(const std::wstring& propertyName, ArrayOf& res);

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
    setConfig(int SampleRate, int BitsPerSample, int NumberOfChannels, int deviceID);

    // control, a PortAudio error is returned when the stream cannot be started
    PaError
    start();
    bool
    pause();
    PaError
    resume();
    bool
    stop();

    PaError
    record(double durationSeconds);

    int
    getTotalSamples();
    bool
    getRunning();
    ArrayOf
    getRecordedData(NelsonType destinationType);
    int
    getSampleRate();
    int
    getBitsPerSample();
    int
    getNumberOfChannels();
    int
    getDeviceID();

    double
    getTimerPeriodSeconds() const
    {
        return timerPeriodSeconds;
    }

    ArrayOf
    getStartFcn()
    {
        return startFunc;
    }
    ArrayOf
    getStopFcn()
    {
        return stopFunc;
    }
    ArrayOf
    getTimerFcn()
    {
        return timerFunc;
    }

    static int
    paRecordCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags,
        void* userData);

    ArrayOf
    getPlayer();

private:
    std::atomic<bool> timedRecording { false };
    std::atomic<uint64_t> timedTargetFrames { 0 };

    wstringVector propertiesNames;
    wstringVector methodsNames;
    int _SampleRate;
    int _BitsPerSample;
    int _NumberOfChannels;
    int _DeviceID;
    int _CurrentSample;
    // written by the audio thread
    std::atomic<int> _TotalSamples;
    std::atomic<bool> _Running;
    // StopFcn is due once per recording, on timed end or on stop
    std::atomic<bool> stopFcnPending { false };
    std::wstring _Tag;
    ArrayOf _UserData;
    std::string _Type;
    PaStreamParameters inputStreamParameters;
    double timerPeriodSeconds { 0.05 };

    bool mainEvaluatorHasEventsLoop { false };
    ArrayOf startFunc;
    ArrayOf stopFunc;
    ArrayOf timerFunc;

    AudioStream stream;
    std::mutex recordMutex; // guards recordBuffer
    std::vector<single> recordBuffer; // interleaved samples
    std::atomic<uint32_t> recordedFrames;
    uint64_t lastTimerFrame = 0; // audio thread only
    NelsonType recordedDataClass;

    AudioplayerObject* audioPlayerObject;
    bool audioPlayerUsed;

    void
    appendFrames(const void* inputBuffer, unsigned long framesPerBuffer);
    void
    triggerTimerIfNeeded();
    void
    enqueueStopFcnOnce();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
