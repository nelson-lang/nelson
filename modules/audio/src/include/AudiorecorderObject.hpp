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
    isWriteableProperty(const std::wstring& propertyName);

    // getter
    bool
    get(const std::wstring& propertyName, ArrayOf& res);

    // setter
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
    getProperties() override;
    wstringVector
    getMethods() override;

    // configure recorder
    bool
    setConfig(int SampleRate, int BitsPerSample, int NumberOfChannels, int deviceID);

    // control
    bool
    start();
    bool
    pause();
    bool
    resume();
    bool
    stop();

    bool
    record(double durationSeconds);

    int
    getTotalSamples();
    bool
    getRunning();
    PaStream*
    getStream();
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

    uint32_t
    getRecordedFrames() const
    {
        return recordedFrames;
    }

    // PortAudio callback
    static int
    paRecordCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags,
        void* userData);

    ArrayOf
    getPlayer();

    bool
    isFunctionHandleCallbackValid(const ArrayOf& callbackArrayOf);

    void
    enqueueCallback(const ArrayOf& callbackArrayOf);

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
    int _TotalSamples;
    bool _Running;
    std::wstring _Tag;
    ArrayOf _UserData;
    std::string _Type;
    PaStreamParameters inputStreamParameters;
    double timerPeriodSeconds { 0.05 };

    bool mainEvaluatorHasEventsLoop { false };
    ArrayOf startFunc;
    ArrayOf stopFunc;
    ArrayOf timerFunc;

    // recording state
    PaStream* paStream;
    std::vector<single> recordBuffer; // interleaved samples
    uint32_t recordedFrames;
    NelsonType recordedDataClass;

    AudioplayerObject* audioPlayerObject;
    bool audioPlayerUsed;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
