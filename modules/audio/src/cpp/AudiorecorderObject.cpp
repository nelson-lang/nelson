//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AudiorecorderObject.hpp"
#include "ClassName.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include <vector>
#include <map>
#include <functional>
#include <thread>
#include <chrono>
#include <cmath>
#include <atomic>
#include "TimerCallback.hpp"
#include "TimerQueue.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include <unordered_map> // added
#include <mutex> // added
//=============================================================================
namespace Nelson {
//=============================================================================
AudiorecorderObject::AudiorecorderObject()
    : HandleGenericObject(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, this, false)
{
    propertiesNames = { L"SampleRate", L"BitsPerSample", L"NumberOfChannels", L"DeviceID",
        L"CurrentSample", L"TotalSamples", L"Running", L"Tag", L"UserData", L"Type", L"StartFcn",
        L"StopFcn", L"TimerFnc", L"TimerPeriod" };

    methodsNames = { L"delete", L"get", L"getaudiodata", L"isrecording", L"pause", L"play",
        L"record", L"recordblocking", L"resume", L"set", L"stop" };
    methodMap = {};

    paStream = nullptr;
    recordedFrames = 0;
    recordBuffer.clear();
    recordedDataClass = NLS_SINGLE;

    int defaultInput = Pa_GetDefaultInputDevice();
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(defaultInput);
    if (pdi) {
        _SampleRate = static_cast<int>(pdi->defaultSampleRate);
        // default to 32-bit float input
        _BitsPerSample = 32;
        _NumberOfChannels
            = (pdi->maxInputChannels > 0) ? static_cast<int>(pdi->maxInputChannels) : 1;
        _DeviceID = defaultInput;
        _CurrentSample = 0;
        _TotalSamples = 0;
        _Running = false;
        _Tag.clear();
        _UserData = ArrayOf::emptyConstructor();
        _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;

        // initialize input stream parameters
        inputStreamParameters.device = _DeviceID;
        inputStreamParameters.channelCount = _NumberOfChannels;
        inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = pdi->defaultLowInputLatency;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    } else {
        _SampleRate = 44100;
        _BitsPerSample = 32;
        _NumberOfChannels = 1;
        _DeviceID = -1;
        _CurrentSample = 0;
        _TotalSamples = 0;
        _Running = false;
        _Tag.clear();
        _UserData = ArrayOf::emptyConstructor();
        _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;
        inputStreamParameters.device = paNoDevice;
        inputStreamParameters.channelCount = 1;
        inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = 0;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    }
}
//=============================================================================
AudiorecorderObject::~AudiorecorderObject()
{
    propertiesNames.clear();
    _SampleRate = 0;
    _BitsPerSample = 0;
    _NumberOfChannels = 0;
    _DeviceID = -1;
    _CurrentSample = 0;
    _TotalSamples = 0;
    _Running = false;
    _Tag.clear();
    _UserData = ArrayOf::emptyConstructor();
    _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;
}
//=============================================================================
int
AudiorecorderObject::getSampleRate()
{
    return _SampleRate;
}
//=============================================================================
int
AudiorecorderObject::getBitsPerSample()
{
    return _BitsPerSample;
}
//=============================================================================
int
AudiorecorderObject::getNumberOfChannels()
{
    return _NumberOfChannels;
}
//=============================================================================
int
AudiorecorderObject::getDeviceID()
{
    return _DeviceID;
}
//=============================================================================
bool
AudiorecorderObject::isWriteableProperty(const std::wstring& propertyName)
{
    static const std::map<std::wstring, bool> writeableProperties = { { L"SampleRate", true },
        { L"BitsPerSample", false }, { L"NumberOfChannels", false }, { L"DeviceID", false },
        { L"CurrentSample", false }, { L"TotalSamples", false }, { L"Running", true },
        { L"Tag", true }, { L"UserData", true }, { L"Type", false }, { L"StartFcn", true },
        { L"StopFcn", true }, { L"TimerFcn", true }, { L"TimerPeriod", true } };

    auto it = writeableProperties.find(propertyName);
    return (it != writeableProperties.end()) ? it->second : false;
}
//=============================================================================
bool
AudiorecorderObject::setConfig(
    int SampleRate, int BitsPerSample, int NumberOfChannels, int deviceID)
{
    _SampleRate = SampleRate;
    _BitsPerSample = BitsPerSample;
    _NumberOfChannels = NumberOfChannels;
    _DeviceID = deviceID;
    // TotalSamples/CurrentSample remain zero until actual recording implemented
    _TotalSamples = 0;
    _CurrentSample = 0;
    // configure input stream parameters for PortAudio
    int dev = deviceID;
    if (dev < 0)
        dev = Pa_GetDefaultInputDevice();
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(dev);
    if (pdi) {
        inputStreamParameters.device = dev;
        inputStreamParameters.channelCount
            = (_NumberOfChannels > 0) ? _NumberOfChannels : static_cast<int>(pdi->maxInputChannels);
        // choose sample format
        if (_BitsPerSample == 32)
            inputStreamParameters.sampleFormat = paFloat32;
        else if (_BitsPerSample == 16)
            inputStreamParameters.sampleFormat = paInt16;
        else if (_BitsPerSample == 8)
            inputStreamParameters.sampleFormat = paInt8;
        else
            inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = pdi->defaultLowInputLatency;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    } else {
        inputStreamParameters.device = paNoDevice;
        inputStreamParameters.channelCount = _NumberOfChannels > 0 ? _NumberOfChannels : 1;
        inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = 0;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    }
    paStream = nullptr;
    recordedFrames = 0;
    recordBuffer.clear();
    return true;
}
//=============================================================================
static std::wstring
FunctionToWideString(const ArrayOf& _data)
{
    if (_data.isEmpty()) {
        return L"''";
    } else if (_data.isFunctionHandle()) {
        function_handle fh = _data.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* anonymousFunction
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        return utf8_to_wstring(anonymousFunction->getDefinition());
    } else if (_data.isCell()) {
        Dimensions dims = _data.getDimensions();
        return L"{" + dims.toWideString() + L" cell}";
    } else if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        std::wstring callbackString
            = std::wstring(L"'") + _data.getContentAsWideCharactersPointer() + std::wstring(L"'");
        return callbackString;
    }
    return L"";
}
//=============================================================================
bool
AudiorecorderObject::disp(Interface* io)
{
    if (io) {
        std::wstring valueToDisp;
        io->outputMessage(L"\n");
        valueToDisp = std::to_wstring(_SampleRate);
        io->outputMessage(L"\tSampleRate: \t" + valueToDisp + L"\n");
        valueToDisp = std::to_wstring(_BitsPerSample);
        io->outputMessage(L"\tBitsPerSample: \t" + valueToDisp + L"\n");
        valueToDisp = std::to_wstring(_NumberOfChannels);
        io->outputMessage(L"\tNumberOfChannels: \t" + valueToDisp + L"\n");
        valueToDisp = std::to_wstring(_DeviceID);
        io->outputMessage(L"\tDeviceID: \t" + valueToDisp + L"\n");
        valueToDisp = std::to_wstring(_CurrentSample);
        io->outputMessage(L"\tCurrentSample: \t" + valueToDisp + L"\n");
        valueToDisp = std::to_wstring(_TotalSamples);
        io->outputMessage(L"\tTotalSamples: \t" + valueToDisp + L"\n");
        if (_Running) {
            valueToDisp = L"on";
        } else {
            valueToDisp = L"off";
        }
        io->outputMessage(L"\tRunning: \t" + valueToDisp + L"\n");
        valueToDisp = _Tag;
        io->outputMessage(L"\tTag: \t'" + valueToDisp + L"'\n");
        if (_UserData.isEmpty(true)) {
            valueToDisp = L"[]";
        } else {
            Dimensions dimsUserData = _UserData.getDimensions();
            std::string userDataClassName;
            ClassName(_UserData, userDataClassName);
            valueToDisp
                = utf8_to_wstring("[" + userDataClassName + "] - size: " + dimsUserData.toString());
        }
        valueToDisp = FunctionToWideString(startFunc);
        io->outputMessage(L"\tstartFcn: \t" + valueToDisp + L"\n");

        valueToDisp = FunctionToWideString(stopFunc);
        io->outputMessage(L"\tstopFcn: \t" + valueToDisp + L"\n");

        valueToDisp = FunctionToWideString(timerFunc);
        io->outputMessage(L"\ttimerFcn: \t" + valueToDisp + L"\n");

        valueToDisp = std::to_wstring(timerPeriodSeconds);
        io->outputMessage(L"\ttimerFcn: \t" + valueToDisp + L"\n");

        if (_UserData.isEmpty(true)) {
            valueToDisp = L"[]";
        } else {
            Dimensions dimsUserData = _UserData.getDimensions();
            std::string userDataClassName;
            ClassName(_UserData, userDataClassName);
            valueToDisp
                = utf8_to_wstring("[" + userDataClassName + "] - size: " + dimsUserData.toString());
        }
        io->outputMessage(L"\tUserData: \t" + valueToDisp + L"\n");
        valueToDisp = utf8_to_wstring(_Type);
        io->outputMessage(L"\tType: \t'" + valueToDisp + L"'\n");
        return true;
    }
    return false;
}
//=============================================================================
wstringVector
AudiorecorderObject::fieldnames()
{
    return propertiesNames;
}
//=============================================================================
bool
AudiorecorderObject::isProperty(const std::wstring& propertyName)
{
    auto it = std::find(propertiesNames.begin(), propertiesNames.end(), propertyName);
    return (it != propertiesNames.end());
}
//=============================================================================
bool
AudiorecorderObject::isMethod(const std::wstring& propertyName)
{
    auto it = std::find(methodsNames.begin(), methodsNames.end(), propertyName);
    return (it != methodsNames.end());
}
//=============================================================================
wstringVector
AudiorecorderObject::getProperties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
AudiorecorderObject::getMethods()
{
    return methodsNames;
}
//=============================================================================
int
AudiorecorderObject::nargoutMethod(const std::wstring& methodName)
{
    std::map<std::wstring, int> methodNargout
        = { { L"delete", 0 }, { L"get", -1 }, { L"getaudiodata", 1 }, { L"isrecording", 1 },
              { L"pause", 0 }, { L"play", 0 }, { L"record", 0 }, { L"recordblocking", 0 },
              { L"resume", 0 }, { L"set", 0 }, { L"stop", 0 } };
    if (methodNargout.find(methodName) == methodNargout.end()) {
        return -1;
    }
    return methodNargout[methodName];
}
//=============================================================================
bool
AudiorecorderObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    if (propertyName == L"SampleRate") {
        res = ArrayOf::doubleConstructor(_SampleRate);
        return true;
    }
    if (propertyName == L"BitsPerSample") {
        res = ArrayOf::doubleConstructor(_BitsPerSample);
        return true;
    }
    if (propertyName == L"NumberOfChannels") {
        res = ArrayOf::doubleConstructor(_NumberOfChannels);
        return true;
    }
    if (propertyName == L"DeviceID") {
        res = ArrayOf::doubleConstructor(_DeviceID);
        return true;
    }
    if (propertyName == L"CurrentSample") {
        res = ArrayOf::doubleConstructor(_CurrentSample);
        return true;
    }
    if (propertyName == L"TotalSamples") {
        res = ArrayOf::doubleConstructor(_TotalSamples);
        return true;
    }
    if (propertyName == L"Running") {
        if (_Running) {
            res = ArrayOf::characterArrayConstructor(L"on");
        } else {
            res = ArrayOf::characterArrayConstructor(L"off");
        }
        return true;
    }
    if (propertyName == L"Tag") {
        res = ArrayOf::characterArrayConstructor(_Tag);
        return true;
    }
    if (propertyName == L"UserData") {
        res = _UserData;
        return true;
    }
    if (propertyName == L"Type") {
        res = ArrayOf::characterArrayConstructor(_Type);
        return true;
    }
    return false;
}
//=============================================================================
bool
AudiorecorderObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    if (propertyName == L"StartFcn") {
        if (propertyValue.isFunctionHandle() || propertyValue.isScalarStringArray()
            || propertyValue.isRowVectorCharacterArray()) {
            startFunc = propertyValue;
            return true;
        }
        return false;
    }
    if (propertyName == L"StopFcn") {
        if (propertyValue.isFunctionHandle() || propertyValue.isScalarStringArray()
            || propertyValue.isRowVectorCharacterArray()) {
            stopFunc = propertyValue;
            return true;
        }
        return false;
    }
    if (propertyName == L"TimerFcn") {
        if (propertyValue.isFunctionHandle() || propertyValue.isScalarStringArray()
            || propertyValue.isRowVectorCharacterArray()) {
            timerFunc = propertyValue;
            return true;
        }
        return false;
    }
    if (propertyName == L"TimerPeriod") {
        if (propertyValue.isScalar() && propertyValue.isDoubleType(true)) {
            double doubleValue = propertyValue.getContentAsDoubleScalar();
            if (doubleValue <= 0.002 || !std::isfinite(doubleValue)) {
                return false;
            }
            timerPeriodSeconds = doubleValue;
            return true;
        }
        return false;
    }

    if (propertyName == L"Tag") {
        std::wstring value = propertyValue.getContentAsWideString();
        _Tag = value;
        return true;
    }
    if (propertyName == L"UserData") {
        _UserData = propertyValue;
        _UserData.ensureSingleOwner();
        return true;
    }
    if (propertyName == L"Running") {
        if (propertyValue.isLogical() && propertyValue.isScalar()) {
            _Running = propertyValue.getContentAsLogicalScalar();
        } else if (propertyValue.isScalarStringArray()
            || propertyValue.isRowVectorCharacterArray()) {
            std::wstring val = propertyValue.getContentAsWideString();
            if ((val == L"on") || (val == L"off")) {
                if ((val == L"on")) {
                    _Running = true;
                } else {
                    _Running = false;
                }
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
    errorMessage = _W("Property not found:") + propertyName;
    return false;
}
//=============================================================================
static void
TriggerTimerIfNeeded(AudiorecorderObject* data)
{
    if (!data)
        return;

    if (data->getTimerFcn().isEmpty())
        return;

    if (!(data->getTimerFcn().isFunctionHandle() || data->getTimerFcn().isScalarStringArray()
            || data->getTimerFcn().isRowVectorCharacterArray()))
        return;

    if (!(std::isfinite(data->getTimerPeriodSeconds()) && data->getTimerPeriodSeconds() > 0.0))
        return;

    const int sampleRate = data->getSampleRate();
    if (sampleRate <= 0)
        return;

    // compute period in frames (at least 1)
    uint64_t periodFrames = static_cast<uint64_t>(
        std::ceil(data->getTimerPeriodSeconds() * static_cast<double>(sampleRate)));
    if (periodFrames == 0)
        periodFrames = 1;

    // static structures to track last fired frame per instance
    static std::unordered_map<AudiorecorderObject*, uint64_t> lastFiredMap;
    static std::mutex lastFiredMutex;

    uint64_t currentFrames = static_cast<uint64_t>(data->getRecordedFrames());

    std::lock_guard<std::mutex> lk(lastFiredMutex);
    uint64_t lastFired = 0;
    auto it = lastFiredMap.find(data);
    if (it != lastFiredMap.end()) {
        lastFired = it->second;
    }

    if (currentFrames >= lastFired + periodFrames) {
        // update last fired frame
        lastFiredMap[data] = currentFrames;

        // schedule timerFunc similarly to other callbacks in this file
        if (data->getTimerFcn().isScalarStringArray()
            || data->getTimerFcn().isRowVectorCharacterArray()) {
            TimerQueue::getInstance()->add(TimerCallback(data->getTimerFcn()));
        } else if (data->getTimerFcn().isFunctionHandle()) {
            size_t nbElements = 3;
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
            elements[0] = data->getTimerFcn();
            elements[1] = ArrayOf::handleConstructor(data);
            elements[2] = ArrayOf::emptyConstructor(0, 0);
            ArrayOf callbackAsArrayOf
                = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
            TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
        }
    }
}
//=============================================================================
int
AudiorecorderObject::paRecordCallback(const void* inputBuffer, void* outputBuffer,
    unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags, void* userData)
{
    auto* data = static_cast<AudiorecorderObject*>(userData);
    if (!data) {
        return paContinue;
    }
    if (!inputBuffer) {
        // push zeros
        for (unsigned long i = 0; i < framesPerBuffer * (unsigned long)data->_NumberOfChannels;
             ++i) {
            data->recordBuffer.push_back(0.0f);
        }
        data->recordedFrames += static_cast<uint32_t>(framesPerBuffer);
        data->_TotalSamples = static_cast<int>(data->recordedFrames);
        // check timed target after updating samples
        if (data->timedRecording.load() && data->timedTargetFrames.load() > 0
            && static_cast<uint64_t>(data->recordedFrames) >= data->timedTargetFrames.load()) {
            data->timedRecording.store(false);
            data->_Running = false;
            if (!data->stopFunc.isEmpty()) {
                if (data->stopFunc.isScalarStringArray()
                    || data->stopFunc.isRowVectorCharacterArray()) {
                    TimerQueue::getInstance()->add(TimerCallback(data->stopFunc));
                } else if (data->stopFunc.isFunctionHandle()) {
                    size_t nbElements = 3;
                    ArrayOf* elements
                        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
                    elements[0] = data->stopFunc;
                    elements[1] = ArrayOf::handleConstructor(data);
                    elements[2] = ArrayOf::emptyConstructor(0, 0);
                    ArrayOf callbackAsArrayOf
                        = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
                    TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
                }
            }
            return paComplete;
        }

        // trigger timer function if configured and period elapsed
        TriggerTimerIfNeeded(data);

        return paContinue;
    }
    if (data->inputStreamParameters.sampleFormat == paFloat32) {
        const float* in = static_cast<const float*>(inputBuffer);
        for (unsigned long i = 0; i < framesPerBuffer * (unsigned long)data->_NumberOfChannels;
             ++i) {
            data->recordBuffer.push_back(static_cast<single>(in[i]));
        }
    } else if (data->inputStreamParameters.sampleFormat == paInt16) {
        const int16* in = static_cast<const int16*>(inputBuffer);
        for (unsigned long i = 0; i < framesPerBuffer * (unsigned long)data->_NumberOfChannels;
             ++i) {
            data->recordBuffer.push_back(static_cast<single>(in[i]) / 32768.0f);
        }
    } else if (data->inputStreamParameters.sampleFormat == paInt8) {
        const int8* in = static_cast<const int8*>(inputBuffer);
        for (unsigned long i = 0; i < framesPerBuffer * (unsigned long)data->_NumberOfChannels;
             ++i) {
            data->recordBuffer.push_back(static_cast<single>(in[i]) / 128.0f);
        }
    } else if (data->inputStreamParameters.sampleFormat == paUInt8) {
        const uint8* in = static_cast<const uint8*>(inputBuffer);
        for (unsigned long i = 0; i < framesPerBuffer * (unsigned long)data->_NumberOfChannels;
             ++i) {
            data->recordBuffer.push_back((static_cast<single>(in[i]) - 128.0f) / 128.0f);
        }
    }
    data->recordedFrames += static_cast<uint32_t>(framesPerBuffer);
    data->_TotalSamples = static_cast<int>(data->recordedFrames);
    data->_Running = true;

    // If a timed non-blocking recording is active and we reached or exceeded the target, end the
    // stream.
    if (data->timedRecording.load() && data->timedTargetFrames.load() > 0
        && static_cast<uint64_t>(data->recordedFrames) >= data->timedTargetFrames.load()) {
        data->timedRecording.store(false);
        data->_Running = false;
        if (!data->stopFunc.isEmpty()) {
            if (data->stopFunc.isScalarStringArray()
                || data->stopFunc.isRowVectorCharacterArray()) {
                TimerQueue::getInstance()->add(TimerCallback(data->stopFunc));
            } else if (data->stopFunc.isFunctionHandle()) {
                size_t nbElements = 3;
                ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
                elements[0] = data->stopFunc;
                elements[1] = ArrayOf::handleConstructor(data);
                elements[2] = ArrayOf::emptyConstructor(0, 0);
                ArrayOf callbackAsArrayOf
                    = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
                TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
            }
        }
        return paComplete;
    }

    // trigger timer function if configured and period elapsed
    TriggerTimerIfNeeded(data);

    return paContinue;
}
//=============================================================================
bool
AudiorecorderObject::record(double durationSeconds)
{
    if (std::isnan(durationSeconds)) {
        // no duration specified: behave like start() (non-blocking)
        timedRecording.store(false);
        timedTargetFrames.store(0);
        return start();
    }

    // non-blocking timed recording:
    // compute target frames (if duration > 0) and set atomics, then start and return immediately.
    if (durationSeconds <= 0.0) {
        // treat <= 0 as "start and return" (no automatic stop)
        timedRecording.store(false);
        timedTargetFrames.store(0);
        return start();
    }

    const int sampleRate = this->getSampleRate();
    uint64_t targetFrames
        = static_cast<uint64_t>(std::ceil(durationSeconds * static_cast<double>(sampleRate)));

    // configure timed recording state before starting stream
    timedTargetFrames.store(targetFrames);
    timedRecording.store(true);

    // start the stream (non-blocking). If start fails, clear timed flags.
    if (!start()) {
        timedRecording.store(false);
        timedTargetFrames.store(0);
        return false;
    }

    // return immediately — the callback will stop the stream when target reached
    return true;
}
//=============================================================================
bool
AudiorecorderObject::start()
{
    if (!startFunc.isEmpty()) {
        if (startFunc.isScalarStringArray() || startFunc.isRowVectorCharacterArray()) {
            TimerQueue::getInstance()->add(TimerCallback(startFunc));
        } else if (startFunc.isFunctionHandle()) {
            size_t nbElements = 3;
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
            elements[0] = startFunc;
            elements[1] = ArrayOf::handleConstructor(this);
            elements[2] = ArrayOf::emptyConstructor(0, 0);
            ArrayOf callbackAsArrayOf
                = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
            TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
        }
    }

    if (paStream) {
        if (!Pa_IsStreamStopped(paStream)) {
            _Running = true;
            return true;
        }
    }
    PaError err = Pa_OpenStream(&paStream, &inputStreamParameters, nullptr, _SampleRate, 1024,
        paNoFlag, AudiorecorderObject::paRecordCallback, this);
    if (err != paNoError) {
        paStream = nullptr;
        return false;
    }
    recordBuffer.clear();
    recordedFrames = 0;
    err = Pa_StartStream(paStream);
    if (err == paNoError) {
        _Running = true;
        return true;
    }
    return false;
}
//=============================================================================
bool
AudiorecorderObject::pause()
{
    if (paStream) {
        PaError err = Pa_StopStream(paStream);
        _Running = false;
        if (!stopFunc.isEmpty()) {
            if (stopFunc.isScalarStringArray() || stopFunc.isRowVectorCharacterArray()) {
                TimerQueue::getInstance()->add(TimerCallback(stopFunc));
            } else if (stopFunc.isFunctionHandle()) {
                size_t nbElements = 3;
                ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
                elements[0] = stopFunc;
                elements[1] = ArrayOf::handleConstructor(this);
                elements[2] = ArrayOf::emptyConstructor(0, 0);
                ArrayOf callbackAsArrayOf
                    = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
                TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
            }
        }
        return (err == paNoError);
    }
    return false;
}
//=============================================================================
bool
AudiorecorderObject::resume()
{
    if (paStream) {
        PaError err = Pa_StartStream(paStream);
        _Running = true;
        return (err == paNoError);
    }
    return false;
}
//=============================================================================
bool
AudiorecorderObject::stop()
{
    _Running = false;
    if (!stopFunc.isEmpty()) {
        if (stopFunc.isScalarStringArray() || stopFunc.isRowVectorCharacterArray()) {
            TimerQueue::getInstance()->add(TimerCallback(stopFunc));
        } else if (stopFunc.isFunctionHandle()) {
            size_t nbElements = 3;
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
            elements[0] = stopFunc;
            elements[1] = ArrayOf::handleConstructor(this);
            elements[2] = ArrayOf::emptyConstructor(0, 0);
            ArrayOf callbackAsArrayOf
                = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
            TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
        }
    }

    if (paStream) {
        if (!Pa_IsStreamStopped(paStream)) {
            PaError err = Pa_AbortStream(paStream);
            if (err == paNoError) {
                err = Pa_CloseStream(paStream);
                paStream = nullptr;
                return (err == paNoError);
            }
            return false;
        }
        paStream = nullptr;
    }
    return true;
}
//=============================================================================
int
AudiorecorderObject::getTotalSamples()
{
    return _TotalSamples;
}
//=============================================================================
bool
AudiorecorderObject::getRunning()
{
    return _Running;
}
//=============================================================================
PaStream*
AudiorecorderObject::getStream()
{
    return paStream;
}
//=============================================================================
ArrayOf
AudiorecorderObject::getRecordedData(NelsonType destinationType)
{
    if (_TotalSamples <= 0) {
        return ArrayOf::emptyConstructor();
    }

    const int rows = _TotalSamples;
    const int cols = _NumberOfChannels;

    // helper lambdas for clamping
    auto clampLong = [](long v, long lo, long hi) -> long {
        if (v < lo)
            return lo;
        if (v > hi)
            return hi;
        return v;
    };

    switch (destinationType) {
    case NLS_DOUBLE: {
        ArrayOf out = ArrayOf::doubleMatrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0)
            return out;
        void* dstptr = out.getReadWriteDataPointer();
        double* dst = static_cast<double*>(dstptr);
        for (int s = 0; s < rows; ++s) {
            for (int c = 0; c < cols; ++c) {
                size_t srcIndex = static_cast<size_t>(s) * static_cast<size_t>(cols) + c;
                size_t dstIndex
                    = static_cast<size_t>(s) + static_cast<size_t>(c) * static_cast<size_t>(rows);
                if (srcIndex < recordBuffer.size()) {
                    dst[dstIndex] = static_cast<double>(recordBuffer[srcIndex]);
                } else {
                    dst[dstIndex] = 0.0;
                }
            }
        }
        return out;
    }
    case NLS_SINGLE: {
        ArrayOf out = ArrayOf::singleMatrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0)
            return out;
        void* dstptr = out.getReadWriteDataPointer();
        single* dst = static_cast<single*>(dstptr);
        for (int s = 0; s < rows; ++s) {
            for (int c = 0; c < cols; ++c) {
                size_t srcIndex = static_cast<size_t>(s) * static_cast<size_t>(cols) + c;
                size_t dstIndex
                    = static_cast<size_t>(s) + static_cast<size_t>(c) * static_cast<size_t>(rows);
                if (srcIndex < recordBuffer.size()) {
                    dst[dstIndex] = recordBuffer[srcIndex];
                } else {
                    dst[dstIndex] = 0.0f;
                }
            }
        }
        return out;
    }
    case NLS_INT16: {
        ArrayOf out = ArrayOf::int16Matrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0)
            return out;
        void* dstptr = out.getReadWriteDataPointer();
        int16* dst = static_cast<int16*>(dstptr);
        for (int s = 0; s < rows; ++s) {
            for (int c = 0; c < cols; ++c) {
                size_t srcIndex = static_cast<size_t>(s) * static_cast<size_t>(cols) + c;
                size_t dstIndex
                    = static_cast<size_t>(s) + static_cast<size_t>(c) * static_cast<size_t>(rows);
                if (srcIndex < recordBuffer.size()) {
                    float v = recordBuffer[srcIndex];
                    long iv = static_cast<long>(std::round(v * 32767.0f));
                    iv = clampLong(iv, -32768, 32767);
                    dst[dstIndex] = static_cast<int16>(iv);
                } else {
                    dst[dstIndex] = 0;
                }
            }
        }
        return out;
    }
    case NLS_INT8: {
        ArrayOf out = ArrayOf::int8Matrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0)
            return out;
        void* dstptr = out.getReadWriteDataPointer();
        int8* dst = static_cast<int8*>(dstptr);
        for (int s = 0; s < rows; ++s) {
            for (int c = 0; c < cols; ++c) {
                size_t srcIndex = static_cast<size_t>(s) * static_cast<size_t>(cols) + c;
                size_t dstIndex
                    = static_cast<size_t>(s) + static_cast<size_t>(c) * static_cast<size_t>(rows);
                if (srcIndex < recordBuffer.size()) {
                    float v = recordBuffer[srcIndex];
                    long iv = static_cast<long>(std::round(v * 127.0f));
                    iv = clampLong(iv, -128, 127);
                    dst[dstIndex] = static_cast<int8>(iv);
                } else {
                    dst[dstIndex] = 0;
                }
            }
        }
        return out;
    }
    case NLS_UINT8: {
        ArrayOf out = ArrayOf::uint8Matrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0)
            return out;
        void* dstptr = out.getReadWriteDataPointer();
        uint8* dst = static_cast<uint8*>(dstptr);
        for (int s = 0; s < rows; ++s) {
            for (int c = 0; c < cols; ++c) {
                size_t srcIndex = static_cast<size_t>(s) * static_cast<size_t>(cols) + c;
                size_t dstIndex
                    = static_cast<size_t>(s) + static_cast<size_t>(c) * static_cast<size_t>(rows);
                if (srcIndex < recordBuffer.size()) {
                    float v = recordBuffer[srcIndex];
                    long iv = static_cast<long>(std::round(v * 127.0f + 128.0f));
                    iv = clampLong(iv, 0, 255);
                    dst[dstIndex] = static_cast<uint8>(iv);
                } else {
                    dst[dstIndex] = 128;
                }
            }
        }
        return out;
    }
    default: {
        // fallback to single precision if unknown type
        ArrayOf out = ArrayOf::singleMatrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0)
            return out;
        void* dstptr = out.getReadWriteDataPointer();
        single* dst = static_cast<single*>(dstptr);
        for (int s = 0; s < rows; ++s) {
            for (int c = 0; c < cols; ++c) {
                size_t srcIndex = static_cast<size_t>(s) * static_cast<size_t>(cols) + c;
                size_t dstIndex
                    = static_cast<size_t>(s) + static_cast<size_t>(c) * static_cast<size_t>(rows);
                if (srcIndex < recordBuffer.size()) {
                    dst[dstIndex] = recordBuffer[srcIndex];
                } else {
                    dst[dstIndex] = 0.0f;
                }
            }
        }
        return out;
    }
    } // switch
}
//=============================================================================
bool
AudiorecorderObject::invokeMethod(Interface* io, const ArrayOfVector& args, int nLhs,
    const std::string& methodName, ArrayOfVector& results)
{
    // Build a local dispatch map that captures this, io, args and nLhs.
    // Each entry takes the results vector by reference and returns success/failure.
    std::map<std::string, std::function<bool(ArrayOfVector&)>> localMap;

    // helper lambdas
    localMap["get"] = [this, &io, &args, nLhs](ArrayOfVector& out) -> bool {
        if (args.size() < 1)
            return false;
        std::wstring prop = args[0].getContentAsWideString();
        ArrayOf res;
        if (this->get(prop, res)) {
            out.push_back(res);
            return true;
        }
        return false;
    };

    localMap["set"] = [this, &io, &args, nLhs](ArrayOfVector& out) -> bool {
        if (args.size() < 2)
            return false;
        std::wstring errorMessage;
        if (this->set(args[0].getContentAsWideString(), args[1], errorMessage)) {
            if (!errorMessage.empty() && io) {
                io->outputMessage(errorMessage + L"\n");
            }
            return true;
        }
        return false;
    };

    localMap["getaudiodata"] = [this, &args](ArrayOfVector& out) -> bool {
        NelsonType destinationType = NLS_DOUBLE;
        if (args.size() > 0) {
            std::wstring dataType = args[0].getContentAsWideString();
            if (dataType == L"double") {
                destinationType = NLS_DOUBLE;
            } else if (dataType == L"single") {
                destinationType = NLS_SINGLE;
            } else if (dataType == L"int8") {
                destinationType = NLS_INT8;
            } else if (dataType == L"uint8") {
                destinationType = NLS_UINT8;
            } else if (dataType == L"int16") {
                destinationType = NLS_INT16;
            } else {
                return false;
            }
        }
        out << this->getRecordedData(destinationType);
        return true;
    };

    localMap["isrecording"] = [this](ArrayOfVector& out) -> bool {
        out << ArrayOf::logicalConstructor(this->getRunning());
        return true;
    };

    localMap["pause"] = [this](ArrayOfVector& out) -> bool { return this->pause(); };

    localMap["resume"] = [this](ArrayOfVector& out) -> bool { return this->resume(); };

    localMap["stop"] = [this](ArrayOfVector& out) -> bool { return this->stop(); };

    localMap["record"] = [this, &args](ArrayOfVector& out) -> bool {
        double duration = std::nan("");
        if (args.size() == 0) {
            duration = std::nan("");
        } else if (args.size() == 1 && args[0].isHandle()) {
            duration = std::nan("");
        } else if (args.size() == 2 && args[0].isHandle()) {
            duration = args[1].getContentAsDoubleScalar();
        } else if (args.size() == 1 && !args[0].isHandle()) {
            duration = args[0].getContentAsDoubleScalar();
        }
        if (args.size() == 2 && args[0].isHandle()) {
            duration = args[1].getContentAsDoubleScalar();
        } else if (args.size() == 1 && !args[0].isHandle()) {
            duration = args[0].getContentAsDoubleScalar();
        }
        // negative durations treated as 0 by record() callers elsewhere; here forward as-is
        return this->record(duration);
    };

    localMap["recordblocking"] = [this, &args](ArrayOfVector& out) -> bool {
        // parse duration from first argument (if provided)
        double duration = 0.0;
        if (args.size() == 2 && args[0].isHandle()) {
            duration = args[1].getContentAsDoubleScalar();

        } else if (args.size() == 1 && !args[0].isHandle()) {
            duration = args[0].getContentAsDoubleScalar();
        }
        if (duration < 0.0) {
            duration = 0.0;
        }

        int sampleRate = this->getSampleRate();
        uint64_t targetFrames
            = static_cast<uint64_t>(std::ceil(duration * static_cast<double>(sampleRate)));
        uint64_t durationMs = static_cast<uint64_t>(std::ceil(duration * 1000.0));

        // start recording
        if (!this->start())
            return false;

        // if a positive duration requested, wait until reached or timeout
        auto t0 = std::chrono::steady_clock::now();
        if (duration > 0.0) {
            while (NelsonConfiguration::getInstance()->getInterruptPending(-1) == false) {
                // allow the host to process events
                ProcessEventsDynamicFunctionWithoutWait();

                // stop when reached target frames
                if (static_cast<uint64_t>(this->getTotalSamples()) >= targetFrames) {
                    break;
                }

                // stop when elapsed time >= requested duration
                auto now = std::chrono::steady_clock::now();
                auto elapsedMs
                    = std::chrono::duration_cast<std::chrono::milliseconds>(now - t0).count();
                if (static_cast<uint64_t>(elapsedMs) >= durationMs) {
                    break;
                }

                // yield/sleep briefly to avoid busy spin
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            }
        } else {
            // duration == 0.0: minimal implementation records briefly and returns.
            // (Recording until external interruption isn't implemented here.)
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }

        this->stop();
        return true;
    };

    // replace the old blocking "play" with a non-blocking version and add "playblocking"
    localMap["play"] = [this](ArrayOfVector& out) -> bool {
        // non-blocking playback: copy buffer and spawn a detached thread to perform playback
        if (_TotalSamples <= 0 || recordBuffer.empty())
            return false;

        // copy necessary state to avoid races with recording
        const int sampleRate = _SampleRate;
        const int channels = _NumberOfChannels > 0 ? _NumberOfChannels : 1;
        std::vector<single> bufferCopy = recordBuffer; // copy interleaved float samples

        std::thread playbackThread(
            [bufferCopy = std::move(bufferCopy), sampleRate, channels]() mutable {
                PaStream* outStream = nullptr;
                PaStreamParameters outputParams;
                int defaultOut = Pa_GetDefaultOutputDevice();
                if (defaultOut == paNoDevice)
                    return; // nothing we can do

                const PaDeviceInfo* pdi = Pa_GetDeviceInfo(defaultOut);
                if (!pdi)
                    return;

                outputParams.device = defaultOut;
                outputParams.channelCount = channels;
                outputParams.sampleFormat = paFloat32;
                outputParams.suggestedLatency = pdi->defaultLowOutputLatency;
                outputParams.hostApiSpecificStreamInfo = nullptr;

                PaError err = Pa_OpenStream(&outStream, nullptr, &outputParams, sampleRate, 1024,
                    paNoFlag, nullptr, nullptr);
                if (err != paNoError || !outStream) {
                    if (outStream) {
                        Pa_CloseStream(outStream);
                        outStream = nullptr;
                    }
                    return;
                }

                err = Pa_StartStream(outStream);
                if (err != paNoError) {
                    Pa_CloseStream(outStream);
                    outStream = nullptr;
                    return;
                }

                const int framesTotal = static_cast<int>(bufferCopy.size()) / channels;
                const int maxFramesPerWrite = 1024;
                int framesWritten = 0;
                const float* src = reinterpret_cast<const float*>(bufferCopy.data());

                while (framesWritten < framesTotal) {
                    int framesToWrite = std::min(maxFramesPerWrite, framesTotal - framesWritten);
                    const void* writePtr
                        = src + static_cast<size_t>(framesWritten) * static_cast<size_t>(channels);
                    err = Pa_WriteStream(outStream, writePtr, framesToWrite);
                    if (err != paNoError) {
                        Pa_AbortStream(outStream);
                        Pa_CloseStream(outStream);
                        outStream = nullptr;
                        return;
                    }
                    framesWritten += framesToWrite;
                }

                // wait for playback to finish
                while (outStream && Pa_IsStreamActive(outStream) == 1) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(10));
                }

                if (outStream) {
                    Pa_StopStream(outStream);
                    Pa_CloseStream(outStream);
                    outStream = nullptr;
                }
            });

        playbackThread.detach();
        return true;
    };

    localMap["playblocking"] = [this](ArrayOfVector& out) -> bool {
        // blocking playback: behave like previous play implementation (blocks until finished)
        if (_TotalSamples <= 0 || recordBuffer.empty())
            return false;

        // copy buffer locally to avoid races
        std::vector<single> bufferCopy = recordBuffer;
        PaStream* outStream = nullptr;
        PaStreamParameters outputParams;
        int defaultOut = Pa_GetDefaultOutputDevice();
        if (defaultOut == paNoDevice)
            return false;
        const PaDeviceInfo* pdi = Pa_GetDeviceInfo(defaultOut);
        if (!pdi)
            return false;

        outputParams.device = defaultOut;
        outputParams.channelCount = _NumberOfChannels > 0 ? _NumberOfChannels : 1;
        outputParams.sampleFormat = paFloat32; // recorded as float
        outputParams.suggestedLatency = pdi->defaultLowOutputLatency;
        outputParams.hostApiSpecificStreamInfo = nullptr;

        PaError err = Pa_OpenStream(
            &outStream, nullptr, &outputParams, _SampleRate, 1024, paNoFlag, nullptr, nullptr);
        if (err != paNoError || !outStream) {
            if (outStream) {
                Pa_CloseStream(outStream);
                outStream = nullptr;
            }
            return false;
        }

        err = Pa_StartStream(outStream);
        if (err != paNoError) {
            Pa_CloseStream(outStream);
            outStream = nullptr;
            return false;
        }

        // write frames in chunks
        const int framesTotal = static_cast<int>(bufferCopy.size()) / outputParams.channelCount;
        const int channels = outputParams.channelCount;
        const int maxFramesPerWrite = 1024;
        int framesWritten = 0;

        const float* src = reinterpret_cast<const float*>(bufferCopy.data());

        while (framesWritten < framesTotal) {
            int framesToWrite = std::min(maxFramesPerWrite, framesTotal - framesWritten);
            const void* writePtr
                = src + static_cast<size_t>(framesWritten) * static_cast<size_t>(channels);
            err = Pa_WriteStream(outStream, writePtr, framesToWrite);
            if (err != paNoError) {
                Pa_AbortStream(outStream);
                Pa_CloseStream(outStream);
                outStream = nullptr;
                return false;
            }
            framesWritten += framesToWrite;
        }

        // block until the stream is no longer active (playback finished)
        while (outStream && Pa_IsStreamActive(outStream) == 1) {
            // allow host event processing while blocking
            ProcessEventsDynamicFunctionWithoutWait();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }

        // close stream
        if (outStream) {
            Pa_StopStream(outStream);
            Pa_CloseStream(outStream);
            outStream = nullptr;
        }
        return true;
    };

    // find and execute
    auto it = localMap.find(methodName);
    if (it != localMap.end()) {
        return it->second(results);
    }

    // fallback: no such method
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
