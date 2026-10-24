//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <functional>
#include "AudioObjectDisplayHelpers.hpp"
#include "AudioplayerObject.hpp"
#include "AudioDevInfo.hpp"
#include "AudioCallbackHelpers.hpp"
#include "ClassName.hpp"
#include "ComplexTranspose.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
AudioplayerObject::AudioplayerObject(bool withEventLoop)
    : HandleGenericObject(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, this, false)
    , _SampleRate(0)
    , _BitsPerSample(0)
    , _NumberOfChannels(0)
    , _DeviceID(-1)
    , _CurrentSample(0)
    , _Running(false)
    , _TotalSamples(0)
    , timerPeriodSeconds(0.0)
{
    mainEvaluatorHasEventsLoop = withEventLoop;

    firstSample = 0;
    lastSample = 0;
    outputStreamParameters.device = 0;

    propertiesNames = { L"SampleRate", L"BitsPerSample", L"NumChannels", L"DeviceID",
        L"CurrentSample", L"TotalSamples", L"Running", L"StartFcn", L"StopFcn", L"TimerFcn",
        L"TimerPeriod", L"Tag", L"UserData", L"Type" };

    methodsNames = { L"pause", L"playblocking", L"isplaying", L"play", L"resume", L"stop" };

    int defaultOutput = Pa_GetDefaultOutputDevice();
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(defaultOutput);
    if (pdi) {
        _SampleRate = (int)pdi->defaultSampleRate;
        _Tag.clear();
        _UserData = ArrayOf::empty();
        _Type = NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR;
        timerPeriodSeconds = 0.0500;
    }
}
//=============================================================================
AudioplayerObject::~AudioplayerObject()
{
    // no StopFcn for a deleted object; closing waits for the audio thread to leave the callback
    stopFcnPending = false;
    stream.close();
    mainEvaluatorHasEventsLoop = false;
    propertiesNames.clear();
    _SampleRate = 0;
    _BitsPerSample = 0;
    _NumberOfChannels = 0;
    _DeviceID = -1;
    _CurrentSample = 0;
    _TotalSamples = 0;
    _Running = false;
    _Tag.clear();
    _UserData = ArrayOf::empty();
    _Type = NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR;
    timerPeriodSeconds = 0.;
}
//=============================================================================
bool
AudioplayerObject::isWritableProperty(const std::wstring& propertyName)
{
    static const std::unordered_set<std::wstring> writableProperties = { L"SampleRate", L"Running",
        L"Tag", L"UserData", L"StartFcn", L"StopFcn", L"TimerFcn", L"TimerPeriod" };
    return writableProperties.find(propertyName) != writableProperties.end();
}
//=============================================================================
int
AudioplayerObject::getSampleRate()
{
    return _SampleRate;
}
//=============================================================================
int
AudioplayerObject::getBitsPerSample()
{
    return _BitsPerSample;
}
//=============================================================================
int
AudioplayerObject::getNumberOfChannels()
{
    return _NumberOfChannels;
}
//=============================================================================
int
AudioplayerObject::getDeviceID()
{
    return _DeviceID;
}
//=============================================================================
int
AudioplayerObject::getCurrentSample()
{
    return _CurrentSample;
}
//=============================================================================
int
AudioplayerObject::getTotalSamples()
{
    return _TotalSamples;
}
//=============================================================================
bool
AudioplayerObject::getRunning()
{
    return _Running;
}
//=============================================================================
bool
AudioplayerObject::isStreamFinishing()
{
    return stream.isOpen() && !stream.isPaused() && !stream.isFinished();
}
//=============================================================================
std::wstring
AudioplayerObject::getTag()
{
    return _Tag;
}
//=============================================================================
Nelson::ArrayOf
AudioplayerObject::getUserData()
{
    return _UserData;
}
//=============================================================================
std::string
AudioplayerObject::getType()
{
    return _Type;
}
//=============================================================================
Nelson::ArrayOf
AudioplayerObject::getStartFcn()
{
    return startFunc;
}
//=============================================================================
Nelson::ArrayOf
AudioplayerObject::getStopFcn()
{
    return stopFunc;
}
//=============================================================================
Nelson::ArrayOf
AudioplayerObject::getTimerFcn()
{
    return timerFunc;
}
//=============================================================================
bool
AudioplayerObject::setSampleRate(int sr)
{
    _SampleRate = sr;
    return true;
}
//=============================================================================
bool
AudioplayerObject::setTag(const std::wstring& tag)
{
    _Tag = tag;
    return true;
}
//=============================================================================
bool
AudioplayerObject::setUserData(const Nelson::ArrayOf& userData)
{
    _UserData = userData;
    _UserData.ensureSingleOwner();
    return true;
}
//=============================================================================
bool
AudioplayerObject::disp(Interface* io)
{
    if (!io) {
        return false;
    }
    releaseFinishedStream();

    std::wstring userData;
    if (_UserData.isEmpty(true)) {
        userData = L"[]";
    } else {
        Dimensions dimsUserData = _UserData.dimensions();
        std::string userDataClassName;
        ClassName(_UserData, userDataClassName);
        userData
            = utf8_to_wstring("[" + userDataClassName + "] - size: " + dimsUserData.toString());
    }

    std::vector<AudioDisplayItem> items = {
        { L"SampleRate", std::to_wstring(getSampleRate()) },
        { L"BitsPerSample", std::to_wstring(getBitsPerSample()) },
        { L"NumChannels", std::to_wstring(getNumberOfChannels()) },
        { L"DeviceID", std::to_wstring(getDeviceID()) },
        { L"CurrentSample", std::to_wstring(getCurrentSample()) },
        { L"TotalSamples", std::to_wstring(getTotalSamples()) },
        { L"Running", getRunning() ? L"on" : L"off" },
        { L"StartFcn", audioCallbackToWideString(getStartFcn()) },
        { L"StopFcn", audioCallbackToWideString(getStopFcn()) },
        { L"TimerFcn", audioCallbackToWideString(getTimerFcn()) },
        { L"TimerPeriod", std::to_wstring(timerPeriodSeconds) },
        { L"Tag", L"'" + getTag() + L"'" },
        { L"UserData", userData },
        { L"Type", L"'" + utf8_to_wstring(getType()) + L"'" },
    };
    return displayAudioObject(io, L"audioplayer", items,
        { L"SampleRate", L"BitsPerSample", L"NumChannels", L"DeviceID", L"CurrentSample",
            L"TotalSamples", L"Running" });
}
//=============================================================================
wstringVector
AudioplayerObject::fieldnames()
{
    return propertiesNames;
}
//=============================================================================
bool
AudioplayerObject::isProperty(const std::wstring& propertyName)
{
    auto it = std::find(propertiesNames.begin(), propertiesNames.end(), propertyName);
    return (it != propertiesNames.end());
}
//=============================================================================
bool
AudioplayerObject::isMethod(const std::wstring& propertyName)
{
    return false;
}
//=============================================================================
wstringVector
AudioplayerObject::properties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
AudioplayerObject::methods()
{
    return methodsNames;
}
//=============================================================================
bool
AudioplayerObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    // static table: the lambdas receive the object, they must not capture it
    using Getter = std::function<ArrayOf(AudioplayerObject*)>;
    static const std::unordered_map<std::wstring, Getter> getters = {
        { L"SampleRate",
            [](AudioplayerObject* self) { return ArrayOf::doubleScalar(self->getSampleRate()); } },
        { L"BitsPerSample",
            [](AudioplayerObject* self) {
                return ArrayOf::doubleScalar(self->getBitsPerSample());
            } },
        { L"NumChannels",
            [](AudioplayerObject* self) {
                return ArrayOf::doubleScalar(self->getNumberOfChannels());
            } },
        { L"DeviceID",
            [](AudioplayerObject* self) { return ArrayOf::doubleScalar(self->getDeviceID()); } },
        { L"CurrentSample",
            [](AudioplayerObject* self) {
                return ArrayOf::doubleScalar(self->getCurrentSample());
            } },
        { L"TotalSamples",
            [](AudioplayerObject* self) {
                return ArrayOf::doubleScalar(self->getTotalSamples());
            } },
        { L"Running",
            [](AudioplayerObject* self) {
                return ArrayOf::characterArray(self->getRunning() ? L"on" : L"off");
            } },
        { L"Tag", [](AudioplayerObject* self) { return ArrayOf::characterArray(self->getTag()); } },
        { L"UserData", [](AudioplayerObject* self) { return self->getUserData(); } },
        { L"Type",
            [](AudioplayerObject* self) { return ArrayOf::characterArray(self->getType()); } },
        { L"StartFcn", [](AudioplayerObject* self) { return self->getStartFcn(); } },
        { L"StopFcn", [](AudioplayerObject* self) { return self->getStopFcn(); } },
        { L"TimerFcn", [](AudioplayerObject* self) { return self->getTimerFcn(); } },
        { L"TimerPeriod",
            [](AudioplayerObject* self) {
                return ArrayOf::doubleScalar(self->timerPeriodSeconds);
            } },
    };

    auto it = getters.find(propertyName);
    if (it != getters.end()) {
        releaseFinishedStream();
        res = it->second(this);
        return true;
    }
    return false;
}
//=============================================================================
bool
AudioplayerObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    // static table: the lambdas receive the object and the value, they must not capture them
    using Setter = std::function<bool(AudioplayerObject*, const ArrayOf&, std::wstring&)>;
    static const std::unordered_map<std::wstring, Setter> setters = {
        { L"SampleRate",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring&) {
                return self->setSampleRate(static_cast<int>(value.asDoubleScalar()));
            } },
        { L"Tag",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring&) {
                return self->setTag(value.asWideString());
            } },
        { L"UserData",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring&) {
                return self->setUserData(value);
            } },
        { L"StartFcn",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring& message) {
                return setAudioCallback(
                    L"StartFcn", value, self->mainEvaluatorHasEventsLoop, self->startFunc, message);
            } },
        { L"StopFcn",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring& message) {
                return setAudioCallback(
                    L"StopFcn", value, self->mainEvaluatorHasEventsLoop, self->stopFunc, message);
            } },
        { L"TimerFcn",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring& message) {
                return setAudioCallback(
                    L"TimerFcn", value, self->mainEvaluatorHasEventsLoop, self->timerFunc, message);
            } },
        { L"TimerPeriod",
            [](AudioplayerObject* self, const ArrayOf& value, std::wstring&) {
                if (!value.isScalar() || !value.isDoubleType(true)) {
                    return false;
                }
                double doubleValue = value.asDoubleScalar();
                if (doubleValue <= 0.002 || !std::isfinite(doubleValue)) {
                    return false;
                }
                self->timerPeriodSeconds = doubleValue;
                return true;
            } },
    };

    auto it = setters.find(propertyName);
    if (it != setters.end()) {
        errorMessage.clear();
        return it->second(this, propertyValue, errorMessage);
    }

    errorMessage
        = formatI18nMessage(Nelson::resolveMessageTemplate(
                                L"Nelson:audio:propertyWritable", L"Property not writable: %s."),
            propertyName);
    return false;
}
//=============================================================================
bool
AudioplayerObject::setSamples(const ArrayOf& data, int SampleRate, std::wstring& errorMessage)
{
    return setSamples(data, SampleRate, 16, -1, errorMessage);
}
//=============================================================================
bool
AudioplayerObject::setSamples(
    const ArrayOf& data, int SampleRate, int BitsPerSample, std::wstring& errorMessage)
{
    return setSamples(data, SampleRate, BitsPerSample, -1, errorMessage);
}
//=============================================================================
bool
AudioplayerObject::setSamples(const ArrayOf& data, int SampleRate, int BitsPerSample, int deviceID,
    std::wstring& errorMessage)
{
    errorMessage.clear();
    PaDeviceIndex outputIndex = getOutputDeviceIndex(deviceID);
    if (outputIndex == paNoDevice) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:audio:wrongDeviceId", L"Wrong device ID.");
        return false;
    }
    const PaDeviceInfo* pdi_output = Pa_GetDeviceInfo(outputIndex);
    if (pdi_output) {
        if (!data.isNumeric() || data.isComplex() || data.isSparse()) {
            errorMessage = Nelson::formatI18nMessage(L"Wrong type: #%d argument.", (1));
            return false;
        }
        if (data.isEmpty()) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:audio:emptyMatrixAllowed", L"Empty matrix not allowed.");
            return false;
        }
        if (!data.is2D() && !data.isVector()) {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:audio:vectorMatrix", L"Vector or matrix 2D expected.");
            return false;
        }
        indexType rows = data.rows();
        indexType columns = data.columns();
        if (columns > rows) {
            bool needToOverload = false;
            audioData = ComplexTranspose(data, needToOverload);
        } else {
            audioData = data;
        }
        Dimensions dimsAudioData = audioData.dimensions();
        rows = dimsAudioData.rows();
        columns = dimsAudioData.columns();
        _NumberOfChannels = static_cast<int>(columns);
        if (_NumberOfChannels > pdi_output->maxOutputChannels) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:audio:tooManyOutputChannels", L"Too many output channels.");
            return false;
        }
        outputStreamParameters.device = outputIndex;
        outputStreamParameters.channelCount = _NumberOfChannels;
        switch (audioData.dataClass()) {
        case NLS_DOUBLE: {
            outputStreamParameters.sampleFormat = paFloat32;
        } break;
        case NLS_SINGLE: {
            outputStreamParameters.sampleFormat = paFloat32;
        } break;
        case NLS_INT8: {
            outputStreamParameters.sampleFormat = paInt8;
        } break;
        case NLS_UINT8: {
            outputStreamParameters.sampleFormat = paUInt8;
        } break;
        case NLS_INT16: {
            outputStreamParameters.sampleFormat = paInt16;
        } break;
        default: {
            errorMessage = _W("Type not supported.");
            return false;
        } break;
        }
        _SampleRate = SampleRate;
        _BitsPerSample = BitsPerSample;
        _TotalSamples = static_cast<int>(audioData.elementCount() / _NumberOfChannels);
        outputStreamParameters.suggestedLatency
            = Pa_GetDeviceInfo(outputStreamParameters.device)->defaultLowOutputLatency;
        outputStreamParameters.hostApiSpecificStreamInfo = nullptr;
        return true;
    }
    errorMessage
        = Nelson::resolveMessageTemplate(L"Nelson:audio:wrongDeviceId", L"Wrong device ID.");

    return false;
}
//=============================================================================
int
AudioplayerObject::paPlayCallback(const void* inputBuffer, void* outputBuffer,
    unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags, void* userData)
{
    auto* data = static_cast<AudioplayerObject*>(userData);
    const bool timerEnabled
        = (!data->timerFunc.isEmpty() && data->timerPeriodSeconds > 0.0 && data->_SampleRate > 0);

    size_t periodSamples = 0;
    if (timerEnabled) {
        periodSamples = static_cast<size_t>(
            data->timerPeriodSeconds * static_cast<double>(data->_SampleRate));
        if (periodSamples == 0) {
            periodSamples = 1;
        }
    }

    auto* outAsSingle = static_cast<single*>(outputBuffer);
    int8* outAsInt8 = static_cast<int8*>(outputBuffer);
    auto* outAsUInt8 = static_cast<uint8*>(outputBuffer);
    auto* outAsInt16 = static_cast<int16*>(outputBuffer);
    NelsonType dataClass = data->audioData.dataClass();
    const void* samples = data->audioData.dataPointer();
    const size_t totalSamples = static_cast<size_t>(data->_TotalSamples);
    const uint32 lastSample = data->lastSample;
    int currentSample = data->_CurrentSample.load();
    for (unsigned long i = 0; i < framesPerBuffer; i++) {
        const bool hasSample = static_cast<uint32>(currentSample) < lastSample;
        for (int c = 0; c < data->_NumberOfChannels; c++) {
            size_t pos = static_cast<size_t>(currentSample) + static_cast<size_t>(c) * totalSamples;
            switch (dataClass) {
            case NLS_SINGLE: {
                *outAsSingle++ = hasSample ? static_cast<const single*>(samples)[pos] : 0.f;
            } break;
            case NLS_DOUBLE: {
                *outAsSingle++ = hasSample
                    ? static_cast<single>(static_cast<const double*>(samples)[pos])
                    : 0.f;
            } break;
            case NLS_INT8: {
                *outAsInt8++ = hasSample ? static_cast<const int8*>(samples)[pos] : 0;
            } break;
            case NLS_UINT8: {
                *outAsUInt8++ = hasSample ? static_cast<const uint8*>(samples)[pos] : 0;
            } break;
            case NLS_INT16: {
                *outAsInt16++ = hasSample ? static_cast<const int16*>(samples)[pos] : 0;
            } break;
            default: {
                // not managed here
            } break;
            }
        }
        if (timerEnabled && (currentSample % periodSamples == 0)) {
            enqueueAudioCallback(data->timerFunc, data);
        }
        currentSample++;
    }
    if (static_cast<uint32>(currentSample) >= lastSample) {
        data->_CurrentSample = static_cast<int>(lastSample);
        data->_Running = false;
        data->enqueueStopFcnOnce();
        return paComplete;
    }
    data->_CurrentSample = currentSample;
    return paContinue;
}
//=============================================================================
PaError
AudioplayerObject::play(int start, int end)
{
    releaseFinishedStream();
    firstSample = start;
    if (end == 0) {
        lastSample = _TotalSamples;
    } else {
        if (end > _TotalSamples) {
            lastSample = _TotalSamples;
        } else {
            lastSample = end;
        }
    }
    if (_Running) {
        return paNoError;
    }
    enqueueAudioCallback(startFunc, this);
    _CurrentSample = firstSample;
    _Running = true;
    stopFcnPending = true;
    PaError err = stream.open(nullptr, &outputStreamParameters, _SampleRate, paPlayCallback, this);
    if (err != paNoError) {
        _Running = false;
        stopFcnPending = false;
    }
    return err;
}
//=============================================================================
bool
AudioplayerObject::pause()
{
    releaseFinishedStream();
    if (!stream.isOpen()) {
        return false;
    }
    _Running = false;
    return stream.pause() == paNoError;
}
//=============================================================================
PaError
AudioplayerObject::resume()
{
    releaseFinishedStream();
    if (!stream.isPaused()) {
        return paNoError;
    }
    _Running = true;
    PaError err = stream.resume();
    if (err != paNoError) {
        _Running = false;
    }
    return err;
}
//=============================================================================
bool
AudioplayerObject::stop()
{
    _Running = false;
    // closing waits for the audio thread, which cannot overwrite CurrentSample afterwards
    stream.close();
    _CurrentSample = 0;
    enqueueStopFcnOnce();
    return true;
}
//=============================================================================
void
AudioplayerObject::enqueueStopFcnOnce()
{
    if (stopFcnPending.exchange(false)) {
        enqueueAudioCallback(stopFunc, this);
    }
}
//=============================================================================
void
AudioplayerObject::releaseFinishedStream()
{
    stream.closeIfFinished();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
