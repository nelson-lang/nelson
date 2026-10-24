//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include <functional>
#include <unordered_map>
#include "AudioObjectDisplayHelpers.hpp"
#include "AudiorecorderObject.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "AudioCallbackHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T, typename Convert>
static ArrayOf
interleavedToColumns(const std::vector<single>& buffer, int rows, int cols, NelsonType type,
    T missing, Convert convert);
//=============================================================================
AudiorecorderObject::AudiorecorderObject(bool withEventLoop)
    : HandleGenericObject(NLS_HANDLE_AUDIORECORDER_CATEGORY_STR, this, true)
    , _SampleRate(8000)
    , _BitsPerSample(8)
    , _NumberOfChannels(1)
    , _DeviceID(-1)
    , _CurrentSample(0)
    , _TotalSamples(0)
    , _Running(false)
    , timerPeriodSeconds(0.0500)
    , mainEvaluatorHasEventsLoop(withEventLoop)
    , inputStreamParameters {}
    , audioPlayerObject {}
{
    propertiesNames = { L"SampleRate", L"BitsPerSample", L"NumChannels", L"DeviceID",
        L"CurrentSample", L"TotalSamples", L"Running", L"StartFcn", L"StopFcn", L"TimerFcn",
        L"TimerPeriod", L"Tag", L"UserData", L"Type" };

    methodsNames = { L"getplayer", L"isrecording", L"play", L"recordblocking", L"getaudiodata",
        L"pause", L"record", L"resume", L"stop" };

    recordedFrames = 0;
    recordBuffer.clear();
    recordedDataClass = NLS_SINGLE;

    auto setDefaults = [this]() {
        _SampleRate = 8000;
        _BitsPerSample = 8;
        _NumberOfChannels = 1;
        _DeviceID = -1;
        _CurrentSample = 0;
        _TotalSamples = 0;
        _Running = false;
        _Tag.clear();
        _UserData = ArrayOf::empty();
        _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;
        inputStreamParameters.device = paNoDevice;
        inputStreamParameters.channelCount = 1;
        inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = 0;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    };

    int defaultInput = Pa_GetDefaultInputDevice();
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(defaultInput);
    if (pdi) {
        _SampleRate = 8000;
        _BitsPerSample = 8;
        _NumberOfChannels = 1;
        _DeviceID = defaultInput;
        _CurrentSample = 0;
        _TotalSamples = 0;
        _Running = false;
        _Tag.clear();
        _UserData = ArrayOf::empty();
        _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;
        inputStreamParameters.device = _DeviceID;
        inputStreamParameters.channelCount = _NumberOfChannels;
        inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = pdi->defaultLowInputLatency;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    } else {
        setDefaults();
    }

    audioPlayerObject = new AudioplayerObject(mainEvaluatorHasEventsLoop);
    audioPlayerUsed = false;
}
//=============================================================================
AudiorecorderObject::~AudiorecorderObject()
{
    // no StopFcn for a deleted object; closing waits for the audio thread to leave the callback
    stopFcnPending = false;
    stream.close();
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
    _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;
    if (audioPlayerObject && !audioPlayerUsed) {
        delete audioPlayerObject;
        audioPlayerObject = nullptr;
    }
    audioPlayerUsed = false;
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
AudiorecorderObject::isWritableProperty(const std::wstring& propertyName)
{
    static const std::map<std::wstring, bool> writableProperties = { { L"SampleRate", true },
        { L"BitsPerSample", false }, { L"NumChannels", false }, { L"DeviceID", false },
        { L"CurrentSample", false }, { L"TotalSamples", false }, { L"Running", true },
        { L"Tag", true }, { L"UserData", true }, { L"Type", false }, { L"StartFcn", true },
        { L"StopFcn", true }, { L"TimerFcn", true }, { L"TimerPeriod", true } };

    auto it = writableProperties.find(propertyName);
    return (it != writableProperties.end()) ? it->second : false;
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
    _TotalSamples = 0;
    _CurrentSample = 0;
    int dev = deviceID;
    if (dev < 0) {
        dev = Pa_GetDefaultInputDevice();
    }
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(dev);
    if (pdi) {
        inputStreamParameters.device = dev;
        inputStreamParameters.channelCount
            = (_NumberOfChannels > 0) ? _NumberOfChannels : static_cast<int>(pdi->maxInputChannels);
        if (_BitsPerSample == 32) {
            inputStreamParameters.sampleFormat = paFloat32;
        } else if (_BitsPerSample == 16) {
            inputStreamParameters.sampleFormat = paInt16;
        } else if (_BitsPerSample == 8) {
            inputStreamParameters.sampleFormat = paInt8;
        } else {
            inputStreamParameters.sampleFormat = paFloat32;
        }
        inputStreamParameters.suggestedLatency = pdi->defaultLowInputLatency;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    } else {
        inputStreamParameters.device = paNoDevice;
        inputStreamParameters.channelCount = _NumberOfChannels > 0 ? _NumberOfChannels : 1;
        inputStreamParameters.sampleFormat = paFloat32;
        inputStreamParameters.suggestedLatency = 0;
        inputStreamParameters.hostApiSpecificStreamInfo = nullptr;
    }
    stream.close();
    recordedFrames = 0;
    recordBuffer.clear();
    return true;
}
//=============================================================================
bool
AudiorecorderObject::disp(Interface* io)
{
    if (!io) {
        return false;
    }
    stream.closeIfFinished();

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
        { L"CurrentSample", std::to_wstring(_CurrentSample) },
        { L"TotalSamples", std::to_wstring(getTotalSamples()) },
        { L"Running", getRunning() ? L"on" : L"off" },
        { L"StartFcn", audioCallbackToWideString(startFunc) },
        { L"StopFcn", audioCallbackToWideString(stopFunc) },
        { L"TimerFcn", audioCallbackToWideString(timerFunc) },
        { L"TimerPeriod", std::to_wstring(timerPeriodSeconds) },
        { L"Tag", L"'" + _Tag + L"'" },
        { L"UserData", userData },
        { L"Type", L"'" + utf8_to_wstring(_Type) + L"'" },
    };
    return displayAudioObject(io, L"audiorecorder", items,
        { L"SampleRate", L"BitsPerSample", L"NumChannels", L"DeviceID", L"CurrentSample",
            L"TotalSamples", L"Running" });
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
AudiorecorderObject::properties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
AudiorecorderObject::methods()
{
    return methodsNames;
}
//=============================================================================
bool
AudiorecorderObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    // static table: the lambdas receive the object, they must not capture it
    using Getter = std::function<ArrayOf(AudiorecorderObject*)>;
    static const std::unordered_map<std::wstring, Getter> getters = {
        { L"SampleRate",
            [](AudiorecorderObject* self) { return ArrayOf::doubleScalar(self->_SampleRate); } },
        { L"BitsPerSample",
            [](AudiorecorderObject* self) { return ArrayOf::doubleScalar(self->_BitsPerSample); } },
        { L"NumChannels",
            [](AudiorecorderObject* self) {
                return ArrayOf::doubleScalar(self->_NumberOfChannels);
            } },
        { L"DeviceID",
            [](AudiorecorderObject* self) { return ArrayOf::doubleScalar(self->_DeviceID); } },
        { L"CurrentSample",
            [](AudiorecorderObject* self) { return ArrayOf::doubleScalar(self->_CurrentSample); } },
        { L"TotalSamples",
            [](AudiorecorderObject* self) {
                return ArrayOf::doubleScalar(self->getTotalSamples());
            } },
        { L"Running",
            [](AudiorecorderObject* self) {
                return ArrayOf::characterArray(self->getRunning() ? L"on" : L"off");
            } },
        { L"StartFcn", [](AudiorecorderObject* self) { return self->startFunc; } },
        { L"StopFcn", [](AudiorecorderObject* self) { return self->stopFunc; } },
        { L"TimerFcn", [](AudiorecorderObject* self) { return self->timerFunc; } },
        { L"TimerPeriod",
            [](AudiorecorderObject* self) {
                return ArrayOf::doubleScalar(self->timerPeriodSeconds);
            } },
        { L"Tag", [](AudiorecorderObject* self) { return ArrayOf::characterArray(self->_Tag); } },
        { L"UserData", [](AudiorecorderObject* self) { return self->_UserData; } },
        { L"Type", [](AudiorecorderObject* self) { return ArrayOf::characterArray(self->_Type); } },
    };

    auto it = getters.find(propertyName);
    if (it == getters.end()) {
        return false;
    }
    stream.closeIfFinished();
    res = it->second(this);
    return true;
}
//=============================================================================
bool
AudiorecorderObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    if (!isWritableProperty(propertyName)) {
        if (isProperty(propertyName)) {
            errorMessage
                = formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:audio:propertyWritable",
                                        L"Property not writable: %s."),
                    propertyName);
        } else {
            errorMessage = formatI18nMessage(
                resolveMessageTemplate(L"Nelson:audio:propertyFound", L"Property not found: %s."),
                propertyName);
        }
        return false;
    }

    // static table: the lambdas receive the object and the value, they must not capture them
    using Setter = std::function<bool(AudiorecorderObject*, const ArrayOf&, std::wstring&)>;
    static const std::unordered_map<std::wstring, Setter> setters = {
        { L"StartFcn",
            [](AudiorecorderObject* self, const ArrayOf& value, std::wstring& message) {
                return setAudioCallback(
                    L"StartFcn", value, self->mainEvaluatorHasEventsLoop, self->startFunc, message);
            } },
        { L"StopFcn",
            [](AudiorecorderObject* self, const ArrayOf& value, std::wstring& message) {
                return setAudioCallback(
                    L"StopFcn", value, self->mainEvaluatorHasEventsLoop, self->stopFunc, message);
            } },
        { L"TimerFcn",
            [](AudiorecorderObject* self, const ArrayOf& value, std::wstring& message) {
                return setAudioCallback(
                    L"TimerFcn", value, self->mainEvaluatorHasEventsLoop, self->timerFunc, message);
            } },
        { L"TimerPeriod",
            [](AudiorecorderObject* self, const ArrayOf& value, std::wstring&) {
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
        { L"Tag",
            [](AudiorecorderObject* self, const ArrayOf& value, std::wstring& message) {
                if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
                    self->_Tag = value.asWideString();
                    return true;
                }
                message = formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                        L"%s must be a character vector or string scalar."),
                    L"Tag");
                return false;
            } },
        { L"UserData",
            [](AudiorecorderObject* self, const ArrayOf& value, std::wstring&) {
                self->_UserData = value;
                return true;
            } },
    };
    auto it = setters.find(propertyName);
    if (it != setters.end()) {
        return it->second(this, propertyValue, errorMessage);
    }
    errorMessage = formatI18nMessage(
        resolveMessageTemplate(L"Nelson:audio:propertyFound", L"Property not found: %s."),
        propertyName);

    return false;
}
//=============================================================================
void
AudiorecorderObject::triggerTimerIfNeeded()
{
    if (timerFunc.isEmpty() || !isValidAudioCallback(timerFunc)) {
        return;
    }
    if (!(std::isfinite(timerPeriodSeconds) && timerPeriodSeconds > 0.0) || _SampleRate <= 0) {
        return;
    }
    uint64_t periodFrames
        = static_cast<uint64_t>(std::ceil(timerPeriodSeconds * static_cast<double>(_SampleRate)));
    if (periodFrames == 0) {
        periodFrames = 1;
    }
    uint64_t currentFrames = static_cast<uint64_t>(recordedFrames.load());
    if (currentFrames >= lastTimerFrame + periodFrames) {
        lastTimerFrame = currentFrames;
        enqueueAudioCallback(timerFunc, this);
    }
}
//=============================================================================
void
AudiorecorderObject::appendFrames(const void* inputBuffer, unsigned long framesPerBuffer)
{
    const size_t count
        = static_cast<size_t>(framesPerBuffer) * static_cast<size_t>(_NumberOfChannels);
    std::lock_guard<std::mutex> lock(recordMutex);
    if (!inputBuffer) {
        recordBuffer.insert(recordBuffer.end(), count, 0.0f);
    } else if (inputStreamParameters.sampleFormat == paFloat32) {
        const float* in = static_cast<const float*>(inputBuffer);
        recordBuffer.insert(recordBuffer.end(), in, in + count);
    } else if (inputStreamParameters.sampleFormat == paInt16) {
        const int16* in = static_cast<const int16*>(inputBuffer);
        for (size_t i = 0; i < count; ++i) {
            recordBuffer.push_back(static_cast<single>(in[i]) / 32768.0f);
        }
    } else if (inputStreamParameters.sampleFormat == paInt8) {
        const int8* in = static_cast<const int8*>(inputBuffer);
        for (size_t i = 0; i < count; ++i) {
            recordBuffer.push_back(static_cast<single>(in[i]) / 128.0f);
        }
    } else if (inputStreamParameters.sampleFormat == paUInt8) {
        const uint8* in = static_cast<const uint8*>(inputBuffer);
        for (size_t i = 0; i < count; ++i) {
            recordBuffer.push_back((static_cast<single>(in[i]) - 128.0f) / 128.0f);
        }
    }
    recordedFrames += static_cast<uint32_t>(framesPerBuffer);
    _TotalSamples = static_cast<int>(recordedFrames.load());
}
//=============================================================================
int
AudiorecorderObject::paRecordCallback(const void* inputBuffer, void* outputBuffer,
    unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags, void* userData)
{
    auto* data = static_cast<AudiorecorderObject*>(userData);
    data->appendFrames(inputBuffer, framesPerBuffer);
    // a timed recording ends itself; the stream is closed later from the main thread
    if (data->timedRecording.load() && data->timedTargetFrames.load() > 0
        && static_cast<uint64_t>(data->recordedFrames.load()) >= data->timedTargetFrames.load()) {
        data->timedRecording = false;
        data->_Running = false;
        data->enqueueStopFcnOnce();
        return paComplete;
    }
    data->triggerTimerIfNeeded();
    return paContinue;
}
//=============================================================================
PaError
AudiorecorderObject::record(double durationSeconds)
{
    // no duration (NaN) or duration <= 0: start without automatic stop
    if (std::isnan(durationSeconds) || durationSeconds <= 0.0) {
        timedRecording = false;
        timedTargetFrames = 0;
        return start();
    }
    // non-blocking timed recording: the callback ends the stream when the target is reached
    timedTargetFrames
        = static_cast<uint64_t>(std::ceil(durationSeconds * static_cast<double>(getSampleRate())));
    timedRecording = true;
    PaError err = start();
    if (err != paNoError) {
        timedRecording = false;
        timedTargetFrames = 0;
    }
    return err;
}
//=============================================================================
PaError
AudiorecorderObject::start()
{
    stream.closeIfFinished();
    if (_Running) {
        return paNoError;
    }
    enqueueAudioCallback(startFunc, this);
    stream.close();
    recordBuffer.clear();
    recordedFrames = 0;
    _TotalSamples = 0;
    lastTimerFrame = 0;
    _Running = true;
    stopFcnPending = true;
    PaError err = stream.open(
        &inputStreamParameters, nullptr, _SampleRate, AudiorecorderObject::paRecordCallback, this);
    if (err != paNoError) {
        _Running = false;
        stopFcnPending = false;
    }
    return err;
}
//=============================================================================
bool
AudiorecorderObject::pause()
{
    stream.closeIfFinished();
    if (!stream.isOpen()) {
        return false;
    }
    _Running = false;
    return stream.pause() == paNoError;
}
//=============================================================================
PaError
AudiorecorderObject::resume()
{
    stream.closeIfFinished();
    if (!stream.isOpen()) {
        return start();
    }
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
AudiorecorderObject::stop()
{
    _Running = false;
    timedRecording = false;
    stream.close();
    enqueueStopFcnOnce();
    return true;
}
//=============================================================================
void
AudiorecorderObject::enqueueStopFcnOnce()
{
    if (stopFcnPending.exchange(false)) {
        enqueueAudioCallback(stopFunc, this);
    }
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
ArrayOf
AudiorecorderObject::getRecordedData(NelsonType destinationType)
{
    std::lock_guard<std::mutex> lock(recordMutex);
    const int rows = _TotalSamples;
    if (rows <= 0) {
        return ArrayOf::empty();
    }
    const int cols = _NumberOfChannels;
    auto quantize = [](single v, float scale, float offset, long lo, long hi) {
        long iv = static_cast<long>(std::round(v * scale + offset));
        return (iv < lo) ? lo : ((iv > hi) ? hi : iv);
    };
    switch (destinationType) {
    case NLS_DOUBLE: {
        return interleavedToColumns<double>(recordBuffer, rows, cols, NLS_DOUBLE, 0.,
            [](single v) { return static_cast<double>(v); });
    }
    case NLS_INT16: {
        return interleavedToColumns<int16>(recordBuffer, rows, cols, NLS_INT16, 0,
            [&](single v) { return static_cast<int16>(quantize(v, 32767.f, 0.f, -32768, 32767)); });
    }
    case NLS_INT8: {
        return interleavedToColumns<int8>(recordBuffer, rows, cols, NLS_INT8, 0,
            [&](single v) { return static_cast<int8>(quantize(v, 127.f, 0.f, -128, 127)); });
    }
    case NLS_UINT8: {
        return interleavedToColumns<uint8>(recordBuffer, rows, cols, NLS_UINT8, 128,
            [&](single v) { return static_cast<uint8>(quantize(v, 127.f, 128.f, 0, 255)); });
    }
    case NLS_SINGLE:
    default: {
        return interleavedToColumns<single>(
            recordBuffer, rows, cols, NLS_SINGLE, 0.f, [](single v) { return v; });
    }
    }
}
//=============================================================================
ArrayOf
AudiorecorderObject::getPlayer()
{
    std::wstring errorMessage;
    audioPlayerUsed = true;
    audioPlayerObject->setSamples(getRecordedData(NLS_DOUBLE), getSampleRate(), errorMessage);
    return ArrayOf::handle(audioPlayerObject);
}
//=============================================================================
// interleaved recording buffer to a rows x cols column-major matrix
template <typename T, typename Convert>
static ArrayOf
interleavedToColumns(const std::vector<single>& buffer, int rows, int cols, NelsonType type,
    T missing, Convert convert)
{
    const size_t nbRows = static_cast<size_t>(rows);
    const size_t nbCols = static_cast<size_t>(cols);
    T* dst = static_cast<T*>(ArrayOf::allocateArrayOf(type, nbRows * nbCols));
    for (size_t r = 0; r < nbRows; ++r) {
        for (size_t c = 0; c < nbCols; ++c) {
            size_t srcIndex = r * nbCols + c;
            dst[r + c * nbRows] = (srcIndex < buffer.size()) ? convert(buffer[srcIndex]) : missing;
        }
    }
    return ArrayOf(type, Dimensions(nbRows, nbCols), dst);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
