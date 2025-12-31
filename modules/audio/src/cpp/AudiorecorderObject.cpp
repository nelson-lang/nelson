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
#include "AudiorecorderObject.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "EventCallback.hpp"
#include "EventQueue.hpp"
//=============================================================================
namespace Nelson {
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
    , inputStreamParameters {} // Initialisation de inputStreamParameters
    , audioPlayerObject {} // Initialisation de audioPlayerObject
{
    propertiesNames = { L"SampleRate", L"BitsPerSample", L"NumChannels", L"DeviceID",
        L"CurrentSample", L"TotalSamples", L"Running", L"StartFcn", L"StopFcn", L"TimerFcn",
        L"TimerPeriod", L"Tag", L"UserData", L"Type" };

    methodsNames = { L"getplayer", L"isrecording", L"play", L"recordblocking", L"getaudiodata",
        L"pause", L"record", L"resume", L"stop" };

    paStream = nullptr;
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
        _UserData = ArrayOf::emptyConstructor();
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
        _UserData = ArrayOf::emptyConstructor();
        _Type = NLS_HANDLE_AUDIORECORDER_CATEGORY_STR;
        // initialize input stream parameters
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
    stop();
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
AudiorecorderObject::isWriteableProperty(const std::wstring& propertyName)
{
    static const std::map<std::wstring, bool> writeableProperties = { { L"SampleRate", true },
        { L"BitsPerSample", false }, { L"NumChannels", false }, { L"DeviceID", false },
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
    if (dev < 0) {
        dev = Pa_GetDefaultInputDevice();
    }
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(dev);
    if (pdi) {
        inputStreamParameters.device = dev;
        inputStreamParameters.channelCount
            = (_NumberOfChannels > 0) ? _NumberOfChannels : static_cast<int>(pdi->maxInputChannels);
        // choose sample format
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
    if (!io) {
        return false;
    }

    struct DisplayItem
    {
        const wchar_t* label;
        std::function<std::wstring()> getter;
    };

    std::vector<DisplayItem> items = {
        { L"SampleRate", [this] { return std::to_wstring(getSampleRate()); } },
        { L"BitsPerSample", [this] { return std::to_wstring(getBitsPerSample()); } },
        { L"NumberOfChannels", [this] { return std::to_wstring(getNumberOfChannels()); } },
        { L"DeviceID", [this] { return std::to_wstring(getDeviceID()); } },
        { L"CurrentSample", [this] { return std::to_wstring(_CurrentSample); } },
        { L"TotalSamples", [this] { return std::to_wstring(getTotalSamples()); } },
        { L"Running", [this] { return getRunning() ? L"on" : L"off"; } },
        { L"StartFcn", [this] { return FunctionToWideString(startFunc); } },
        { L"StopFcn", [this] { return FunctionToWideString(stopFunc); } },
        { L"TimerFcn", [this] { return FunctionToWideString(timerFunc); } },
        { L"TimerPeriod", [this] { return std::to_wstring(timerPeriodSeconds); } },
        { L"Tag", [this] { return L"'" + _Tag + L"'"; } },
        { L"UserData",
            [this] {
                if (_UserData.isEmpty(true)) {
                    return L"[]";
                }
                Dimensions dimsUserData = _UserData.getDimensions();
                std::string userDataClassName;
                ClassName(_UserData, userDataClassName);
                const std::string input
                    = "[" + userDataClassName + "] - size: " + dimsUserData.toString();
                std::wstring temp = utf8_to_wstring(input);
                return temp.c_str();
            } },
        { L"Type", [this] { return L"'" + utf8_to_wstring(_Type) + L"'"; } },
    };

    io->outputMessage(L"\n");
    for (const auto& item : items) {
        std::wstring value = item.getter();
        io->outputMessage(L"\t" + std::wstring(item.label) + L": \t" + value + L"\n");
    }
    return true;
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
bool
AudiorecorderObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    using Getter = std::function<ArrayOf()>;

    static const std::unordered_map<std::wstring, Getter> getters = {
        { L"SampleRate", [&]() { return ArrayOf::doubleConstructor(_SampleRate); } },
        { L"BitsPerSample", [&]() { return ArrayOf::doubleConstructor(_BitsPerSample); } },
        { L"NumChannels", [&]() { return ArrayOf::doubleConstructor(_NumberOfChannels); } },
        { L"DeviceID", [&]() { return ArrayOf::doubleConstructor(_DeviceID); } },
        { L"CurrentSample", [&]() { return ArrayOf::doubleConstructor(_CurrentSample); } },
        { L"TotalSamples", [&]() { return ArrayOf::doubleConstructor(_TotalSamples); } },
        { L"Running",
            [&]() { return ArrayOf::characterArrayConstructor(_Running ? L"on" : L"off"); } },
        { L"StartFcn", [&]() { return getStartFcn(); } },
        { L"StopFcn", [&]() { return getStopFcn(); } },
        { L"TimerFcn", [&]() { return getTimerFcn(); } },
        { L"TimerPeriod", [&]() { return ArrayOf::doubleConstructor(timerPeriodSeconds); } },
        { L"Tag", [&]() { return ArrayOf::characterArrayConstructor(_Tag); } },
        { L"UserData", [&]() { return _UserData; } },
        { L"Type", [&]() { return ArrayOf::characterArrayConstructor(_Type); } },
    };

    auto it = getters.find(propertyName);
    if (it == getters.end()) {
        return false;
    }
    res = it->second();
    return true;
}
//=============================================================================
bool
AudiorecorderObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    if (!isWriteableProperty(propertyName)) {
        if (isProperty(propertyName)) {
            errorMessage = _W("Property is not writeable:") + propertyName;
        } else {
            errorMessage = _W("Property not found:") + propertyName;
        }
        return false;
    }

    using Setter = std::function<bool()>;
    static const std::unordered_map<std::wstring, Setter> setters = {
        { L"StartFcn",
            [&]() {
                if (!mainEvaluatorHasEventsLoop) {
                    errorMessage = _W("Cannot set StartFcn without event loop.");
                    return false;
                }
                if (!isFunctionHandleCallbackValid(propertyValue)) {
                    errorMessage = _W("StartFcn must be a function handle.");
                    return false;
                }
                startFunc = propertyValue;
                startFunc.ensureSingleOwner();
                return true;
            } },
        { L"StopFcn",
            [&]() {
                if (!mainEvaluatorHasEventsLoop) {
                    errorMessage = _W("Cannot set StopFcn without event loop.");
                    return false;
                }
                if (!isFunctionHandleCallbackValid(propertyValue)) {
                    errorMessage = _W("StopFcn must be a function handle.");
                    return false;
                }
                stopFunc = propertyValue;
                stopFunc.ensureSingleOwner();
                return true;
            } },
        { L"TimerFcn",
            [&]() {
                if (!mainEvaluatorHasEventsLoop) {
                    errorMessage = _W("Cannot set TimerFcn without event loop.");
                    return false;
                }
                if (!isFunctionHandleCallbackValid(propertyValue)) {
                    errorMessage = _W("TimerFcn must be a function handle.");
                    return false;
                }
                timerFunc = propertyValue;
                timerFunc.ensureSingleOwner();
                return true;
            } },
        { L"TimerPeriod",
            [&]() {
                if (propertyValue.isScalar() && propertyValue.isDoubleType(true)) {
                    double doubleValue = propertyValue.getContentAsDoubleScalar();
                    if (doubleValue <= 0.002 || !std::isfinite(doubleValue)) {
                        return false;
                    }
                    timerPeriodSeconds = doubleValue;
                    return true;
                }
                return false;
            } },
        { L"Tag",
            [&]() {
                if (propertyValue.isRowVectorCharacterArray()
                    || propertyValue.isScalarStringArray()) {
                    _Tag = propertyValue.getContentAsWideString();
                    return true;
                }
                errorMessage = _W("Tag must be a character array or string.");
                return false;
            } },
        { L"UserData",
            [&]() {
                _UserData = propertyValue;
                return true;
            } },

    };
    auto it = setters.find(propertyName);
    if (it != setters.end()) {
        return it->second();
    }
    errorMessage = _W("Property not found:") + propertyName;

    return false;
}
//=============================================================================
static void
TriggerTimerIfNeeded(AudiorecorderObject* data)
{
    if (!data) {
        return;
    }

    if (data->getTimerFcn().isEmpty()) {
        return;
    }

    if (!data->isFunctionHandleCallbackValid(data->getTimerFcn())) {
        return;
    }

    if (!(std::isfinite(data->getTimerPeriodSeconds()) && data->getTimerPeriodSeconds() > 0.0)) {
        return;
    }

    const int sampleRate = data->getSampleRate();
    if (sampleRate <= 0) {
        return;
    }

    // compute period in frames (at least 1)
    uint64_t periodFrames = static_cast<uint64_t>(
        std::ceil(data->getTimerPeriodSeconds() * static_cast<double>(sampleRate)));
    if (periodFrames == 0) {
        periodFrames = 1;
    }

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
        data->enqueueCallback(data->getTimerFcn());
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
            data->stop();

            return paComplete;
        }

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
        data->stop();
        return paComplete;
    }
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
    if (paStream) {
        if (!Pa_IsStreamStopped(paStream)) {
            _Running = true;
            return true;
        }
    }
    enqueueCallback(startFunc);

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
        return (err == paNoError);
    }
    return false;
}
//=============================================================================
bool
AudiorecorderObject::resume()
{
    if (!paStream) {
        start();
    }

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
    enqueueCallback(stopFunc);
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
        if (v < lo) {
            return lo;
        }
        if (v > hi) {
            return hi;
        }
        return v;
    };

    switch (destinationType) {
    case NLS_DOUBLE: {
        ArrayOf out = ArrayOf::doubleMatrix2dConstructor(rows, cols);
        if (out.getElementCount() == 0) {
            return out;
        }
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
        if (out.getElementCount() == 0) {
            return out;
        }
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
        if (out.getElementCount() == 0) {
            return out;
        }
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
        if (out.getElementCount() == 0) {
            return out;
        }
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
        if (out.getElementCount() == 0) {
            return out;
        }
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
        if (out.getElementCount() == 0) {
            return out;
        }
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
    }
    return {};
}
//=============================================================================
ArrayOf
AudiorecorderObject::getPlayer()
{
    std::wstring errorMessage;
    audioPlayerUsed = true;
    audioPlayerObject->setSamples(getRecordedData(NLS_DOUBLE), getSampleRate(), errorMessage);
    return ArrayOf::handleConstructor(audioPlayerObject);
}
//=============================================================================
bool
AudiorecorderObject::isFunctionHandleCallbackValid(const ArrayOf& callbackArrayOf)
{
    if (callbackArrayOf.isScalarStringArray() || callbackArrayOf.isRowVectorCharacterArray()) {
        return true;
    }
    if (callbackArrayOf.isFunctionHandle()) {
        return true;
    }
    return false;
}
//=============================================================================
void
AudiorecorderObject::enqueueCallback(const ArrayOf& callbackArrayOf)
{
    if (!callbackArrayOf.isEmpty()) {
        if (callbackArrayOf.isScalarStringArray() || callbackArrayOf.isRowVectorCharacterArray()) {
            EventQueue::getInstance()->add(EventCallback(callbackArrayOf));
        } else if (callbackArrayOf.isFunctionHandle()) {
            size_t nbElements = 3;
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
            elements[0] = callbackArrayOf;
            elements[1] = ArrayOf::handleConstructor(this);
            elements[2] = ArrayOf::emptyConstructor(0, 0);
            EventQueue::getInstance()->add(
                EventCallback(ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements)));
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
