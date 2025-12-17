//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AudioplayerObject.hpp"
#include "AudioDevInfo.hpp"
#include "ClassName.hpp"
#include "ComplexTranspose.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "TimerCallback.hpp"
#include "TimerQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
AudioplayerObject::AudioplayerObject(bool withEventLoop)
    : HandleGenericObject(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, this, false)
{
    mainEvaluatorHasEventsLoop = withEventLoop;

    firstSample = 0;
    lastSample = 0;
    outputStreamParameters.device = 0;

    propertiesNames = { L"SampleRate", L"BitsPerSample", L"NumberOfChannels", L"DeviceID",
        L"CurrentSample", L"TotalSamples", L"Running", L"Tag", L"UserData", L"Type", L"StartFcn",
        L"StopFcn", L"TimerFcn", L"TimerPeriod" };

    int defaultOutput = Pa_GetDefaultOutputDevice();
    const PaDeviceInfo* pdi = Pa_GetDeviceInfo(defaultOutput);
    if (pdi) {
        _SampleRate = (int)pdi->defaultSampleRate;
        _BitsPerSample = 0;
        _NumberOfChannels = 0;
        _DeviceID = -1;
        _CurrentSample = 0;
        _TotalSamples = 0;
        _Running = false;
        _Tag.clear();
        _UserData = ArrayOf::emptyConstructor();
        _Type = NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR;
        timerPeriodSeconds = 0.0500;
    }
    paStream = nullptr;
}
//=============================================================================
AudioplayerObject::~AudioplayerObject()
{
    stop();
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
    _UserData = ArrayOf::emptyConstructor();
    _Type = NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR;
    paStream = nullptr;
    timerPeriodSeconds = 0.;
}
//=============================================================================
bool
AudioplayerObject::isWriteableProperty(const std::wstring& propertyName)
{
    static const std::unordered_set<std::wstring> writeableProperties = { L"SampleRate", L"Running",
        L"Tag", L"UserData", L"StartFcn", L"StopFcn", L"TimerFcn", L"TimerPeriod" };
    return writeableProperties.find(propertyName) != writeableProperties.end();
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
AudioplayerObject::setRunning(bool on)
{
    _Running = on;
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
AudioplayerObject::disp(Interface* io)
{
    if (!io)
        return false;

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
        { L"CurrentSample", [this] { return std::to_wstring(getCurrentSample()); } },
        { L"TotalSamples", [this] { return std::to_wstring(getTotalSamples()); } },
        { L"Running", [this] { return getRunning() ? L"on" : L"off"; } },
        { L"StartFcn", [this] { return FunctionToWideString(getStartFcn()); } },
        { L"StopFcn", [this] { return FunctionToWideString(getStopFcn()); } },
        { L"TimerFcn", [this] { return FunctionToWideString(getTimerFcn()); } },
        { L"TimerPeriod", [this] { return std::to_wstring(timerPeriodSeconds); } },
        { L"Tag", [this] { return L"'" + getTag() + L"'"; } },
        { L"UserData",
            [this] {
                if (_UserData.isEmpty(true))
                    return L"[]";
                Dimensions dimsUserData = _UserData.getDimensions();
                std::string userDataClassName;
                ClassName(_UserData, userDataClassName);
                const std::string input
                    = "[" + userDataClassName + "] - size: " + dimsUserData.toString();
                std::wstring temp = utf8_to_wstring(input);
                return temp.c_str();
            } },
        { L"Type", [this] { return L"'" + utf8_to_wstring(getType()) + L"'"; } },
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
AudioplayerObject::getProperties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
AudioplayerObject::getMethods()
{
    return {};
}
//=============================================================================
bool
AudioplayerObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    using Getter = std::function<ArrayOf()>;
    static const std::unordered_map<std::wstring, Getter> getters = {
        { L"SampleRate", [this]() { return ArrayOf::doubleConstructor(getSampleRate()); } },
        { L"BitsPerSample", [this]() { return ArrayOf::doubleConstructor(getBitsPerSample()); } },
        { L"NumberOfChannels",
            [this]() { return ArrayOf::doubleConstructor(getNumberOfChannels()); } },
        { L"DeviceID", [this]() { return ArrayOf::doubleConstructor(getDeviceID()); } },
        { L"CurrentSample", [this]() { return ArrayOf::doubleConstructor(getCurrentSample()); } },
        { L"TotalSamples", [this]() { return ArrayOf::doubleConstructor(getTotalSamples()); } },
        { L"Running",
            [this]() {
                return getRunning() ? ArrayOf::characterArrayConstructor(L"on")
                                    : ArrayOf::characterArrayConstructor(L"off");
            } },
        { L"Tag", [this]() { return ArrayOf::characterArrayConstructor(getTag()); } },
        { L"UserData", [this]() { return getUserData(); } },
        { L"Type", [this]() { return ArrayOf::characterArrayConstructor(getType()); } },
        { L"StartFcn", [this]() { return getStartFcn(); } },
        { L"StopFcn", [this]() { return getStopFcn(); } },
        { L"TimerFcn", [this]() { return getTimerFcn(); } },
        { L"TimerPeriod", [this]() { return ArrayOf::doubleConstructor(timerPeriodSeconds); } }
    };

    auto it = getters.find(propertyName);
    if (it != getters.end()) {
        res = it->second();
        return true;
    }
    return false;
}
//=============================================================================
bool
AudioplayerObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    using Setter = std::function<bool()>;
    static const std::unordered_map<std::wstring, Setter> setters
        = { { L"SampleRate",
                [&]() {
                    return setSampleRate(
                        static_cast<int>(propertyValue.getContentAsDoubleScalar()));
                } },
              { L"Tag", [&]() { return setTag(propertyValue.getContentAsWideString()); } },
              { L"UserData", [&]() { return setUserData(propertyValue); } },
              { L"StartFcn",
                  [&]() {
                      if (!mainEvaluatorHasEventsLoop) {
                          errorMessage = _W("Cannot set StartFcn without event loop.");
                          return false;
                      }
                      if (!propertyValue.isFunctionHandle() && propertyValue.isScalarStringArray()
                          && propertyValue.isRowVectorCharacterArray()) {
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
                      if (!propertyValue.isFunctionHandle() && propertyValue.isScalarStringArray()
                          && propertyValue.isRowVectorCharacterArray()) {
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
                      if (!propertyValue.isFunctionHandle() && propertyValue.isScalarStringArray()
                          && propertyValue.isRowVectorCharacterArray()) {
                          errorMessage = _W("TimerFcn must be a function handle.");
                          return false;
                      }
                      timerFunc = propertyValue;
                      timerFunc.ensureSingleOwner();
                      return true;
                  } },
              { L"TimerPeriod", [&]() {
                   if (propertyValue.isScalar() && propertyValue.isDoubleType(true)) {
                       double doubleValue = propertyValue.getContentAsDoubleScalar();
                       if (doubleValue <= 0.002 || !std::isfinite(doubleValue)) {
                           return false;
                       }
                       timerPeriodSeconds = doubleValue;
                       return true;
                   }
                   return false;
               } } };

    auto it = setters.find(propertyName);
    if (it != setters.end()) {
        errorMessage.clear();
        return it->second();
    }

    errorMessage = _W("Property not writeable:") + propertyName;
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
        errorMessage = _W("Wrong device ID.");
        return false;
    }
    const PaDeviceInfo* pdi_output = Pa_GetDeviceInfo(outputIndex);
    if (pdi_output) {
        if (!data.isNumeric() || data.isComplex() || data.isSparse()) {
            errorMessage = ERROR_WRONG_ARGUMENT_1_TYPE;
            return false;
        }
        if (data.isEmpty()) {
            errorMessage = _W("Empty matrix not allowed.");
            return false;
        }
        if (!data.is2D() && !data.isVector()) {
            errorMessage = _W("Vector or matrix 2D expected.");
            return false;
        }
        indexType rows = data.getRows();
        indexType columns = data.getColumns();
        if (columns > rows) {
            bool needToOverload = false;
            audioData = ComplexTranspose(data, needToOverload);
        } else {
            audioData = data;
        }
        Dimensions dimsAudioData = audioData.getDimensions();
        rows = dimsAudioData.getRows();
        columns = dimsAudioData.getColumns();
        _NumberOfChannels = static_cast<int>(columns);
        if (_NumberOfChannels > pdi_output->maxOutputChannels) {
            errorMessage = _W("Too many output channels.");
            return false;
        }
        outputStreamParameters.device = outputIndex;
        outputStreamParameters.channelCount = _NumberOfChannels;
        switch (audioData.getDataClass()) {
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
        _TotalSamples = static_cast<int>(audioData.getElementCount() / _NumberOfChannels);
        outputStreamParameters.suggestedLatency
            = Pa_GetDeviceInfo(outputStreamParameters.device)->defaultLowOutputLatency;
        outputStreamParameters.hostApiSpecificStreamInfo = nullptr;
        PaError err = Pa_IsFormatSupported(nullptr, &(outputStreamParameters), SampleRate);
        if (err != paNoError) {
            const char* errorText = Pa_GetErrorText(err);
            errorMessage = utf8_to_wstring(errorText);
            return false;
        }
        return true;
    }
    errorMessage = _W("Wrong device ID.");

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

    data->_Running = true;
    auto* outAsSingle = static_cast<single*>(outputBuffer);
    int8* outAsInt8 = static_cast<int8*>(outputBuffer);
    auto* outAsUInt8 = static_cast<uint8*>(outputBuffer);
    auto* outAsInt16 = static_cast<int16*>(outputBuffer);
    NelsonType dataClass = data->audioData.getDataClass();
    for (unsigned int i = 0; i < framesPerBuffer; i++) {
        for (int c = 0; c < data->_NumberOfChannels; c++) {
            if (static_cast<uint32>(data->_CurrentSample) < data->lastSample) {
                switch (dataClass) {
                case NLS_SINGLE: {
                    auto* ptrData = (single*)data->audioData.getDataPointer();
                    size_t pos = (size_t)(data->_CurrentSample)
                        + (size_t)(c) * (size_t)(data->_TotalSamples);
                    single val = ptrData[pos];
                    *outAsSingle = val;
                    outAsSingle++;
                } break;
                case NLS_DOUBLE: {
                    auto* ptrData = (double*)data->audioData.getDataPointer();
                    size_t pos = (size_t)(data->_CurrentSample)
                        + (size_t)(c) * (size_t)(data->_TotalSamples);
                    double val = ptrData[pos];
                    *outAsSingle = static_cast<single>(val);
                    outAsSingle++;
                } break;
                case NLS_INT8: {
                    int8* ptrData = (int8*)data->audioData.getDataPointer();
                    size_t pos = (size_t)(data->_CurrentSample)
                        + (size_t)(c) * (size_t)(data->_TotalSamples);
                    int8 val = ptrData[pos];
                    *outAsInt8 = val;
                    outAsInt8++;
                } break;
                case NLS_UINT8: {
                    auto* ptrData = (uint8*)data->audioData.getDataPointer();
                    size_t pos = (size_t)(data->_CurrentSample)
                        + (size_t)(c) * (size_t)(data->_TotalSamples);
                    uint8 val = ptrData[pos];
                    *outAsUInt8 = val;
                    outAsUInt8++;
                } break;
                case NLS_INT16: {
                    auto* ptrData = (int16*)data->audioData.getDataPointer();
                    size_t pos = (size_t)(data->_CurrentSample)
                        + (size_t)(c) * (size_t)(data->_TotalSamples);
                    int16 val = ptrData[pos];
                    *outAsInt16 = val;
                    outAsInt16++;
                } break;
                default: {
                    // not managed here
                } break;
                }
            } else {
                switch (dataClass) {
                case NLS_DOUBLE:
                case NLS_SINGLE: {
                    *outAsSingle++ = 0;
                } break;
                case NLS_INT8: {
                    *outAsInt8++ = 0;
                } break;
                case NLS_UINT8: {
                    *outAsUInt8++ = 0;
                } break;
                case NLS_INT16: {
                    *outAsInt16++ = 0;
                } break;
                default: {
                } break;
                }
            }
        }

        if (timerEnabled && (data->_CurrentSample % periodSamples == 0)) {
            if (!data->timerFunc.isEmpty()) {
                if (data->timerFunc.isScalarStringArray()
                    || data->timerFunc.isRowVectorCharacterArray()) {
                    TimerQueue::getInstance()->add(TimerCallback(data->timerFunc));
                } else if (data->timerFunc.isFunctionHandle()) {
                    size_t nbElements = 3;
                    ArrayOf* elements
                        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
                    elements[0] = data->timerFunc;
                    elements[1] = ArrayOf::handleConstructor(data);
                    elements[2] = ArrayOf::emptyConstructor(0, 0);
                    ArrayOf callbackAsArrayOf
                        = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
                    TimerQueue::getInstance()->add(TimerCallback(callbackAsArrayOf));
                }
            }
        }
        data->_CurrentSample++;
    }
    if (static_cast<uint32>(data->_CurrentSample) >= data->lastSample) {
        data->_CurrentSample = data->lastSample;
        data->_Running = false;
        data->paStream = nullptr;

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
        return 1;
    }
    return 0;
}
//=============================================================================
PaStream*
AudioplayerObject::getStream()
{
    return paStream;
}
//=============================================================================
bool
AudioplayerObject::play(int start, int end)
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
    if (paStream) {
        if (Pa_IsStreamStopped(paStream) == 1) {
            stop();
        } else {
            _Running = true;
            return true;
        }
    }
    _CurrentSample = firstSample;
    PaError err = Pa_OpenStream(&paStream, nullptr, &(outputStreamParameters), _SampleRate, 1024,
        paNoFlag, paPlayCallback, this);
    if (err != paNoError) {
        paStream = nullptr;
        return false;
    }
    err = Pa_StartStream(paStream);
    _Running = true;
    return (err == paNoError);
}
//=============================================================================
bool
AudioplayerObject::pause()
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
AudioplayerObject::resume()
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
AudioplayerObject::stop()
{
    _CurrentSample = 0;
    _Running = false;
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
} // namespace Nelson
//=============================================================================
