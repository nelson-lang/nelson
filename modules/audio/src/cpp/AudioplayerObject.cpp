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
#include "AudioplayerObject.hpp"
#include "AudioDevInfo.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassName.hpp"
#include "ComplexTranspose.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
AudioplayerObject::AudioplayerObject()
    : HandleGenericObject(std::wstring(AUDIOPLAYER_CATEGORY_STR), this, false)
{
    propertiesNames = { L"SampleRate", L"BitsPerSample", L"NumberOfChannels", L"DeviceID",
        L"CurrentSample", L"TotalSamples", L"Running", L"Tag", L"UserData", L"Type" };
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
        _Tag = L"";
        _UserData = ArrayOf::emptyConstructor();
        _Type = AUDIOPLAYER_CATEGORY_STR;
    }
    paStream = nullptr;
}
//=============================================================================
AudioplayerObject::~AudioplayerObject()
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
    _Tag = L"";
    _UserData = ArrayOf::emptyConstructor();
    _Type = AUDIOPLAYER_CATEGORY_STR;
    paStream = nullptr;
}
//=============================================================================
bool
AudioplayerObject::isWriteableProperty(std::wstring propertyName)
{
    if (propertyName == L"SampleRate") {
        return true;
    }
    if (propertyName == L"BitsPerSample") {
        return false;
    }
    if (propertyName == L"NumberOfChannels") {
        return false;
    }
    if (propertyName == L"DeviceID") {
        return false;
    }
    if (propertyName == L"CurrentSample") {
        return false;
    }
    if (propertyName == L"TotalSamples") {
        return false;
    }
    if (propertyName == L"Running") {
        return true;
    }
    if (propertyName == L"Tag") {
        return true;
    }
    if (propertyName == L"UserData") {
        return true;
    }
    if (propertyName == L"Type") {
        return false;
    }
    return false;
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
std::wstring
AudioplayerObject::getType()
{
    return _Type;
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
AudioplayerObject::setTag(std::wstring tag)
{
    _Tag = tag;
    return true;
}
//=============================================================================
bool
AudioplayerObject::setUserData(Nelson::ArrayOf userData)
{
    _UserData = userData;
    _UserData.ensureSingleOwner();
    return true;
}
//=============================================================================
bool
AudioplayerObject::disp(Evaluator* eval)
{
    if (eval != nullptr) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring valueToDisp = L"";
            io->outputMessage(L"\n");
            valueToDisp = std::to_wstring(getSampleRate());
            io->outputMessage(L"\tSampleRate: \t" + valueToDisp + L"\n");
            valueToDisp = std::to_wstring(getBitsPerSample());
            io->outputMessage(L"\tBitsPerSample: \t" + valueToDisp + L"\n");
            valueToDisp = std::to_wstring(getNumberOfChannels());
            io->outputMessage(L"\tNumberOfChannels: \t" + valueToDisp + L"\n");
            valueToDisp = std::to_wstring(getDeviceID());
            io->outputMessage(L"\tDeviceID: \t" + valueToDisp + L"\n");
            valueToDisp = std::to_wstring(getCurrentSample());
            io->outputMessage(L"\tCurrentSample: \t" + valueToDisp + L"\n");
            valueToDisp = std::to_wstring(getTotalSamples());
            io->outputMessage(L"\tTotalSamples: \t" + valueToDisp + L"\n");
            if (getRunning()) {
                valueToDisp = L"on";
            } else {
                valueToDisp = L"off";
            }
            io->outputMessage(L"\tRunning: \t" + valueToDisp + L"\n");
            valueToDisp = getTag();
            io->outputMessage(L"\tTag: \t'" + valueToDisp + L"'\n");
            if (_UserData.isEmpty(true)) {
                valueToDisp = L"[]";
            } else {
                Dimensions dimsUserData = _UserData.getDimensions();
                std::string userDataClassName;
                ClassName(_UserData, userDataClassName);
                valueToDisp = utf8_to_wstring(
                    "[" + userDataClassName + "] - size: " + dimsUserData.toString());
            }
            io->outputMessage(L"\tUserData: \t" + valueToDisp + L"\n");
            valueToDisp = getType();
            io->outputMessage(L"\tType: \t'" + valueToDisp + L"'\n");
            io->outputMessage(L"\n");
            return true;
        }
    }
    return false;
}
//=============================================================================
wstringVector
AudioplayerObject::fieldnames()
{
    return propertiesNames;
}
//=============================================================================
bool
AudioplayerObject::isProperty(std::wstring propertyName)
{
    auto it = std::find(propertiesNames.begin(), propertiesNames.end(), propertyName);
    return (it != propertiesNames.end());
}
//=============================================================================
bool
AudioplayerObject::isMethod(std::wstring propertyName)
{
    return false;
}
//=============================================================================
bool
AudioplayerObject::get(std::wstring propertyName, ArrayOf& res)
{
    if (propertyName == L"SampleRate") {
        res = ArrayOf::doubleConstructor(getSampleRate());
        return true;
    }
    if (propertyName == L"BitsPerSample") {
        res = ArrayOf::doubleConstructor(getBitsPerSample());
        return true;
    }
    if (propertyName == L"NumberOfChannels") {
        res = ArrayOf::doubleConstructor(getNumberOfChannels());
        return true;
    }
    if (propertyName == L"DeviceID") {
        res = ArrayOf::doubleConstructor(getDeviceID());
        return true;
    }
    if (propertyName == L"CurrentSample") {
        res = ArrayOf::doubleConstructor(getCurrentSample());
        return true;
    }
    if (propertyName == L"TotalSamples") {
        res = ArrayOf::doubleConstructor(getTotalSamples());
        return true;
    }
    if (propertyName == L"Running") {
        if (getRunning()) {
            res = ArrayOf::characterArrayConstructor(L"on");
        } else {
            res = ArrayOf::characterArrayConstructor(L"off");
        }
        return true;
    }
    if (propertyName == L"Tag") {
        res = ArrayOf::characterArrayConstructor(getTag());
        return true;
    }
    if (propertyName == L"UserData") {
        res = getUserData();
        return true;
    }
    if (propertyName == L"Type") {
        res = ArrayOf::characterArrayConstructor(getType());
        return true;
    }
    return false;
}
//=============================================================================
bool
AudioplayerObject::set(std::wstring propertyName, ArrayOf propertyValue, std::wstring& errorMessage)
{
    if (propertyName == L"SampleRate") {
        return true;
    }
    if (propertyName == L"BitsPerSample") {
        return true;
    }
    if (propertyName == L"NumberOfChannels") {
        return true;
    }
    if (propertyName == L"DeviceID") {
        return true;
    }
    if (propertyName == L"CurrentSample") {
        return true;
    }
    if (propertyName == L"TotalSamples") {
        return true;
    }
    if (propertyName == L"Tag") {
        std::wstring value = propertyValue.getContentAsWideString();
        setTag(value);
        return true;
    }
    if (propertyName == L"UserData") {
        setUserData(propertyValue);
        return true;
    }
    if (propertyName == L"Type") {
        return true;
    }
    return false;
}
//=============================================================================
bool
AudioplayerObject::setSamples(ArrayOf data, int SampleRate, std::wstring& errorMessage)
{
    return setSamples(data, SampleRate, 16, -1, errorMessage);
}
//=============================================================================
bool
AudioplayerObject::setSamples(
    ArrayOf data, int SampleRate, int BitsPerSample, std::wstring& errorMessage)
{
    return setSamples(data, SampleRate, BitsPerSample, -1, errorMessage);
}
//=============================================================================
bool
AudioplayerObject::setSamples(
    ArrayOf data, int SampleRate, int BitsPerSample, int deviceID, std::wstring& errorMessage)
{
    errorMessage = L"";
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
        indexType rows = data.getDimensions().getRows();
        indexType columns = data.getDimensions().getColumns();
        if (columns > rows) {
            bool needToOverload = false;
            audioData = ComplexTranspose(data, needToOverload);
        } else {
            audioData = data;
        }
        rows = audioData.getDimensions().getRows();
        columns = audioData.getDimensions().getColumns();
        _NumberOfChannels = (int)columns;
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
        _TotalSamples = (int)(audioData.getDimensions().getElementCount() / _NumberOfChannels);
        outputStreamParameters.suggestedLatency
            = Pa_GetDeviceInfo(outputStreamParameters.device)->defaultLowOutputLatency;
        outputStreamParameters.hostApiSpecificStreamInfo = NULL;
        PaError err = Pa_IsFormatSupported(0, &(outputStreamParameters), SampleRate);
        if (err != paNoError) {
            const char* errorText = Pa_GetErrorText(err);
            errorMessage = utf8_to_wstring(errorText);
            return false;
        }
        return true;
    } else {
        errorMessage = _W("Wrong device ID.");
    }
    return false;
}
//=============================================================================
int
AudioplayerObject::paPlayCallback(const void* inputBuffer, void* outputBuffer,
    unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags, void* userData)
{
    AudioplayerObject* data = (AudioplayerObject*)userData;
    data->_Running = true;
    single* outAsSingle = (single*)outputBuffer;
    int8* outAsInt8 = (int8*)outputBuffer;
    uint8* outAsUInt8 = (uint8*)outputBuffer;
    int16* outAsInt16 = (int16*)outputBuffer;
    Class dataClass = data->audioData.getDataClass();
    for (unsigned int i = 0; i < framesPerBuffer; i++) {
        for (int c = 0; c < data->_NumberOfChannels; c++) {
            if ((uint32)data->_CurrentSample < data->lastSample) {
                switch (dataClass) {
                case NLS_SINGLE: {
                    single* ptrData = (single*)data->audioData.getDataPointer();
                    single val = ptrData[data->_CurrentSample + (c * data->_TotalSamples)];
                    *outAsSingle = (single)val;
                    outAsSingle++;
                } break;
                case NLS_DOUBLE: {
                    double* ptrData = (double*)data->audioData.getDataPointer();
                    double val = ptrData[data->_CurrentSample + (c * data->_TotalSamples)];
                    *outAsSingle = (single)val;
                    outAsSingle++;
                } break;
                case NLS_INT8: {
                    int8* ptrData = (int8*)data->audioData.getDataPointer();
                    int8 val = ptrData[data->_CurrentSample + (c * data->_TotalSamples)];
                    *outAsInt8 = (int8)val;
                    outAsInt8++;
                } break;
                case NLS_UINT8: {
                    uint8* ptrData = (uint8*)data->audioData.getDataPointer();
                    uint8 val = ptrData[data->_CurrentSample + (c * data->_TotalSamples)];
                    *outAsUInt8 = (uint8)val;
                    outAsUInt8++;
                } break;
                case NLS_INT16: {
                    int16* ptrData = (int16*)data->audioData.getDataPointer();
                    int16 val = ptrData[data->_CurrentSample + (c * data->_TotalSamples)];
                    *outAsInt16 = (int16)val;
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
                }
            }
        }
        data->_CurrentSample++;
    }
    if ((uint32)data->_CurrentSample >= data->lastSample) {
        data->_CurrentSample = data->lastSample;
        data->_Running = false;
        data->paStream = nullptr;
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
    PaError err = Pa_OpenStream(
        &paStream, 0, &(outputStreamParameters), _SampleRate, 1024, paNoFlag, paPlayCallback, this);
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
            return (err == paNoError);
        }
        paStream = nullptr;
    }
    return true;
}
//=============================================================================
}
//=============================================================================
