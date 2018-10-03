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
#include "AudioDevInfo.hpp"
#include "characters_encoding.hpp"
#include <portaudio.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::container::vector<AudioDeviceInfo> audioDevices;
//=============================================================================
boost::container::vector<AudioDeviceInfo>
getAudioDevices()
{
    if (audioDevices.empty()) {
        retrieveAudioDevicesInfo();
    }
    return audioDevices;
}
//=============================================================================
void
clearAudioDevicesInfo()
{
    audioDevices.clear();
}
//=============================================================================
PaDeviceIndex
getOutputDeviceIndex(int deviceID)
{
    PaDeviceIndex idx = paNoDevice;
    if (deviceID < -1) {
        idx = paNoDevice;
    } else {
        if (deviceID == -1) {
            idx = Pa_GetDefaultOutputDevice();
        } else {
            const PaDeviceInfo* pdi = nullptr;
            for (AudioDeviceInfo dev : audioDevices) {
                if (deviceID == dev.Id) {
                    pdi = dev.padeviceInfo;
                    break;
                }
            }
            if (pdi) {
                for (PaDeviceIndex i = 0; i < Pa_GetDeviceCount(); i++) {
                    const PaDeviceInfo* currentdeviceInfo = Pa_GetDeviceInfo(i);
                    if (pdi == currentdeviceInfo) {
                        return i;
                    }
                }
            }
        }
    }
    return idx;
}
//=============================================================================
PaDeviceIndex
getInputDeviceIndex(int deviceID)
{
    PaDeviceIndex idx = paNoDevice;
    if (deviceID < -1) {
        idx = paNoDevice;
    } else {
        if (deviceID == -1) {
            idx = Pa_GetDefaultInputDevice();
        } else {
            const PaDeviceInfo* pdi = nullptr;
            for (AudioDeviceInfo dev : audioDevices) {
                if (deviceID == dev.Id) {
                    pdi = dev.padeviceInfo;
                    break;
                }
            }
            if (pdi) {
                for (PaDeviceIndex i = 0; i < Pa_GetDeviceCount(); i++) {
                    const PaDeviceInfo* currentdeviceInfo = Pa_GetDeviceInfo(i);
                    if (pdi == currentdeviceInfo) {
                        return i;
                    }
                }
            }
        }
    }
    return idx;
}
//=============================================================================
void
retrieveAudioDevicesInfo()
{
    audioDevices.clear();
    int k = 0;
    for (PaDeviceIndex i = 0; i < Pa_GetDeviceCount(); i++) {
        const PaDeviceInfo* deviceInfo = Pa_GetDeviceInfo(i);
        if (deviceInfo != nullptr) {
            bool isInput = (deviceInfo->maxInputChannels != 0);
            bool isOutput = (deviceInfo->maxOutputChannels != 0);
            if (isInput || isOutput) {
                AudioDeviceInfo info;
                info.Id = k;
                info.padeviceInfo = deviceInfo;
                audioDevices.push_back(info);
                k++;
            }
        }
    }
}
//=============================================================================
static ArrayOf
getDevicesInfo(
    boost::container::vector<AudioDeviceInfo> infos, bool input, size_t nbInputs, size_t nbOutputs)
{
    Dimensions dimsInOrOutputs;
    if (input) {
        dimsInOrOutputs[0] = nbInputs;
    } else {
        dimsInOrOutputs[0] = nbOutputs;
    }
    ArrayOf structInOrOuputs;
    if (dimsInOrOutputs[0] == 0) {
        structInOrOuputs = ArrayOf::emptyConstructor();
    } else {
        stringVector structnames;
        structnames.push_back("Name");
        structnames.push_back("DriverVersion");
        structnames.push_back("MaxChannels");
        structnames.push_back("DefaultSampleRate");
        structnames.push_back("DefaultLowLatency");
        structnames.push_back("DefaultHighLatency");
        structnames.push_back("ID");
        dimsInOrOutputs[1] = 1;
        ArrayOf* InOrOutputs = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dimsInOrOutputs.getElementCount(), structnames);
        structInOrOuputs
            = ArrayOf(NLS_STRUCT_ARRAY, dimsInOrOutputs, InOrOutputs, false, structnames);
        ArrayOfVector name;
        ArrayOfVector driverversion;
        ArrayOfVector maxchannels;
        ArrayOfVector defaultsamplerate;
        ArrayOfVector defaultlowlatency;
        ArrayOfVector defaulthighlatency;
        ArrayOfVector id;
        name.reserve(dimsInOrOutputs.getElementCount());
        driverversion.reserve(dimsInOrOutputs.getElementCount());
        maxchannels.reserve(dimsInOrOutputs.getElementCount());
        defaultsamplerate.reserve(dimsInOrOutputs.getElementCount());
        defaultlowlatency.reserve(dimsInOrOutputs.getElementCount());
        defaulthighlatency.reserve(dimsInOrOutputs.getElementCount());
        id.reserve(dimsInOrOutputs.getElementCount());
        for (AudioDeviceInfo info : infos) {
            bool isInput = (info.padeviceInfo->maxInputChannels != 0);
            bool isOutput = (info.padeviceInfo->maxOutputChannels != 0);
            std::wstring deviceName = utf8_to_wstring(info.padeviceInfo->name);
            const PaHostApiInfo* apiInfo = Pa_GetHostApiInfo(info.padeviceInfo->hostApi);
            const char* driverVersion = apiInfo ? apiInfo->name : "";
            std::wstring deviceDriverVersion = utf8_to_wstring(driverVersion);
            if (input) {
                if (isInput) {
                    double maxChannels = (double)info.padeviceInfo->maxInputChannels;
                    double defaultLowLatency = (double)info.padeviceInfo->defaultLowInputLatency;
                    double defaultHighLatency = (double)info.padeviceInfo->defaultHighInputLatency;
                    double defaultSampleRate = (double)info.padeviceInfo->defaultSampleRate;
                    name.push_back(ArrayOf::characterArrayConstructor(deviceName));
                    driverversion.push_back(
                        ArrayOf::characterArrayConstructor(deviceDriverVersion));
                    maxchannels.push_back(ArrayOf::doubleConstructor(maxChannels));
                    defaultsamplerate.push_back(ArrayOf::doubleConstructor(defaultSampleRate));
                    defaultlowlatency.push_back(ArrayOf::doubleConstructor(defaultLowLatency));
                    defaulthighlatency.push_back(ArrayOf::doubleConstructor(defaultHighLatency));
                    id.push_back(ArrayOf::doubleConstructor((double)info.Id));
                }
            } else {
                if (isOutput) {
                    double maxChannels = (double)info.padeviceInfo->maxOutputChannels;
                    double defaultLowLatency = (double)info.padeviceInfo->defaultLowOutputLatency;
                    double defaultHighLatency = (double)info.padeviceInfo->defaultHighOutputLatency;
                    double defaultSampleRate = (double)info.padeviceInfo->defaultSampleRate;
                    name.push_back(ArrayOf::characterArrayConstructor(deviceName));
                    driverversion.push_back(
                        ArrayOf::characterArrayConstructor(deviceDriverVersion));
                    defaultsamplerate.push_back(ArrayOf::doubleConstructor(defaultSampleRate));
                    maxchannels.push_back(ArrayOf::doubleConstructor(maxChannels));
                    defaultlowlatency.push_back(ArrayOf::doubleConstructor(defaultLowLatency));
                    defaulthighlatency.push_back(ArrayOf::doubleConstructor(defaultHighLatency));
                    id.push_back(ArrayOf::doubleConstructor((double)info.Id));
                }
            }
        }
        structInOrOuputs.setFieldAsList("Name", name);
        structInOrOuputs.setFieldAsList("DriverVersion", driverversion);
        structInOrOuputs.setFieldAsList("MaxChannels", maxchannels);
        structInOrOuputs.setFieldAsList("DefaultSampleRate", defaultsamplerate);
        structInOrOuputs.setFieldAsList("DefaultLowLatency", defaultlowlatency);
        structInOrOuputs.setFieldAsList("DefaultHighLatency", defaulthighlatency);
        structInOrOuputs.setFieldAsList("ID", id);
    }
    return structInOrOuputs;
}
//=============================================================================
ArrayOf
AudioDevInfo(std::wstring& errorMessage)
{
    errorMessage = L"";
    size_t nbInputs = 0;
    size_t nbOutputs = 0;
    for (AudioDeviceInfo info : audioDevices) {
        if (info.padeviceInfo->maxInputChannels != 0) {
            nbInputs++;
        }
        if (info.padeviceInfo->maxOutputChannels != 0) {
            nbOutputs++;
        }
    }
    ArrayOf structInputs = getDevicesInfo(audioDevices, true, nbInputs, nbOutputs);
    ArrayOf structOutputs = getDevicesInfo(audioDevices, false, nbInputs, nbOutputs);
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("input");
    fieldnames.push_back("output");
    fieldvalues.push_back(structInputs);
    fieldvalues.push_back(structOutputs);
    return ArrayOf::structConstructor(fieldnames, fieldvalues);
}
//=============================================================================
ArrayOf
AudioDevInfoDefault(std::wstring& errorMessage)
{
    errorMessage = L"";
    PaDeviceIndex defaultOutput = Pa_GetDefaultOutputDevice();
    boost::container::vector<AudioDeviceInfo> infoOutput;
    if (defaultOutput != paNoDevice) {
        const PaDeviceInfo* pdi_output = Pa_GetDeviceInfo(defaultOutput);
        if (pdi_output) {
            for (AudioDeviceInfo element : audioDevices) {
                if (element.padeviceInfo == pdi_output) {
                    AudioDeviceInfo outputDevice;
                    outputDevice.padeviceInfo = pdi_output;
                    outputDevice.Id = element.Id;
                    infoOutput.push_back(outputDevice);
                    break;
                }
            }
        }
    }
    PaDeviceIndex defaultInput = Pa_GetDefaultInputDevice();
    boost::container::vector<AudioDeviceInfo> infoInput;
    if (defaultInput != paNoDevice) {
        const PaDeviceInfo* pdi_input = Pa_GetDeviceInfo(defaultInput);
        if (pdi_input) {
            for (AudioDeviceInfo element : audioDevices) {
                if (element.padeviceInfo == pdi_input) {
                    AudioDeviceInfo inputDevice;
                    inputDevice.padeviceInfo = pdi_input;
                    inputDevice.Id = element.Id;
                    infoInput.push_back(inputDevice);
                    break;
                }
            }
        }
    }
    ArrayOf structInputs = getDevicesInfo(infoInput, true, infoInput.size(), 0);
    ArrayOf structOutputs = getDevicesInfo(infoOutput, false, 0, infoOutput.size());
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("input");
    fieldnames.push_back("output");
    fieldvalues.push_back(structInputs);
    fieldvalues.push_back(structOutputs);
    return ArrayOf::structConstructor(fieldnames, fieldvalues);
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, std::wstring& errorMessage)
{
    errorMessage = L"";
    if (io == 0 || io == 1) {
        int res = 0;
        for (AudioDeviceInfo info : audioDevices) {
            if (io == 0) // output
            {
                if (info.padeviceInfo->maxInputChannels == 0) {
                    res++;
                }
            } else // input
            {
                if (info.padeviceInfo->maxInputChannels != 0) {
                    res++;
                }
            }
        }
        return ArrayOf::doubleConstructor((double)res);
    } else {
        errorMessage = _W("Wrong value for #1 argument.");
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
static bool
getDeviceInfoById(boost::container::vector<AudioDeviceInfo> infos, int ID, AudioDeviceInfo& info)
{
    for (AudioDeviceInfo element : infos) {
        if (element.Id == ID) {
            info = element;
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, int id, std::wstring& errorMessage)
{
    errorMessage = L"";
    if (io == 0 || io == 1) {
        AudioDeviceInfo infoFound;
        if (getDeviceInfoById(audioDevices, id, infoFound)) {
            if (io == 1) {
                if (infoFound.padeviceInfo->maxInputChannels == 0) {
                    errorMessage = _W("Wrong value for #1 argument.");
                }
            } else {
                if (infoFound.padeviceInfo->maxOutputChannels == 0) {
                    errorMessage = _W("Wrong value for #1 argument.");
                }
            }
            return ArrayOf::characterArrayConstructor(
                utf8_to_wstring(infoFound.padeviceInfo->name));
        } else {
            errorMessage = _W("Wrong value for #2 argument.");
        }
    } else {
        errorMessage = _W("Wrong value for #1 argument.");
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
static bool
getDeviceInfoByName(boost::container::vector<AudioDeviceInfo> infos, int IO, std::wstring name,
    AudioDeviceInfo& info)
{
    for (AudioDeviceInfo element : infos) {
        if (IO == 1) {
            bool isInput = (element.padeviceInfo->maxInputChannels != 0);
            if (isInput) {
                if (utf8_to_wstring(element.padeviceInfo->name) == name) {
                    info = element;
                    return true;
                }
            }
        } else {
            bool isOutput = (element.padeviceInfo->maxOutputChannels != 0);
            if (isOutput) {
                if (utf8_to_wstring(element.padeviceInfo->name) == name) {
                    info = element;
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, std::wstring name, std::wstring& errorMessage)
{
    errorMessage = L"";
    if (io == 0 || io == 1) {
        AudioDeviceInfo infoFound;
        if (!getDeviceInfoByName(audioDevices, io, name, infoFound)) {
            errorMessage = _W("Wrong value for #1 or #2 argument.");
        } else {
            return ArrayOf::doubleConstructor((double)infoFound.Id);
        }
    } else {
        errorMessage = _W("Wrong value for #1 argument.");
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
ArrayOf
AudioDevInfoDriverVersion(int io, int id, std::wstring& errorMessage)
{
    errorMessage = L"";
    if (io == 0 || io == 1) {
        AudioDeviceInfo infoFound;
        if (getDeviceInfoById(audioDevices, id, infoFound)) {
            if (io == 1) {
                if (infoFound.padeviceInfo->maxInputChannels == 0) {
                    errorMessage = _W("Wrong value for #1 argument.");
                }
            } else {
                if (infoFound.padeviceInfo->maxOutputChannels == 0) {
                    errorMessage = _W("Wrong value for #1 argument.");
                }
            }
            const PaHostApiInfo* apiInfo = Pa_GetHostApiInfo(infoFound.padeviceInfo->hostApi);
            const char* driverVersion = apiInfo ? apiInfo->name : "";
            return ArrayOf::characterArrayConstructor(utf8_to_wstring(driverVersion));
        } else {
            errorMessage = _W("Wrong value for #2 argument.");
        }
    } else {
        errorMessage = _W("Wrong value for #1 argument.");
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
static PaSampleFormat
bitsToFormat(int bits)
{
    PaSampleFormat format = 0;
    switch (bits) {
    case -1: {
        format = paFloat32;
    } break;
    case 8: {
        format = paInt8;
    } break;
    case 16: {
        format = paInt16;
    } break;
    case 24: {
        format = paInt24;
    } break;
    case 32: {
        format = paInt32;
    } break;
    }
    return format;
}
//=============================================================================
static bool
searchAudioDevice(boost::container::vector<AudioDeviceInfo> infos, int io, int rate, int bits,
    int chans, AudioDeviceInfo& info)
{
    if (io == 0 || io == 1) {
        for (size_t k = 0; k < infos.size(); k++) {
            PaStreamParameters streamParameters;
            streamParameters.device = infos[k].Id;
            streamParameters.channelCount = chans;
            PaSampleFormat format = bitsToFormat(bits);
            if (format == 0) {
                return false;
            } else {
                streamParameters.sampleFormat = format;
            }
            streamParameters.suggestedLatency = infos[k].padeviceInfo->defaultLowInputLatency;
            streamParameters.hostApiSpecificStreamInfo = 0;
            if (io == 0) {
                if (infos[k].padeviceInfo->maxOutputChannels < chans) {
                    continue;
                }
                PaError err = Pa_IsFormatSupported(0, &streamParameters, rate);
                if (err == paFormatIsSupported) {
                    info = infos[k];
                    return true;
                }
            } else {
                if (infos[k].padeviceInfo->maxInputChannels < chans) {
                    continue;
                }
                PaError err = Pa_IsFormatSupported(&streamParameters, 0, rate);
                if (err == paFormatIsSupported) {
                    info = infos[k];
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, int rate, int bits, int chans, std::wstring& errorMessage)
{
    errorMessage = L"";
    if (io == 0 || io == 1) {
        AudioDeviceInfo deviceFound;
        if (searchAudioDevice(audioDevices, io, rate, bits, chans, deviceFound)) {
            return ArrayOf::doubleConstructor((double)deviceFound.Id);
        } else {
            return ArrayOf::doubleConstructor(-1);
        }
    } else {
        errorMessage = _W("Wrong value for #1 argument.");
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
static bool
supportAudioDevice(
    const PaDeviceInfo* padeviceInfo, int io, int id, int rate, PaSampleFormat format, int chans)
{
    PaStreamParameters streamParameters;
    streamParameters.channelCount = chans;
    streamParameters.sampleFormat = format;
    streamParameters.suggestedLatency = padeviceInfo->defaultLowInputLatency;
    streamParameters.hostApiSpecificStreamInfo = 0;
    if (io == 0) {
        streamParameters.device = getOutputDeviceIndex(id);
        if (padeviceInfo->maxOutputChannels < chans) {
            return false;
        }
        PaError err = Pa_IsFormatSupported(0, &streamParameters, rate);
        if (err == paFormatIsSupported) {
            return true;
        }
    } else if (io == 1) {
        streamParameters.device = getInputDeviceIndex(id);
        if (padeviceInfo->maxInputChannels < chans) {
            return false;
        }
        PaError err = Pa_IsFormatSupported(&streamParameters, 0, rate);
        if (err == paFormatIsSupported) {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, int id, int rate, int bits, int chans, std::wstring& errorMessage)
{
    errorMessage = L"";
    if (io == 0 || io == 1) {
        PaSampleFormat format = bitsToFormat(bits);
        const PaDeviceInfo* padeviceInfo = nullptr;
        for (AudioDeviceInfo info : audioDevices) {
            if (info.Id == id) {
                padeviceInfo = info.padeviceInfo;
                break;
            }
        }
        if (padeviceInfo == nullptr) {
            errorMessage = _W("Wrong value for #2 argument.");
        } else {
            if (format != 0) {
                if (supportAudioDevice(padeviceInfo, io, id, rate, format, chans)) {
                    return ArrayOf::logicalConstructor(true);
                } else {
                    return ArrayOf::logicalConstructor(false);
                }
            } else {
                errorMessage = _W("Wrong value for #4 argument.");
            }
        }
    } else {
        errorMessage = _W("Wrong value for #1 argument.");
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
}
//=============================================================================
