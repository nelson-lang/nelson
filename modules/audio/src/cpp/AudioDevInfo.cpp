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
#include <portaudio.h>
#include "AudioDevInfo.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    class AudioDeviceInfo {
    public:
        std::wstring NAME;
        std::wstring DriverVersion;
        bool isInput;
        bool isOutput;
        int Id;
        const PaDeviceInfo *padeviceInfo;
    };
    //=============================================================================
    boost::container::vector<AudioDeviceInfo> retrieveAudioInfo()
    {
        boost::container::vector<AudioDeviceInfo> infos;
        int k = 0;
        for (PaDeviceIndex i = 0; i < Pa_GetDeviceCount(); i++)
        {
            const PaDeviceInfo *deviceInfo = Pa_GetDeviceInfo(i);
            if (deviceInfo != nullptr)
            {
                AudioDeviceInfo info;
                info.isInput = (deviceInfo->maxInputChannels != 0);
                info.isOutput = (deviceInfo->maxOutputChannels != 0);
                info.NAME = utf8_to_wstring(deviceInfo->name);
                const PaHostApiInfo *apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
                const char *driverVersion = apiInfo ? apiInfo->name : "";
                info.DriverVersion = utf8_to_wstring(driverVersion);
                info.Id = k;
                info.padeviceInfo = deviceInfo;
                if (info.isInput || info.isOutput)
                {
                    k++;
                    infos.push_back(info);
                }
            }
        }
        return infos;
    }
    //=============================================================================
    static ArrayOf getDevicesInfo(boost::container::vector<AudioDeviceInfo> infos, bool input, size_t nbInputs, size_t nbOutputs)
    {
        stringVector structnames;
        structnames.push_back("Name");
        structnames.push_back("DriverVersion");
        structnames.push_back("ID");
        Dimensions dimsInOrOutputs;
        if (input)
        {
            dimsInOrOutputs[0] = nbInputs;
        }
        else
        {
            dimsInOrOutputs[0] = nbOutputs;
        }
        dimsInOrOutputs[1] = 1;
        ArrayOf *InOrOutputs = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dimsInOrOutputs.getElementCount(), structnames);
        ArrayOf structInOrOuputs = ArrayOf(NLS_STRUCT_ARRAY, dimsInOrOutputs, InOrOutputs, false, structnames);
        ArrayOfVector name;
        ArrayOfVector driverversion;
        ArrayOfVector id;
        name.reserve(dimsInOrOutputs.getElementCount());
        driverversion.reserve(dimsInOrOutputs.getElementCount());
        id.reserve(dimsInOrOutputs.getElementCount());
        for (AudioDeviceInfo info : infos)
        {
            if (input)
            {
                if (info.isInput)
                {
                    name.push_back(ArrayOf::stringConstructor(info.NAME));
                    driverversion.push_back(ArrayOf::stringConstructor(info.DriverVersion));
                    id.push_back(ArrayOf::doubleConstructor((double)info.Id));
                }
            }
            else
            {
                if (info.isOutput)
                {
                    name.push_back(ArrayOf::stringConstructor(info.NAME));
                    driverversion.push_back(ArrayOf::stringConstructor(info.DriverVersion));
                    id.push_back(ArrayOf::doubleConstructor((double)info.Id));
                }
            }
        }
        structInOrOuputs.setFieldAsList("Name", name);
        structInOrOuputs.setFieldAsList("DriverVersion", driverversion);
        structInOrOuputs.setFieldAsList("ID", id);
        return structInOrOuputs;
    }
    //=============================================================================
    ArrayOf AudioDevInfo(std::wstring &errorMessage)
    {
        errorMessage = L"";
        boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
        size_t nbInputs = 0;
        size_t nbOutputs = 0;
        for (AudioDeviceInfo info:infos)
        {
            if (info.isInput)
            {
                nbInputs++;
            }
            if (info.isOutput)
            {
                nbOutputs++;
            }
        }
        ArrayOf structInputs = getDevicesInfo(infos, true, nbInputs, nbOutputs);
        ArrayOf structOutputs = getDevicesInfo(infos, false, nbInputs, nbOutputs);
        stringVector fieldnames;
        ArrayOfVector fieldvalues;
        fieldnames.push_back("input");
        fieldnames.push_back("output");
        fieldvalues.push_back(structInputs);
        fieldvalues.push_back(structOutputs);
        return ArrayOf::structConstructor(fieldnames, fieldvalues);
    }
    //=============================================================================
    ArrayOf AudioDevInfo(int io, std::wstring &errorMessage)
    {
        errorMessage = L"";
        if (io == 0 || io == 1)
        {
            int res = 0;
            boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
            for (AudioDeviceInfo info : infos)
            {
                if (io == 0) // output
                {
                    if (!info.isInput)
                    {
                        res++;
                    }
                }
                else // input
                {
                    if (info.isInput)
                    {
                        res++;
                    }
                }
            }
            return ArrayOf::doubleConstructor((double)res);
        }
        else
        {
            errorMessage = _W("Wrong value for #1 argument.");
        }
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
    static bool getDeviceInfoById(boost::container::vector<AudioDeviceInfo> infos, int ID, AudioDeviceInfo &info)
    {
        for (AudioDeviceInfo element : infos)
        {
            if (element.Id == ID)
            {
                info = element;
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    ArrayOf AudioDevInfo(int io, int id, std::wstring &errorMessage)
    {
        errorMessage = L"";
        if (io == 0 || io == 1)
        {
            boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
            AudioDeviceInfo infoFound;
            if (getDeviceInfoById(infos, id, infoFound))
            {
                if (io == 1)
                {
                    if (!infoFound.isInput)
                    {
                        errorMessage = _W("Wrong value for #1 argument.");
                    }
                }
                else
                {
                    if (!infoFound.isOutput)
                    {
                        errorMessage = _W("Wrong value for #1 argument.");
                    }
                }
                return ArrayOf::stringConstructor(infoFound.NAME);
            }
            else
            {
                errorMessage = _W("Wrong value for #2 argument.");
            }
        }
        else
        {
            errorMessage = _W("Wrong value for #1 argument.");
        }
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
    static bool getDeviceInfoByName(boost::container::vector<AudioDeviceInfo> infos, int IO, std::wstring name, AudioDeviceInfo &info)
    {
        for (AudioDeviceInfo element : infos)
        {
            if (IO == 1)
            {
                if (element.isInput)
                {
                    if (element.NAME == name)
                    {
                        info = element;
                        return true;
                    }
                }
            }
            else
            {
                if (element.isOutput)
                {
                    if (element.NAME == name)
                    {
                        info = element;
                        return true;
                    }
                }
            }
        }
        return false;
    }
    //=============================================================================
    ArrayOf AudioDevInfo(int io, std::wstring name, std::wstring &errorMessage)
    {
        errorMessage = L"";
        if (io == 0 || io == 1)
        {
            boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
            AudioDeviceInfo infoFound;
            if (!getDeviceInfoByName(infos, io, name, infoFound))
            {
                errorMessage = _W("Wrong value for #1 or #2 argument.");
            }
            else
            {
                return ArrayOf::doubleConstructor((double)infoFound.Id);
            }
        }
        else
        {
            errorMessage = _W("Wrong value for #1 argument.");
        }
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
    ArrayOf AudioDevInfoDriverVersion(int io, int id, std::wstring &errorMessage)
    {
        errorMessage = L"";
        if (io == 0 || io == 1)
        {
            boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
            AudioDeviceInfo infoFound;
            if (getDeviceInfoById(infos, id, infoFound))
            {
                if (io == 1)
                {
                    if (!infoFound.isInput)
                    {
                        errorMessage = _W("Wrong value for #1 argument.");
                    }
                }
                else
                {
                    if (!infoFound.isOutput)
                    {
                        errorMessage = _W("Wrong value for #1 argument.");
                    }
                }
                return ArrayOf::stringConstructor(infoFound.DriverVersion);
            }
            else
            {
                errorMessage = _W("Wrong value for #2 argument.");
            }
        }
        else
        {
            errorMessage = _W("Wrong value for #1 argument.");
        }
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
    static PaSampleFormat bitsToFormat(int bits)
    {
        PaSampleFormat format = 0;
        switch (bits)
        {
            case -1:
            {
                format = paFloat32;
            }
            break;
            case 8:
            {
                format = paInt8;
            }
            break;
            case 16:
            {
                format = paInt16;
            }
            break;
            case 24:
            {
                format = paInt24;
            }
            break;
            case 32:
            {
                format = paInt32;
            }
            break;
        }
        return format;
    }
    //=============================================================================
    static bool searchAudioDevice(boost::container::vector<AudioDeviceInfo> infos, int io, int rate, int bits, int chans, AudioDeviceInfo &info)
    {
        if (io == 0 || io == 1)
        {
            for (size_t k = 0; k < infos.size(); k++)
            {
                PaStreamParameters streamParameters;
                streamParameters.device = infos[k].Id;
                streamParameters.channelCount = chans;
                PaSampleFormat format = bitsToFormat(bits);
                if (format == 0)
                {
                    return false;
                }
                else
                {
                    streamParameters.sampleFormat = format;
                }
                streamParameters.suggestedLatency = infos[k].padeviceInfo->defaultLowInputLatency;
                streamParameters.hostApiSpecificStreamInfo = 0;
                if (io == 0)
                {
                    if (infos[k].padeviceInfo->maxOutputChannels < chans)
                    {
                        continue;
                    }
                    PaError err = Pa_IsFormatSupported(0, &streamParameters, rate);
                    if (err == paFormatIsSupported)
                    {
                        info = infos[k];
                        return true;
                    }
                }
                else
                {
                    if (infos[k].padeviceInfo->maxInputChannels < chans)
                    {
                        continue;
                    }
                    PaError err = Pa_IsFormatSupported(&streamParameters, 0, rate);
                    if (err == paFormatIsSupported)
                    {
                        info = infos[k];
                        return true;
                    }
                }
            }
        }
        return false;
    }
    //=============================================================================
    ArrayOf AudioDevInfo(int io, int rate, int bits, int chans, std::wstring &errorMessage)
    {
        errorMessage = L"";
        if (io == 0 || io == 1)
        {
            boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
            AudioDeviceInfo deviceFound;
            if (searchAudioDevice(infos, io, rate, bits, chans, deviceFound))
            {
                return ArrayOf::doubleConstructor((double)deviceFound.Id);
            }
            else
            {
                return ArrayOf::doubleConstructor(-1);
            }
        }
        else
        {
            errorMessage = _W("Wrong value for #1 argument.");
        }
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
    static bool supportAudioDevice(const PaDeviceInfo *padeviceInfo, int io, int id, int rate, PaSampleFormat format, int chans)
    {
        PaStreamParameters streamParameters;
        streamParameters.device = id;
        streamParameters.channelCount = chans;
        streamParameters.sampleFormat = format;
        streamParameters.suggestedLatency = padeviceInfo->defaultLowInputLatency;
        streamParameters.hostApiSpecificStreamInfo = 0;
        if (io == 0)
        {
            if (padeviceInfo->maxOutputChannels < chans)
            {
                return false;
            }
            PaError err = Pa_IsFormatSupported(0, &streamParameters, rate);
            if (err == paFormatIsSupported)
            {
                return true;
            }
        }
        else if (io == 1)
        {
            if (padeviceInfo->maxInputChannels < chans)
            {
                return false;
            }
            PaError err = Pa_IsFormatSupported(&streamParameters, 0, rate);
            if (err == paFormatIsSupported)
            {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    ArrayOf AudioDevInfo(int io, int id, int rate, int bits, int chans, std::wstring &errorMessage)
    {
        errorMessage = L"";
        if (io == 0 || io == 1)
        {
            boost::container::vector<AudioDeviceInfo> infos = retrieveAudioInfo();
            PaSampleFormat format = bitsToFormat(bits);
            const PaDeviceInfo *padeviceInfo = nullptr;
            for (AudioDeviceInfo info : infos)
            {
                if (info.Id == id)
                {
                    padeviceInfo = info.padeviceInfo;
                    break;
                }
            }
            if (padeviceInfo == nullptr)
            {
                errorMessage = _W("Wrong value for #2 argument.");
            }
            else
            {
                if (format != 0)
                {
                    if (supportAudioDevice(padeviceInfo, io, id, rate, format, chans))
                    {
                        return ArrayOf::logicalConstructor(true);
                    }
                    else
                    {
                        return ArrayOf::logicalConstructor(false);
                    }
                }
                else
                {
                    errorMessage = _W("Wrong value for #4 argument.");
                }
            }
        }
        else
        {
            errorMessage = _W("Wrong value for #1 argument.");
        }
        return ArrayOf::emptyConstructor();
    }
    //=============================================================================
}
//=============================================================================
