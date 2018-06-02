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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsAudio_exports.h"
#include <boost/container/vector.hpp>
#include <portaudio.h>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class AudioDeviceInfo
{
public:
    int Id;
    const PaDeviceInfo* padeviceInfo;
};
//=============================================================================
boost::container::vector<AudioDeviceInfo>
getAudioDevices();
PaDeviceIndex
getOutputDeviceIndex(int deviceID);
PaDeviceIndex
getInputDeviceIndex(int deviceID);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfoDefault(std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, int id, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, std::wstring name, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfoDriverVersion(int io, int id, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, int rate, int bits, int chans, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, int id, int rate, int bits, int chans, std::wstring& errorMessage);
NLSAUDIO_IMPEXP void
clearAudioDevicesInfo();
NLSAUDIO_IMPEXP void
retrieveAudioDevicesInfo();
} // namespace Nelson
//=============================================================================
