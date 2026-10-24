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
#include <string>
#ifndef NELSON_AUDIO_FILE_ONLY_BACKEND
#define NELSON_AUDIO_FILE_ONLY_BACKEND 0
#endif
#if !NELSON_AUDIO_FILE_ONLY_BACKEND
#include <portaudio.h>
#include <vector>
#endif
#include "ArrayOf.hpp"
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#if !NELSON_AUDIO_FILE_ONLY_BACKEND
class AudioDeviceInfo
{
public:
    int Id;
    const PaDeviceInfo* padeviceInfo;
};
//=============================================================================
std::vector<AudioDeviceInfo>
getAudioDevices();
PaDeviceIndex
getOutputDeviceIndex(int deviceID);
PaDeviceIndex
getInputDeviceIndex(int deviceID);
#endif
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfoDefault(std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, int id, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, const std::wstring& name, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfoDriverVersion(int io, int id, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, int rate, int bits, int chans, std::wstring& errorMessage);
NLSAUDIO_IMPEXP ArrayOf
AudioDevInfo(int io, int id, int rate, int bits, int chans, std::wstring& errorMessage);
#if !NELSON_AUDIO_FILE_ONLY_BACKEND
NLSAUDIO_IMPEXP void
clearAudioDevicesInfo();
NLSAUDIO_IMPEXP void
retrieveAudioDevicesInfo();
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
