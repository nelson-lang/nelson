//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AudioDevInfo.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr int OUTPUT_DEVICE = 0;
static constexpr int INPUT_DEVICE = 1;
static constexpr int WEB_AUDIO_ID = 0;
//=============================================================================
static ArrayOf
makeWebAudioDevice()
{
    stringVector fieldnames;
    fieldnames.push_back("Name");
    fieldnames.push_back("DriverVersion");
    fieldnames.push_back("MaxChannels");
    fieldnames.push_back("DefaultSampleRate");
    fieldnames.push_back("DefaultLowLatency");
    fieldnames.push_back("DefaultHighLatency");
    fieldnames.push_back("ID");

    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::characterArray(L"Web Audio");
    fieldvalues << ArrayOf::characterArray(L"Browser Web Audio");
    fieldvalues << ArrayOf::doubleScalar(2);
    fieldvalues << ArrayOf::doubleScalar(44100);
    fieldvalues << ArrayOf::doubleScalar(0);
    fieldvalues << ArrayOf::doubleScalar(0);
    fieldvalues << ArrayOf::doubleScalar(WEB_AUDIO_ID);
    return ArrayOf::structArray(fieldnames, fieldvalues);
}
//=============================================================================
static ArrayOf
makePortableDevices()
{
    stringVector fieldnames;
    fieldnames.push_back("input");
    fieldnames.push_back("output");

    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::empty();
    fieldvalues << makeWebAudioDevice();
    return ArrayOf::structArray(fieldnames, fieldvalues);
}
//=============================================================================
static bool
isWebAudioId(int id)
{
    return id == WEB_AUDIO_ID || id == 1;
}
//=============================================================================
static bool
isSupportedOutputFormat(int rate, int bits, int chans)
{
    bool supportedBits = bits == -1 || bits == 8 || bits == 16 || bits == 24 || bits == 32;
    return rate > 0 && rate <= 384000 && supportedBits && chans >= 1 && chans <= 2;
}
//=============================================================================
ArrayOf
AudioDevInfo(std::wstring& errorMessage)
{
    errorMessage.clear();
    return makePortableDevices();
}
//=============================================================================
ArrayOf
AudioDevInfoDefault(std::wstring& errorMessage)
{
    errorMessage.clear();
    return makePortableDevices();
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (io == OUTPUT_DEVICE) {
        return ArrayOf::doubleScalar(1);
    }
    if (io == INPUT_DEVICE) {
        return ArrayOf::doubleScalar(0);
    }
    errorMessage = L"Wrong value for #1 argument.";
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, int id, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (io == OUTPUT_DEVICE && isWebAudioId(id)) {
        return ArrayOf::characterArray(L"Web Audio");
    }
    errorMessage = L"Wrong value for #1 or #2 argument.";
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, const std::wstring& name, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (io == OUTPUT_DEVICE && (name == L"Web Audio" || name == L"Browser Web Audio")) {
        return ArrayOf::doubleScalar(WEB_AUDIO_ID);
    }
    errorMessage = L"Wrong value for #1 or #2 argument.";
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
AudioDevInfoDriverVersion(int io, int id, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (io == OUTPUT_DEVICE && isWebAudioId(id)) {
        return ArrayOf::characterArray(L"Browser Web Audio");
    }
    errorMessage = L"Wrong value for #1 or #2 argument.";
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, int rate, int bits, int chans, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (io == OUTPUT_DEVICE && isSupportedOutputFormat(rate, bits, chans)) {
        return ArrayOf::doubleScalar(WEB_AUDIO_ID);
    }
    if (io == OUTPUT_DEVICE || io == INPUT_DEVICE) {
        return ArrayOf::doubleScalar(-1);
    }
    errorMessage = L"Wrong value for #1 argument.";
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
AudioDevInfo(int io, int id, int rate, int bits, int chans, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (io == OUTPUT_DEVICE && isWebAudioId(id)) {
        return ArrayOf::logicalScalar(isSupportedOutputFormat(rate, bits, chans));
    }
    if (io == INPUT_DEVICE) {
        return ArrayOf::logicalScalar(false);
    }
    errorMessage = L"Wrong value for #1 or #2 argument.";
    return ArrayOf::empty();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
