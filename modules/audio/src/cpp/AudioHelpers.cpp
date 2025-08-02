//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <portaudio.h>
#include <cstdarg>
#if !defined(__APPLE__) && !defined(__MACH__) && !defined(_MSC_VER)
#include <alsa/error.h>
#include <jack/jack.h>
#endif
#include "AudioHelpers.hpp"
#include "AudioDevInfo.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool audioInitialized = false;
//=============================================================================
#if !defined(__APPLE__) && !defined(__MACH__) && !defined(_MSC_VER)
void
alsa_error_handler(const char* file, int line, const char* function, int err, const char* fmt, ...)
{
}
//=============================================================================
void
jack_error_handler(const char* text)
{
}
//=============================================================================
static void
disableAlsaError()
{
    snd_lib_error_set_handler(alsa_error_handler);
}
//=============================================================================
static void
disableJackError()
{
    jack_set_error_function(jack_error_handler);
}
#endif
//=============================================================================

bool
initializeAudio()
{
#if !defined(__APPLE__) && !defined(__MACH__) && !defined(_MSC_VER)
    disableAlsaError();
    disableJackError();
#endif
    if (!audioInitialized) {
        PaError err = Pa_Initialize();
        if (err == paNoError) {
            audioInitialized = true;
            retrieveAudioDevicesInfo();
            return audioInitialized;
        }
    }
    audioInitialized = false;
    return false;
}
//=============================================================================
bool
terminateAudio()
{
    if (audioInitialized) {
        PaError err = Pa_Terminate();
        if (err == paNoError) {
            clearAudioDevicesInfo();
            audioInitialized = false;
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
