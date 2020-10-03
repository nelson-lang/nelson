//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <portaudio.h>
#include <cstdarg>
#if not defined(__APPLE__) && not defined(__MACH__) && not defined(_MSC_VER)
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
#if not defined(__APPLE__) && not defined(__MACH__) && not defined(_MSC_VER)
void
alsa_error_handler(const char* file, int line, const char* function, int err, const char* fmt, ...)
{ }
//=============================================================================
void
jack_error_handler(const char* text)
{ }
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
#if not defined(__APPLE__) && not defined(__MACH__) && not defined(_MSC_VER)
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
