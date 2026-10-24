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
#include <atomic>
#include <string>
#include <portaudio.h>
#include "nlsAudio_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Owns one PortAudio stream. Every method is called from the main thread only:
// the audio thread never opens, stops or closes a stream. When the user callback
// returns paComplete, PortAudio reports the end through the stream finished
// callback and the stream is closed later by closeIfFinished().
//=============================================================================
class NLSAUDIO_IMPEXP AudioStream
{
public:
    AudioStream() = default;
    ~AudioStream();
    AudioStream(const AudioStream&) = delete;
    AudioStream&
    operator=(const AudioStream&)
        = delete;

    PaError
    open(const PaStreamParameters* input, const PaStreamParameters* output, double sampleRate,
        PaStreamCallback* callback, void* userData);
    PaError
    pause();
    PaError
    resume();
    void
    close();
    // closes the stream once it ended by itself (not when paused)
    void
    closeIfFinished();

    bool
    isOpen() const;
    bool
    isPaused() const;
    bool
    isFinished() const;

    // closes every open stream, required before Pa_Terminate
    static void
    closeAll();

private:
    PaStream* stream = nullptr;
    PaStreamCallback* ownerCallback = nullptr;
    void* ownerUserData = nullptr;
    bool paused = false;
    std::atomic<bool> finished { false };

    static int
    onStreamCallback(const void* input, void* output, unsigned long frameCount,
        const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags,
        void* userData);
    static void
    onStreamFinished(void* userData);
};
//=============================================================================
// raises a Nelson error with the PortAudio message when err is not paNoError
NLSAUDIO_IMPEXP void
checkAudioStreamError(PaError err);
//=============================================================================
} // namespace Nelson
//=============================================================================
