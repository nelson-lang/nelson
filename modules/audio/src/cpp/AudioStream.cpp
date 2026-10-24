//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mutex>
#include <unordered_set>
#include <vector>
#include "AudioStream.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::mutex openStreamsMutex;
static std::unordered_set<AudioStream*> openStreams;
//=============================================================================
static std::vector<AudioStream*>
getOpenStreams();
static void
registerOpenStream(AudioStream* audioStream, bool isOpen);
static std::wstring
errorText(PaError err);
//=============================================================================
AudioStream::~AudioStream() { close(); }
//=============================================================================
PaError
AudioStream::open(const PaStreamParameters* input, const PaStreamParameters* output,
    double sampleRate, PaStreamCallback* callback, void* userData)
{
    close();
    // streams of other objects which ended by themselves release their device now
    for (AudioStream* audioStream : getOpenStreams()) {
        audioStream->closeIfFinished();
    }
    ownerCallback = callback;
    ownerUserData = userData;
    PaStream* newStream = nullptr;
    PaError err = Pa_OpenStream(&newStream, input, output, sampleRate, paFramesPerBufferUnspecified,
        paNoFlag, &AudioStream::onStreamCallback, this);
    if (err != paNoError) {
        return err;
    }
    stream = newStream;
    paused = false;
    finished = false;
    registerOpenStream(this, true);
    err = Pa_SetStreamFinishedCallback(stream, &AudioStream::onStreamFinished);
    if (err == paNoError) {
        err = Pa_StartStream(stream);
    }
    if (err != paNoError) {
        close();
    }
    return err;
}
//=============================================================================
PaError
AudioStream::pause()
{
    if (!stream) {
        return paBadStreamPtr;
    }
    paused = true;
    return Pa_StopStream(stream);
}
//=============================================================================
PaError
AudioStream::resume()
{
    if (!stream) {
        return paBadStreamPtr;
    }
    if (Pa_IsStreamStopped(stream) != 1) {
        return paNoError;
    }
    paused = false;
    finished = false;
    return Pa_StartStream(stream);
}
//=============================================================================
void
AudioStream::close()
{
    if (!stream) {
        return;
    }
    // Pa_CloseStream aborts an active stream and waits for its callback to return
    Pa_CloseStream(stream);
    stream = nullptr;
    paused = false;
    finished = false;
    registerOpenStream(this, false);
}
//=============================================================================
void
AudioStream::closeIfFinished()
{
    if (stream && finished && !paused) {
        close();
    }
}
//=============================================================================
bool
AudioStream::isOpen() const
{
    return stream != nullptr;
}
//=============================================================================
bool
AudioStream::isPaused() const
{
    return stream != nullptr && paused;
}
//=============================================================================
bool
AudioStream::isFinished() const
{
    return finished.load();
}
//=============================================================================
void
AudioStream::closeAll()
{
    for (AudioStream* audioStream : getOpenStreams()) {
        audioStream->close();
    }
}
//=============================================================================
int
AudioStream::onStreamCallback(const void* input, void* output, unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo, PaStreamCallbackFlags statusFlags, void* userData)
{
    auto* audioStream = static_cast<AudioStream*>(userData);
    return audioStream->ownerCallback(
        input, output, frameCount, timeInfo, statusFlags, audioStream->ownerUserData);
}
//=============================================================================
void
AudioStream::onStreamFinished(void* userData)
{
    // audio thread: only flags the end, the main thread closes the stream
    static_cast<AudioStream*>(userData)->finished = true;
}
//=============================================================================
std::vector<AudioStream*>
getOpenStreams()
{
    std::lock_guard<std::mutex> lock(openStreamsMutex);
    return std::vector<AudioStream*>(openStreams.begin(), openStreams.end());
}
//=============================================================================
void
registerOpenStream(AudioStream* audioStream, bool isOpen)
{
    std::lock_guard<std::mutex> lock(openStreamsMutex);
    if (isOpen) {
        openStreams.insert(audioStream);
    } else {
        openStreams.erase(audioStream);
    }
}
//=============================================================================
void
checkAudioStreamError(PaError err)
{
    if (err != paNoError) {
        Error(L"Nelson:audio:streamFailed", L"Audio stream failed: %s", errorText(err));
    }
}
//=============================================================================
std::wstring
errorText(PaError err)
{
    std::string text = Pa_GetErrorText(err);
    if (err == paUnanticipatedHostError) {
        const PaHostErrorInfo* hostError = Pa_GetLastHostErrorInfo();
        if (hostError && hostError->errorText && hostError->errorText[0] != 0) {
            text = text + " (" + hostError->errorText + ")";
        }
    }
    return utf8_to_wstring(text);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
