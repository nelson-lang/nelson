//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "ClassName.hpp"
#include "ComplexTranspose.hpp"
#include "PortableAudioplayerObject.hpp"
#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<int> nextPortableAudioId { 1 };
//=============================================================================
static wstringVector
portableAudioplayerProperties()
{
    return { L"SampleRate", L"BitsPerSample", L"NumChannels", L"DeviceID", L"CurrentSample",
        L"TotalSamples", L"Running", L"StartFcn", L"StopFcn", L"TimerFcn", L"TimerPeriod", L"Tag",
        L"UserData", L"Type" };
}
//=============================================================================
static wstringVector
portableAudioplayerMethods()
{
    return { L"delete", L"get", L"pause", L"play", L"playblocking", L"resume", L"set", L"stop" };
}
//=============================================================================
static std::string
base64Encode(const unsigned char* data, size_t length)
{
    static constexpr char alphabet[]
        = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    std::string encoded;
    encoded.reserve(((length + 2) / 3) * 4);
    for (size_t i = 0; i < length; i += 3) {
        const uint32_t a = data[i];
        const uint32_t b = i + 1 < length ? data[i + 1] : 0;
        const uint32_t c = i + 2 < length ? data[i + 2] : 0;
        const uint32_t triple = (a << 16) | (b << 8) | c;
        encoded.push_back(alphabet[(triple >> 18) & 0x3F]);
        encoded.push_back(alphabet[(triple >> 12) & 0x3F]);
        encoded.push_back(i + 1 < length ? alphabet[(triple >> 6) & 0x3F] : '=');
        encoded.push_back(i + 2 < length ? alphabet[triple & 0x3F] : '=');
    }
    return encoded;
}
//=============================================================================
static std::vector<float>
packFloat32Samples(const std::vector<double>& samples, int totalSamples, int channels,
    int firstSample, int offsetFrame, int frames)
{
    std::vector<float> packed(static_cast<size_t>(frames) * static_cast<size_t>(channels));
    size_t output = 0;
    for (int channel = 0; channel < channels; ++channel) {
        const int offset = channel * totalSamples;
        for (int frame = 0; frame < frames; ++frame) {
            const int sampleIndex = firstSample + offsetFrame + frame;
            const double sample = std::clamp(samples[offset + sampleIndex], -1.0, 1.0);
            packed[output++] = static_cast<float>(sample);
        }
    }
    return packed;
}
//=============================================================================
static std::string
encodeFloat32SamplesBase64(const std::vector<double>& samples, int totalSamples, int channels,
    int firstSample, int offsetFrame, int frames)
{
    const std::vector<float> packed
        = packFloat32Samples(samples, totalSamples, channels, firstSample, offsetFrame, frames);
    return base64Encode(
        reinterpret_cast<const unsigned char*>(packed.data()), packed.size() * sizeof(float));
}
//=============================================================================
#ifdef __EMSCRIPTEN__
static bool
hasDirectAudioBufferHooks()
{
    return EM_ASM_INT({
        return typeof Module['onNelsonAudioBufferBegin'] == 'function'
            && typeof Module['onNelsonAudioBufferChunk'] == 'function'
            && typeof Module['onNelsonAudioBufferPlay'] == 'function';
    }) != 0;
}
//=============================================================================
static void
emitDirectAudioBegin(int audioId, int sampleRate, int channels, int frames)
{
    EM_ASM(
        { Module['onNelsonAudioBufferBegin']($0, $1, $2, $3); }, audioId, sampleRate, channels,
        frames);
}
//=============================================================================
static void
emitDirectAudioChunk(int audioId, int offsetFrame, int frames, const std::vector<float>& packed)
{
    const auto* data = reinterpret_cast<const unsigned char*>(packed.data());
    const int byteLength = static_cast<int>(packed.size() * sizeof(float));
    EM_ASM(
        {
            const bytes = HEAPU8.slice($3, $3 + $4).buffer;
            Module['onNelsonAudioBufferChunk']($0, $1, $2, bytes);
        },
        audioId, offsetFrame, frames, data, byteLength);
}
//=============================================================================
static void
emitDirectAudioPlay(int audioId)
{
    EM_ASM({ Module['onNelsonAudioBufferPlay']($0); }, audioId);
}
//=============================================================================
static bool
hasDirectAudioControlHook()
{
    return EM_ASM_INT({ return typeof Module['onNelsonAudioControl'] == 'function'; }) != 0;
}
//=============================================================================
static void
emitDirectAudioControl(int audioId, const char* command)
{
    EM_ASM({ Module['onNelsonAudioControl']($0, UTF8ToString($1)); }, audioId, command);
}
//=============================================================================
#endif
//=============================================================================
PortableAudioplayerObject::PortableAudioplayerObject()
    : HandleGenericObject(NLS_HANDLE_AUDIOPLAYER_CATEGORY_STR, this, false)
    , audioId(nextPortableAudioId++)
{
}
//=============================================================================
bool
PortableAudioplayerObject::setSamples(
    const ArrayOf& data, int newSampleRate, int newBitsPerSample, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (!data.isNumeric() || data.isComplex() || data.isSparse()) {
        errorMessage = L"Audio data must be real and numeric.";
        return false;
    }
    if (data.isEmpty()) {
        errorMessage = L"Empty matrix not allowed.";
        return false;
    }
    if (!data.is2D() && !data.isVector()) {
        errorMessage = L"Vector or matrix 2D expected.";
        return false;
    }
    if (newSampleRate <= 0) {
        errorMessage = L"Sample rate must be positive.";
        return false;
    }
    bool supportedBits = newBitsPerSample == 8 || newBitsPerSample == 16 || newBitsPerSample == 24;
    if (!supportedBits) {
        errorMessage = L"Bits per sample must be 8, 16, or 24.";
        return false;
    }

    bool needToOverload = false;
    ArrayOf audioData
        = data.columns() > data.rows() ? ComplexTranspose(data, needToOverload) : data;
    audioData.promoteType(NLS_DOUBLE);
    channels = static_cast<int>(audioData.columns());
    totalSamples = static_cast<int>(audioData.rows());
    if (channels < 1 || channels > 2) {
        errorMessage = L"Too many output channels.";
        return false;
    }
    const double* ptr = static_cast<const double*>(audioData.dataPointer());
    samples.assign(ptr, ptr + audioData.elementCount());
    sampleRate = newSampleRate;
    bitsPerSample = newBitsPerSample;
    return true;
}
//=============================================================================
bool
PortableAudioplayerObject::play(int firstSample, int lastSample, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (samples.empty()) {
        return true;
    }
    if (firstSample < 0) {
        firstSample = 0;
    }
    if (lastSample <= 0) {
        lastSample = totalSamples - 1;
    }
    if (firstSample >= totalSamples || lastSample >= totalSamples || firstSample > lastSample) {
        errorMessage = L"Invalid range.";
        return false;
    }
    int frames = lastSample - firstSample + 1;
    this->firstSample = firstSample;
    this->lastSample = lastSample;
    currentSample = firstSample;
    startedAt = std::chrono::steady_clock::now();
    running = true;
    constexpr int chunkFrames = 16384;
#ifdef __EMSCRIPTEN__
    if (hasDirectAudioBufferHooks()) {
        emitDirectAudioBegin(audioId, sampleRate, channels, frames);
        for (int offsetFrame = 0; offsetFrame < frames; offsetFrame += chunkFrames) {
            const int currentFrames = std::min(chunkFrames, frames - offsetFrame);
            const std::vector<float> packed = packFloat32Samples(
                samples, totalSamples, channels, firstSample, offsetFrame, currentFrames);
            emitDirectAudioChunk(audioId, offsetFrame, currentFrames, packed);
        }
        emitDirectAudioPlay(audioId);
        return true;
    }
#endif
    std::cout << "__NELSON_UI_ACTION_BEGIN__";
    std::cout << "{\"type\":\"audio-buffer-begin\",\"id\":" << audioId
              << ",\"sampleRate\":" << sampleRate << ",\"channels\":" << channels
              << ",\"frames\":" << frames << "}";
    std::cout << "__NELSON_UI_ACTION_END__";
    for (int offsetFrame = 0; offsetFrame < frames; offsetFrame += chunkFrames) {
        int currentFrames = std::min(chunkFrames, frames - offsetFrame);
        const std::string encoded = encodeFloat32SamplesBase64(
            samples, totalSamples, channels, firstSample, offsetFrame, currentFrames);
        std::cout << "__NELSON_UI_ACTION_BEGIN__";
        std::cout << "{\"type\":\"audio-buffer-chunk\",\"id\":" << audioId
                  << ",\"offset\":" << offsetFrame << ",\"frames\":" << currentFrames
                  << ",\"samplesF32Base64\":\"" << encoded << "\"}";
        std::cout << "__NELSON_UI_ACTION_END__";
    }
    std::cout << "__NELSON_UI_ACTION_BEGIN__";
    std::cout << "{\"type\":\"audio-buffer-play\",\"id\":" << audioId << "}";
    std::cout << "__NELSON_UI_ACTION_END__" << std::endl;
    return true;
}
//=============================================================================
bool
PortableAudioplayerObject::pause()
{
    updateCurrentSample();
    running = false;
    emitControl("pause");
    return true;
}
//=============================================================================
bool
PortableAudioplayerObject::resume()
{
    if (currentSample < lastSample) {
        startedAt = std::chrono::steady_clock::now()
            - std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                std::chrono::duration<double>(
                    static_cast<double>(currentSample - firstSample) / sampleRate));
        running = true;
    }
    emitControl("resume");
    return true;
}
//=============================================================================
bool
PortableAudioplayerObject::stop()
{
    running = false;
    currentSample = 0;
    emitControl("stop");
    return true;
}
//=============================================================================
bool
PortableAudioplayerObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    if (propertyName == L"SampleRate") {
        res = ArrayOf::doubleScalar(sampleRate);
        return true;
    }
    if (propertyName == L"BitsPerSample") {
        res = ArrayOf::doubleScalar(bitsPerSample);
        return true;
    }
    if (propertyName == L"NumChannels") {
        res = ArrayOf::doubleScalar(channels);
        return true;
    }
    if (propertyName == L"DeviceID") {
        res = ArrayOf::doubleScalar(-1);
        return true;
    }
    if (propertyName == L"CurrentSample") {
        res = ArrayOf::doubleScalar(getCurrentSample());
        return true;
    }
    if (propertyName == L"TotalSamples") {
        res = ArrayOf::doubleScalar(getTotalSamples());
        return true;
    }
    if (propertyName == L"Running") {
        res = ArrayOf::characterArray(getRunning() ? L"on" : L"off");
        return true;
    }
    if (propertyName == L"TimerPeriod") {
        res = ArrayOf::doubleScalar(0.05);
        return true;
    }
    if (propertyName == L"Tag") {
        res = ArrayOf::characterArray(tag);
        return true;
    }
    if (propertyName == L"Type") {
        res = ArrayOf::characterArray(L"audioplayer");
        return true;
    }
    if (propertyName == L"UserData") {
        res = userData;
        return true;
    }
    if (propertyName == L"StartFcn" || propertyName == L"StopFcn" || propertyName == L"TimerFcn") {
        res = ArrayOf::empty();
        return true;
    }
    return false;
}
//=============================================================================
bool
PortableAudioplayerObject::set(
    const std::wstring& propertyName, const ArrayOf& propertyValue, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (propertyName == L"Tag") {
        tag = propertyValue.asWideString();
        return true;
    }
    if (propertyName == L"UserData") {
        userData = propertyValue;
        return true;
    }
    errorMessage = L"Cannot set a read-only property.";
    return false;
}
//=============================================================================
bool
PortableAudioplayerObject::isWritableProperty(const std::wstring& propertyName)
{
    return propertyName == L"Tag" || propertyName == L"UserData";
}
//=============================================================================
int
PortableAudioplayerObject::getCurrentSample()
{
    updateCurrentSample();
    return currentSample;
}
//=============================================================================
int
PortableAudioplayerObject::getTotalSamples() const
{
    return totalSamples;
}
//=============================================================================
bool
PortableAudioplayerObject::getRunning()
{
    updateCurrentSample();
    return running;
}
//=============================================================================
bool
PortableAudioplayerObject::isProperty(const std::wstring& propertyName)
{
    wstringVector names = properties();
    return std::find(names.begin(), names.end(), propertyName) != names.end();
}
//=============================================================================
wstringVector
PortableAudioplayerObject::properties()
{
    return portableAudioplayerProperties();
}
//=============================================================================
wstringVector
PortableAudioplayerObject::methods()
{
    return portableAudioplayerMethods();
}
//=============================================================================
void
PortableAudioplayerObject::updateCurrentSample()
{
    if (!running || sampleRate <= 0) {
        return;
    }
    const auto now = std::chrono::steady_clock::now();
    const auto elapsed = std::chrono::duration<double>(now - startedAt).count();
    const int advanced = static_cast<int>(elapsed * sampleRate);
    currentSample = std::min(lastSample, firstSample + advanced);
    if (currentSample >= lastSample) {
        running = false;
    }
}
//=============================================================================
void
PortableAudioplayerObject::emitControl(const char* command) const
{
#ifdef __EMSCRIPTEN__
    if (hasDirectAudioControlHook()) {
        emitDirectAudioControl(audioId, command);
        return;
    }
#endif
    std::cout << "__NELSON_UI_ACTION_BEGIN__";
    std::cout << "{\"type\":\"audio-control\",\"id\":" << audioId << ",\"command\":\"" << command
              << "\"}";
    std::cout << "__NELSON_UI_ACTION_END__" << std::endl;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
