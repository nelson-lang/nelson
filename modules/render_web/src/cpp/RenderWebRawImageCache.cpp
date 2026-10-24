//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RenderWebInternal.hpp"
#include <algorithm>
#include <atomic>
#include <cstring>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct RawRgbaEntry
    {
        uint64_t sample = 0;
        int width = 0;
        int height = 0;
        uint64_t lastUse = 0;
        std::string token;
        std::vector<unsigned char> rgba;
    };
    //=============================================================================
    constexpr size_t MAX_RAW_RGBA_CACHE_ENTRIES = 24;
    constexpr size_t MAX_RAW_RGBA_CACHE_BYTES = 32 * 1024 * 1024;
    //=============================================================================
    std::vector<RawRgbaEntry>&
    rawRgbaCache()
    {
        static thread_local auto* cache = new std::vector<RawRgbaEntry>;
        return *cache;
    }
    //=============================================================================
    uint64_t&
    rawRgbaCacheClock()
    {
        static thread_local uint64_t clock = 0;
        return clock;
    }
    //=============================================================================
    size_t&
    rawRgbaCacheBytes()
    {
        static thread_local size_t bytes = 0;
        return bytes;
    }
    //=============================================================================
    void
    mixSample(uint64_t& hash, uint64_t value)
    {
        value += 0x9e3779b97f4a7c15ULL;
        value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
        value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
        value ^= value >> 31;
        hash ^= value + 0x9e3779b97f4a7c15ULL + (hash << 6) + (hash >> 2);
    }
    //=============================================================================
    uint64_t
    sampledRgbaHash(const unsigned char* rgba, int width, int height, int stride)
    {
        uint64_t hash = 1469598103934665603ULL;
        mixSample(hash, static_cast<uint64_t>(static_cast<uint32_t>(width)));
        mixSample(hash, static_cast<uint64_t>(static_cast<uint32_t>(height)));
        if (rgba == nullptr || width <= 0 || height <= 0) {
            return hash;
        }
        const size_t rowBytes = static_cast<size_t>(width) * 4;
        const int rows[] = { 0, height / 2, height - 1 };
        for (int row : rows) {
            const unsigned char* source = rgba + static_cast<size_t>(row) * stride;
            const int samples = 16;
            for (int sample = 0; sample < samples; ++sample) {
                const size_t offset = rowBytes <= sizeof(uint64_t)
                    ? 0
                    : (static_cast<size_t>(sample) * (rowBytes - sizeof(uint64_t)))
                        / static_cast<size_t>(samples - 1);
                uint64_t block = 0;
                std::memcpy(&block, source + offset, std::min(sizeof(block), rowBytes - offset));
                mixSample(hash, block);
            }
        }
        return hash;
    }
    //=============================================================================
    bool
    sameRgba(const std::vector<unsigned char>& cached, const unsigned char* rgba, int width,
        int height, int stride)
    {
        const size_t rowBytes = static_cast<size_t>(width) * 4;
        if (cached.size() != static_cast<size_t>(height) * rowBytes) {
            return false;
        }
        for (int row = 0; row < height; ++row) {
            const unsigned char* source = rgba + static_cast<size_t>(row) * stride;
            const unsigned char* previous = cached.data() + static_cast<size_t>(row) * rowBytes;
            if (std::memcmp(source, previous, rowBytes) != 0) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    std::vector<unsigned char>
    copyTightRgba(const unsigned char* rgba, int width, int height, int stride)
    {
        const size_t rowBytes = static_cast<size_t>(width) * 4;
        std::vector<unsigned char> copy(static_cast<size_t>(height) * rowBytes);
        for (int row = 0; row < height; ++row) {
            const unsigned char* source = rgba + static_cast<size_t>(row) * stride;
            unsigned char* target = copy.data() + static_cast<size_t>(row) * rowBytes;
            std::memcpy(target, source, rowBytes);
        }
        return copy;
    }
    //=============================================================================
    void
    evictRawRgbaCache()
    {
        auto& cache = rawRgbaCache();
        size_t& totalBytes = rawRgbaCacheBytes();
        while (cache.size() > MAX_RAW_RGBA_CACHE_ENTRIES || totalBytes > MAX_RAW_RGBA_CACHE_BYTES) {
            const auto oldest = std::min_element(cache.begin(), cache.end(),
                [](const RawRgbaEntry& left, const RawRgbaEntry& right) {
                    return left.lastUse < right.lastUse;
                });
            if (oldest == cache.end()) {
                break;
            }
            totalBytes -= oldest->rgba.size();
            cache.erase(oldest);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
renderWebRawRgbaPayload(const unsigned char* rgba, int width, int height, int stride,
    bool contentToken, std::vector<unsigned char>& payload, std::string& token,
    bool& includePayload, bool alwaysIncludePayload)
{
    payload.clear();
    token.clear();
    includePayload = true;
    if (rgba == nullptr || width <= 0 || height <= 0) {
        return false;
    }

    const size_t rowBytes = static_cast<size_t>(width) * 4;
    if (contentToken) {
        payload = copyTightRgba(rgba, width, height, stride);
        token = std::to_string(
            renderWebRgbaContentHash(payload.data(), width, height, static_cast<int>(rowBytes)));
        return true;
    }

    const uint64_t sample = sampledRgbaHash(rgba, width, height, stride);
    const uint64_t use = ++rawRgbaCacheClock();
    auto& cache = rawRgbaCache();
    for (RawRgbaEntry& entry : cache) {
        if (entry.sample != sample || entry.width != width || entry.height != height) {
            continue;
        }
        if (!sameRgba(entry.rgba, rgba, width, height, stride)) {
            continue;
        }
        entry.lastUse = use;
        token = entry.token;
        if (alwaysIncludePayload) {
            // Stateless render: keep the stable token (so the client caches it)
            // but resend the pixels, because this client may never have received
            // the frame that first defined the token.
            payload = entry.rgba;
            includePayload = true;
        } else {
            includePayload = false;
        }
        return true;
    }

    static std::atomic<uint64_t> rawImageTokenClock { 0 };
    token = "raw:" + std::to_string(width) + "x" + std::to_string(height) + ":"
        + std::to_string(++rawImageTokenClock);
    payload = copyTightRgba(rgba, width, height, stride);
    rawRgbaCacheBytes() += payload.size();
    cache.push_back({ sample, width, height, use, token, payload });
    evictRawRgbaCache();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
