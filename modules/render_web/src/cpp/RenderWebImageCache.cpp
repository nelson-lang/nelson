//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RenderWebInternal.hpp"
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct EncodedImage
    {
        uint64_t hash = 0;
        int width = 0;
        int height = 0;
        uint64_t lastUse = 0;
        std::string pngBase64;
    };
    //=============================================================================
    void
    mixHashWord(uint64_t& hash, uint64_t value)
    {
        // SplitMix64 avalanche followed by boost-style hash combine. The token stays
        // content-based, with fewer operations per frame by hashing RGBA as 64-bit
        // blocks.
        value += 0x9e3779b97f4a7c15ULL;
        value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
        value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
        value ^= value >> 31;
        hash ^= value + 0x9e3779b97f4a7c15ULL + (hash << 6) + (hash >> 2);
    }
    //=============================================================================
} // namespace
//=============================================================================
uint64_t
renderWebRgbaContentHash(const unsigned char* rgba, int width, int height, int stride)
{
    uint64_t hash = 1469598103934665603ULL;
    mixHashWord(hash, static_cast<uint64_t>(static_cast<uint32_t>(width)));
    mixHashWord(hash, static_cast<uint64_t>(static_cast<uint32_t>(height)));
    if (rgba == nullptr || width <= 0 || height <= 0) {
        return hash;
    }
    const size_t rowBytes = static_cast<size_t>(width) * 4;
    for (int row = 0; row < height; ++row) {
        const unsigned char* source = rgba + static_cast<size_t>(row) * stride;
        size_t index = 0;
        for (; index + sizeof(uint64_t) <= rowBytes; index += sizeof(uint64_t)) {
            uint64_t block = 0;
            std::memcpy(&block, source + index, sizeof(block));
            mixHashWord(hash, block);
        }
        if (index < rowBytes) {
            uint64_t tail = static_cast<uint64_t>(rowBytes - index);
            int shift = 8;
            for (; index < rowBytes; ++index, shift += 8) {
                tail |= static_cast<uint64_t>(source[index]) << shift;
            }
            mixHashWord(hash, tail);
        }
    }
    return hash;
}
//=============================================================================
namespace {
    //=============================================================================
    std::vector<EncodedImage>&
    imageCache()
    {
        static thread_local auto* cache = new std::vector<EncodedImage>;
        return *cache;
    }
    //=============================================================================
    uint64_t&
    imageCacheClock()
    {
        static thread_local uint64_t clock = 0;
        return clock;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
renderWebCachedRgbaToPngBase64(const unsigned char* rgba, int width, int height, int stride,
    std::string& base64, std::string& token, bool& includePayload)
{
#ifndef __EMSCRIPTEN__
    token.clear();
    includePayload = true;
    return renderWebRgbaToPngBase64(rgba, width, height, stride, base64);
#else
    constexpr size_t maximumEntries = 12;
    const uint64_t hash = renderWebRgbaContentHash(rgba, width, height, stride);
    token = std::to_string(hash);
    const uint64_t use = ++imageCacheClock();
    std::vector<EncodedImage>& cache = imageCache();
    const auto found = std::find_if(cache.begin(), cache.end(), [&](const EncodedImage& entry) {
        return entry.hash == hash && entry.width == width && entry.height == height;
    });
    if (found != cache.end()) {
        found->lastUse = use;
        base64 = found->pngBase64;
        includePayload = false;
    } else {
        if (!renderWebRgbaToPngBase64(rgba, width, height, stride, base64)) {
            return false;
        }
        if (cache.size() >= maximumEntries) {
            const auto oldest = std::min_element(cache.begin(), cache.end(),
                [](const EncodedImage& left, const EncodedImage& right) {
                    return left.lastUse < right.lastUse;
                });
            cache.erase(oldest);
        }
        cache.push_back({ hash, width, height, use, base64 });
        includePayload = true;
    }
    return true;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
