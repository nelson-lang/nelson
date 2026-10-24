//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterCodecInternal.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    uint16_t
    little16(const uint8_t* bytes)
    {
        return static_cast<uint16_t>(bytes[0] | (static_cast<uint16_t>(bytes[1]) << 8));
    }
    //=============================================================================
    bool
    unpackPcx(const std::vector<uint8_t>& input, size_t begin, size_t expected,
        std::vector<uint8_t>& output)
    {
        output.clear();
        output.reserve(expected);
        size_t offset = begin;
        while (offset < input.size() && output.size() < expected) {
            uint8_t value = input[offset++];
            size_t count = 1;
            if ((value & 0xc0) == 0xc0) {
                count = value & 0x3f;
                if (offset >= input.size()) {
                    return false;
                }
                value = input[offset++];
            }
            output.insert(output.end(), std::min(count, expected - output.size()), value);
        }
        return output.size() == expected;
    }
    //=============================================================================
    void
    appendRun(std::vector<uint8_t>& output, uint8_t value, size_t count)
    {
        while (count > 0) {
            const uint8_t chunk = static_cast<uint8_t>(std::min<size_t>(count, 63));
            if (chunk > 1 || (value & 0xc0) == 0xc0) {
                output.push_back(0xc0 | chunk);
            }
            output.push_back(value);
            count -= chunk;
        }
    }
    //=============================================================================
    void
    encodeRow(const uint8_t* row, size_t size, std::vector<uint8_t>& output)
    {
        size_t offset = 0;
        while (offset < size) {
            size_t count = 1;
            while (offset + count < size && row[offset + count] == row[offset] && count < 63) {
                ++count;
            }
            appendRun(output, row[offset], count);
            offset += count;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
decodePcx(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    if (bytes.size() < 128 || bytes[0] != 0x0a || bytes[2] != 1
        || (bytes[3] != 1 && bytes[3] != 2 && bytes[3] != 4 && bytes[3] != 8)) {
        error = "unsupported PCX image";
        return false;
    }
    image.width = little16(bytes.data() + 8) - little16(bytes.data() + 4) + 1;
    image.height = little16(bytes.data() + 10) - little16(bytes.data() + 6) + 1;
    const int bitsPerPixel = bytes[3];
    const int planes = bytes[65];
    const size_t bytesPerLine = little16(bytes.data() + 66);
    const size_t packedWidth = (static_cast<size_t>(image.width) * bitsPerPixel + 7) / 8;
    if (image.width <= 0 || image.height <= 0 || planes <= 0
        || (bitsPerPixel == 8 && planes != 1 && planes != 3)
        || (bitsPerPixel != 8 && planes != 1 && !(bitsPerPixel == 1 && planes <= 4))
        || bytesPerLine < packedWidth) {
        error = "invalid PCX layout";
        return false;
    }
    std::vector<uint8_t> unpacked;
    const size_t decodedSize = bytesPerLine * static_cast<size_t>(image.height) * planes;
    if (!unpackPcx(bytes, 128, decodedSize, unpacked)) {
        error = "truncated PCX image";
        return false;
    }
    if (bitsPerPixel == 8 && planes == 1) {
        if (bytes.size() < 769 || bytes[bytes.size() - 769] != 12) {
            error = "PCX palette is missing";
            return false;
        }
        image.format = RasterPixelFormat::Indexed8;
        image.pixels.resize(static_cast<size_t>(image.width) * image.height);
        image.palette.resize(256 * 4, 255);
        for (int y = 0; y < image.height; ++y) {
            std::copy_n(unpacked.data() + static_cast<size_t>(y) * bytesPerLine, image.width,
                image.pixels.data() + static_cast<size_t>(y) * image.width);
        }
        const size_t paletteStart = bytes.size() - 768;
        for (size_t i = 0; i < 256; ++i) {
            std::copy_n(bytes.data() + paletteStart + i * 3, 3, image.palette.data() + i * 4);
        }
    } else if (bitsPerPixel == 8) {
        image.format = RasterPixelFormat::RGB8;
        image.pixels.resize(static_cast<size_t>(image.width) * image.height * 3);
        const size_t rowStride = bytesPerLine * 3;
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                for (int plane = 0; plane < 3; ++plane) {
                    image.pixels[(static_cast<size_t>(y) * image.width + x) * 3 + plane]
                        = unpacked[static_cast<size_t>(y) * rowStride + plane * bytesPerLine + x];
                }
            }
        }
    } else {
        const int colorCount = 1 << (bitsPerPixel * planes);
        image.format = RasterPixelFormat::Indexed8;
        image.pixels.resize(static_cast<size_t>(image.width) * image.height);
        image.palette.resize(static_cast<size_t>(colorCount) * 4, 255);
        for (int color = 0; color < colorCount; ++color) {
            image.palette[static_cast<size_t>(color) * 4] = bytes[16 + color * 3];
            image.palette[static_cast<size_t>(color) * 4 + 1] = bytes[17 + color * 3];
            image.palette[static_cast<size_t>(color) * 4 + 2] = bytes[18 + color * 3];
        }
        if (colorCount == 2
            && std::equal(
                image.palette.begin(), image.palette.begin() + 3, image.palette.begin() + 4)) {
            image.palette[4] = image.palette[5] = image.palette[6] = 255;
        }
        const size_t rowStride = bytesPerLine * planes;
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                uint8_t index = 0;
                if (planes == 1) {
                    const int shift = 8 - bitsPerPixel - (x * bitsPerPixel) % 8;
                    index = static_cast<uint8_t>((unpacked[static_cast<size_t>(y) * rowStride
                                                      + static_cast<size_t>(x * bitsPerPixel / 8)]
                                                     >> shift)
                        & ((1 << bitsPerPixel) - 1));
                } else {
                    for (int plane = 0; plane < planes; ++plane) {
                        const uint8_t packed = unpacked[static_cast<size_t>(y) * rowStride
                            + static_cast<size_t>(plane) * bytesPerLine + x / 8];
                        index |= static_cast<uint8_t>(((packed >> (7 - x % 8)) & 1) << plane);
                    }
                }
                image.pixels[static_cast<size_t>(y) * image.width + x] = index;
            }
        }
    }
    return true;
}
//=============================================================================
bool
encodePcx(const RasterImage& image, std::vector<uint8_t>& bytes, std::string& error)
{
    if (image.format == RasterPixelFormat::Gray16) {
        error = "16-bit images cannot be written as PCX";
        return false;
    }
    const bool indexed
        = image.format == RasterPixelFormat::Indexed8 && image.palette.size() <= 1024;
    const int planes = indexed ? 1 : 3;
    const size_t bytesPerLine = static_cast<size_t>((image.width + 1) & ~1);
    bytes.assign(128, 0);
    bytes[0] = 0x0a;
    bytes[1] = 5;
    bytes[2] = 1;
    bytes[3] = 8;
    const uint16_t xmax = static_cast<uint16_t>(image.width - 1);
    const uint16_t ymax = static_cast<uint16_t>(image.height - 1);
    bytes[8] = static_cast<uint8_t>(xmax);
    bytes[9] = static_cast<uint8_t>(xmax >> 8);
    bytes[10] = static_cast<uint8_t>(ymax);
    bytes[11] = static_cast<uint8_t>(ymax >> 8);
    bytes[65] = static_cast<uint8_t>(planes);
    bytes[66] = static_cast<uint8_t>(bytesPerLine);
    bytes[67] = static_cast<uint8_t>(bytesPerLine >> 8);
    bytes[68] = 1;
    std::vector<uint8_t> row(bytesPerLine, 0);
    for (int y = 0; y < image.height; ++y) {
        for (int plane = 0; plane < planes; ++plane) {
            std::fill(row.begin(), row.end(), 0);
            for (int x = 0; x < image.width; ++x) {
                const size_t pixel = static_cast<size_t>(y) * image.width + x;
                if (indexed) {
                    row[x] = image.pixels[pixel];
                } else if (image.format == RasterPixelFormat::Gray8) {
                    row[x] = image.pixels[pixel];
                } else {
                    row[x] = image.pixels[pixel * image.channels() + plane];
                }
            }
            encodeRow(row.data(), row.size(), bytes);
        }
    }
    if (indexed) {
        bytes.push_back(12);
        for (size_t i = 0; i < 256; ++i) {
            const size_t offset = i * 4;
            for (size_t channel = 0; channel < 3; ++channel) {
                bytes.push_back(
                    offset + channel < image.palette.size() ? image.palette[offset + channel] : 0);
            }
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
