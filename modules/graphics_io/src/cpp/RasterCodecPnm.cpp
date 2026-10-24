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
#include <cctype>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    class TokenReader
    {
    public:
        explicit TokenReader(const std::vector<uint8_t>& bytes) : m_bytes(bytes) { }
        std::string
        next()
        {
            skip();
            const size_t begin = m_pos;
            while (m_pos < m_bytes.size() && !std::isspace(m_bytes[m_pos])) {
                ++m_pos;
            }
            return std::string(
                reinterpret_cast<const char*>(m_bytes.data() + begin), m_pos - begin);
        }
        size_t
        dataOffset()
        {
            if (m_pos < m_bytes.size() && std::isspace(m_bytes[m_pos])) {
                ++m_pos;
            }
            return m_pos;
        }

    private:
        void
        skip()
        {
            while (m_pos < m_bytes.size()) {
                while (m_pos < m_bytes.size() && std::isspace(m_bytes[m_pos])) {
                    ++m_pos;
                }
                if (m_pos >= m_bytes.size() || m_bytes[m_pos] != '#') {
                    break;
                }
                while (m_pos < m_bytes.size() && m_bytes[m_pos] != '\n') {
                    ++m_pos;
                }
            }
        }
        const std::vector<uint8_t>& m_bytes;
        size_t m_pos { 0 };
    };
    //=============================================================================
    uint8_t
    luminance(uint8_t red, uint8_t green, uint8_t blue)
    {
        return static_cast<uint8_t>((77 * red + 150 * green + 29 * blue) >> 8);
    }
    //=============================================================================
    void
    pixelRgb(const RasterImage& image, size_t index, uint8_t& red, uint8_t& green, uint8_t& blue)
    {
        if (image.format == RasterPixelFormat::Indexed8) {
            const size_t offset = static_cast<size_t>(image.pixels[index]) * 4;
            red = image.palette[offset];
            green = image.palette[offset + 1];
            blue = image.palette[offset + 2];
        } else if (image.format == RasterPixelFormat::Gray8) {
            red = green = blue = image.pixels[index];
        } else {
            const size_t channels = image.format == RasterPixelFormat::RGB8 ? 3 : 4;
            red = image.pixels[index * channels];
            green = image.pixels[index * channels + 1];
            blue = image.pixels[index * channels + 2];
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
decodePnm(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    TokenReader reader(bytes);
    const std::string magic = reader.next();
    if (magic.size() != 2 || magic[0] != 'P' || magic[1] < '1' || magic[1] > '6') {
        error = "invalid PNM image";
        return false;
    }
    const bool ascii = magic[1] <= '3';
    const bool bitmap = magic[1] == '1' || magic[1] == '4';
    const bool gray = magic[1] == '2' || magic[1] == '5';
    try {
        image.width = std::stoi(reader.next());
        image.height = std::stoi(reader.next());
        const int maximum = bitmap ? 1 : std::stoi(reader.next());
        if (image.width <= 0 || image.height <= 0 || maximum <= 0 || maximum > 65535) {
            throw std::invalid_argument("range");
        }
        image.format = gray && maximum > 255
            ? RasterPixelFormat::Gray16
            : (gray || bitmap ? RasterPixelFormat::Gray8 : RasterPixelFormat::RGB8);
        const size_t count = static_cast<size_t>(image.width) * image.height;
        image.pixels.resize(count * image.channels() * image.bytesPerChannel());
        if (ascii) {
            const size_t samples = count * image.channels();
            for (size_t i = 0; i < samples; ++i) {
                int value = std::stoi(reader.next());
                if (bitmap) {
                    value = value ? 0 : 255;
                } else {
                    value = value * (maximum > 255 ? 65535 : 255) / maximum;
                }
                if (image.format == RasterPixelFormat::Gray16) {
                    reinterpret_cast<uint16_t*>(image.pixels.data())[i]
                        = static_cast<uint16_t>(value);
                } else {
                    image.pixels[i] = static_cast<uint8_t>(value);
                }
            }
        } else if (bitmap) {
            const size_t offset = reader.dataOffset();
            const size_t packedRow = static_cast<size_t>((image.width + 7) / 8);
            if (offset + packedRow * image.height > bytes.size()) {
                throw std::invalid_argument("size");
            }
            for (int y = 0; y < image.height; ++y) {
                for (int x = 0; x < image.width; ++x) {
                    image.pixels[static_cast<size_t>(y) * image.width + x]
                        = bytes[offset + static_cast<size_t>(y) * packedRow + x / 8]
                            & (0x80 >> (x % 8))
                        ? 0
                        : 255;
                }
            }
        } else {
            const size_t offset = reader.dataOffset();
            const size_t samples = count * image.channels();
            const size_t bytesPerSample = maximum > 255 ? 2 : 1;
            if (offset + samples * bytesPerSample > bytes.size()) {
                throw std::invalid_argument("size");
            }
            for (size_t i = 0; i < samples; ++i) {
                const int value = bytesPerSample == 1
                    ? bytes[offset + i]
                    : (static_cast<int>(bytes[offset + i * 2]) << 8) | bytes[offset + i * 2 + 1];
                const int scaled = static_cast<int>(
                    static_cast<uint64_t>(value) * (maximum > 255 ? 65535 : 255) / maximum);
                if (image.format == RasterPixelFormat::Gray16) {
                    reinterpret_cast<uint16_t*>(image.pixels.data())[i]
                        = static_cast<uint16_t>(scaled);
                } else {
                    image.pixels[i] = static_cast<uint8_t>(scaled);
                }
            }
        }
    } catch (...) {
        error = "invalid or truncated PNM image";
        return false;
    }
    return true;
}
//=============================================================================
bool
encodePnm(const std::string& requested, const RasterImage& image, std::vector<uint8_t>& bytes,
    std::string& error)
{
    std::string format = requested == "pnm"
        ? (image.format == RasterPixelFormat::Gray8 || image.format == RasterPixelFormat::Gray16
                  ? "pgm"
                  : "ppm")
        : requested;
    if (image.format == RasterPixelFormat::Gray16 && format != "pgm") {
        error = "16-bit images are only writable as PNG, TIFF or PGM";
        return false;
    }
    std::ostringstream header;
    header << (format == "pbm" ? "P4" : (format == "pgm" ? "P5" : "P6")) << "\n"
           << image.width << " " << image.height << "\n";
    if (format != "pbm") {
        header << (image.format == RasterPixelFormat::Gray16 ? 65535 : 255) << "\n";
    }
    const std::string text = header.str();
    bytes.assign(text.begin(), text.end());
    const size_t count = static_cast<size_t>(image.width) * image.height;
    if (format == "pbm") {
        const size_t rowBytes = static_cast<size_t>((image.width + 7) / 8);
        const size_t begin = bytes.size();
        bytes.resize(begin + rowBytes * image.height, 0);
        for (size_t i = 0; i < count; ++i) {
            uint8_t r = 0, g = 0, b = 0;
            pixelRgb(image, i, r, g, b);
            if (luminance(r, g, b) < 128) {
                bytes[begin + (i / image.width) * rowBytes + (i % image.width) / 8]
                    |= static_cast<uint8_t>(0x80 >> (i % 8));
            }
        }
    } else if (format == "pgm" && image.format == RasterPixelFormat::Gray16) {
        const auto* values = reinterpret_cast<const uint16_t*>(image.pixels.data());
        for (size_t i = 0; i < count; ++i) {
            bytes.push_back(static_cast<uint8_t>(values[i] >> 8));
            bytes.push_back(static_cast<uint8_t>(values[i]));
        }
    } else if (format == "pgm") {
        for (size_t i = 0; i < count; ++i) {
            uint8_t r = 0, g = 0, b = 0;
            pixelRgb(image, i, r, g, b);
            bytes.push_back(luminance(r, g, b));
        }
    } else {
        for (size_t i = 0; i < count; ++i) {
            uint8_t r = 0, g = 0, b = 0;
            pixelRgb(image, i, r, g, b);
            bytes.insert(bytes.end(), { r, g, b });
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
