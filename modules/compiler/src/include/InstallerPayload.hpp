//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ApplicationArchiveFormat.hpp"
#include <cstring>
//=============================================================================
namespace Nelson::Installer {
constexpr size_t footerSize = 96;
constexpr char signature[16] = "NLS.INSTALLER.1";
constexpr char runtimeSignature[16] = "NLS.RUNTIME.1";
struct Payload
{
    uint64_t offset;
    uint64_t length;
    std::string digest;
    bool runtime;
};
//=============================================================================
inline uint64_t
containerEnd(std::istream& input, uint64_t size)
{
    using ApplicationArchive::readInteger;
    using ApplicationArchive::require;
    std::array<uint8_t, 264> header {};
    input.seekg(0);
    input.read(reinterpret_cast<char*>(header.data()), 64);
    require(input.good() && header[0] == 'M' && header[1] == 'Z', "Invalid installer executable.");
    const auto pe = readInteger(header.data() + 60, 4);
    require(pe <= size && size - pe >= header.size(), "Invalid installer executable header.");
    input.seekg(pe);
    input.read(reinterpret_cast<char*>(header.data()), header.size());
    require(input.good() && std::memcmp(header.data(), "PE\0\0", 4) == 0,
        "Invalid installer executable signature.");
    const auto magic = readInteger(header.data() + 24, 2);
    require(magic == 0x20b || magic == 0x10b, "Unsupported installer executable format.");
    const size_t directory = 24 + (magic == 0x20b ? 112 : 96) + 4 * 8;
    require(readInteger(header.data() + 20, 2) >= directory + 8 - 24,
        "Invalid installer executable directories.");
    const auto certificate = readInteger(header.data() + directory, 4);
    const auto certificateSize = readInteger(header.data() + directory + 4, 4);
    if (certificate == 0 && certificateSize == 0) {
        return size;
    }
    require(certificate % 8 == 0 && certificate <= size && certificateSize == size - certificate,
        "Invalid installer certificate bounds.");
    return certificate;
}
//=============================================================================
inline Payload
readPayload(std::istream& input, uint64_t size)
{
    using ApplicationArchive::require;
    const auto end = containerEnd(input, size);
    require(end >= footerSize, "Installer payload is missing.");
    std::array<uint8_t, footerSize> footer {};
    input.seekg(end - footerSize);
    input.read(reinterpret_cast<char*>(footer.data()), footer.size());
    const bool runtime
        = std::memcmp(footer.data(), runtimeSignature, sizeof(runtimeSignature)) == 0;
    require(
        input.good() && (runtime || std::memcmp(footer.data(), signature, sizeof(signature)) == 0),
        "Installer payload is missing or damaged.");
    Payload payload { ApplicationArchive::readInteger(footer.data() + 16, 8),
        ApplicationArchive::readInteger(footer.data() + 24, 8),
        std::string(reinterpret_cast<char*>(footer.data() + 32), 64), runtime };
    require(payload.offset >= 512 && payload.offset <= end - footerSize && payload.length >= 64
            && payload.length <= end - footerSize - payload.offset
            && end - footerSize - payload.offset - payload.length < 8
            && ApplicationArchive::validDigest(payload.digest),
        "Invalid installer payload bounds.");
    input.seekg(payload.offset + payload.length);
    for (auto position = payload.offset + payload.length; position < end - footerSize; ++position) {
        require(input.get() == 0, "Invalid installer payload padding.");
    }
    input.seekg(payload.offset);
    return payload;
}
//=============================================================================
} // namespace Nelson::Installer
//=============================================================================
