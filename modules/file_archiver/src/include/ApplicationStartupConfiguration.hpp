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
#include <vector>
//=============================================================================
namespace Nelson::ApplicationStartup {
constexpr size_t recordSize = 40;
constexpr size_t footerSize = 24;
constexpr size_t maxPathBytes = 131072;
constexpr std::array<uint8_t, 16> signature
    = { 'N', 'L', 'S', 'S', 'T', 'A', 'R', 'T', 'U', 'P', 'C', 'O', 'N', 'F', '0', '1' };
constexpr std::array<uint8_t, 16> footerSignature
    = { 'N', 'L', 'S', 'L', 'O', 'G', 'C', 'O', 'N', 'F', 'I', 'G', '0', '0', '0', '1' };
struct Location
{
    uint64_t offset = 0;
    uint64_t length = 0;
};
//=============================================================================
constexpr std::array<uint8_t, recordSize>
record()
{
    std::array<uint8_t, recordSize> bytes {};
    for (size_t i = 0; i < signature.size(); ++i) {
        bytes[i] = signature[i];
    }
    bytes[16] = recordSize;
    bytes[20] = 1;
    return bytes;
}
//=============================================================================
inline Location
decode(const uint8_t* bytes)
{
    using ApplicationArchive::readInteger;
    using ApplicationArchive::require;
    require(std::equal(signature.begin(), signature.end(), bytes)
            && readInteger(bytes + 16, 4) == recordSize && readInteger(bytes + 20, 4) == 1,
        "Invalid application startup configuration record.");
    Location result { readInteger(bytes + 24, 8), readInteger(bytes + 32, 8) };
    require((result.offset == 0 && result.length == 0)
            || (result.offset >= recordSize && result.offset <= 64ULL * 1024 * 1024
                && result.length >= 9 && result.length <= maxPathBytes + 8),
        "Invalid application startup configuration bounds.");
    return result;
}
//=============================================================================
inline Location
embeddedLocation(const std::array<uint8_t, recordSize>& record)
{
    std::array<uint8_t, recordSize> bytes {};
    const volatile uint8_t* source = record.data();
    for (size_t i = 0; i < bytes.size(); ++i) {
        bytes[i] = source[i];
    }
    return decode(bytes.data());
}
//=============================================================================
inline void
validateLogFilename(const std::string& filename)
{
    ApplicationArchive::require(filename.size() <= maxPathBytes
            && std::none_of(
                filename.begin(), filename.end(), [](unsigned char c) { return c < 32; }),
        "Invalid application runtime log filename.");
#ifdef _WIN32
    if (!filename.empty()) {
        const auto path = std::filesystem::u8path(filename);
        ApplicationArchive::require(path.has_root_name() == path.has_root_directory(),
            "Runtime log paths must be relative or fully qualified, not drive-relative.");
    }
#endif
}
//=============================================================================
inline std::vector<uint8_t>
encodeLogFilename(const std::string& filename)
{
    validateLogFilename(filename);
    ApplicationArchive::require(
        !filename.empty(), "An application runtime log filename is required.");
    std::vector<uint8_t> bytes(8 + filename.size());
    ApplicationArchive::writeInteger(bytes.data(), 1, 4);
    ApplicationArchive::writeInteger(bytes.data() + 4, filename.size(), 4);
    std::copy(filename.begin(), filename.end(), bytes.begin() + 8);
    return bytes;
}
//=============================================================================
inline std::string
decodeLogFilename(const std::vector<uint8_t>& bytes)
{
    ApplicationArchive::require(bytes.size() >= 9 && bytes.size() <= maxPathBytes + 8
            && ApplicationArchive::readInteger(bytes.data(), 4) == 1
            && ApplicationArchive::readInteger(bytes.data() + 4, 4) == bytes.size() - 8,
        "Invalid application runtime log configuration.");
    std::string filename(bytes.begin() + 8, bytes.end());
    validateLogFilename(filename);
    return filename;
}
//=============================================================================
inline std::array<uint8_t, footerSize>
footer(size_t length)
{
    ApplicationArchive::require(
        length >= 9 && length <= maxPathBytes + 8, "Invalid application runtime log size.");
    std::array<uint8_t, footerSize> bytes {};
    std::copy(footerSignature.begin(), footerSignature.end(), bytes.begin());
    ApplicationArchive::writeInteger(bytes.data() + footerSignature.size(), length, 8);
    return bytes;
}
//=============================================================================
inline uint64_t
decodeFooter(const std::array<uint8_t, footerSize>& bytes)
{
    ApplicationArchive::require(
        std::equal(footerSignature.begin(), footerSignature.end(), bytes.begin()),
        "Invalid application runtime log footer.");
    const auto length = ApplicationArchive::readInteger(bytes.data() + footerSignature.size(), 8);
    ApplicationArchive::require(
        length >= 9 && length <= maxPathBytes + 8, "Invalid application runtime log size.");
    return length;
}
//=============================================================================
} // namespace Nelson::ApplicationStartup
//=============================================================================
