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
#include <algorithm>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>
//=============================================================================
namespace Nelson::ApplicationArchive {
constexpr size_t trailerSize = 352;
constexpr uint64_t maxArchiveBytes = 1024ULL * 1024 * 1024;
struct Trailer
{
    uint64_t offset = 0;
    uint64_t length = 0;
    bool gui = false;
    bool installedRuntime = false;
    bool externalArchive = false;
    std::string digest;
    std::string runtimeDirectory;
    std::string engineFingerprint;
};
//=============================================================================
inline void
require(bool condition, const char* message)
{
    if (!condition) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
inline uint64_t
readInteger(const uint8_t* bytes, size_t count)
{
    uint64_t value = 0;
    for (size_t i = 0; i < count; ++i) {
        value |= static_cast<uint64_t>(bytes[i]) << (8 * i);
    }
    return value;
}
//=============================================================================
inline void
writeInteger(uint8_t* bytes, uint64_t value, size_t count)
{
    for (size_t i = 0; i < count; ++i) {
        bytes[i] = static_cast<uint8_t>(value >> (8 * i));
    }
}
//=============================================================================
inline bool
validRuntimeDirectory(const std::string& name)
{
    return !name.empty() && name.size() < 256 && name != "." && name != ".." && name.back() != '.'
        && name.back() != ' '
        && std::none_of(
            name.begin(), name.end(),
            [](unsigned char c) {
                return c < 32 || c == '/' || c == '\\' || c == ':' || c == '*' || c == '?'
                    || c == '"' || c == '<' || c == '>' || c == '|';
            });
}
//=============================================================================
inline bool
validDigest(const std::string& value)
{
    return value.size() == 64 && std::all_of(value.begin(), value.end(), [](char c) {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
    });
}
//=============================================================================
inline bool
validRelativePath(const std::string& name)
{
    if (name.empty() || name.front() == '/') {
        return false;
    }
    size_t begin = 0;
    while (begin < name.size()) {
        const auto end = name.find('/', begin);
        auto component = name.substr(begin, end == std::string::npos ? end : end - begin);
        if (!validRuntimeDirectory(component)) {
            return false;
        }
        std::transform(component.begin(), component.end(), component.begin(), [](unsigned char c) {
            return static_cast<char>(c >= 'a' && c <= 'z' ? c - ('a' - 'A') : c);
        });
        const auto base = component.substr(0, component.find('.'));
        if (base == "CON" || base == "PRN" || base == "AUX" || base == "NUL"
            || (base.size() == 4 && (base.substr(0, 3) == "COM" || base.substr(0, 3) == "LPT")
                && base[3] >= '1' && base[3] <= '9')) {
            return false;
        }
        if (end == std::string::npos) {
            break;
        }
        begin = end + 1;
    }
    return true;
}
//=============================================================================
inline Trailer
decode(const std::array<uint8_t, trailerSize>& bytes, uint64_t size)
{
    require(size >= trailerSize, "Missing application archive.");
    const std::string magic = "NLSAPP01";
    require(std::equal(magic.begin(), magic.end(), bytes.begin()),
        "Invalid application archive magic.");
    const auto version = readInteger(bytes.data() + 8, 4);
    require(version >= 1 && version <= 3, "Incompatible application archive version.");
    const auto flags = readInteger(bytes.data() + 12, 4);
    require(version == 1 ? flags <= 1
                         : (version == 2 ? flags == 2 || flags == 3 : flags >= 4 && flags <= 7),
        "Invalid application archive flags.");
    Trailer result;
    result.gui = (flags & 1) != 0;
    result.installedRuntime = (flags & 2) != 0;
    result.externalArchive = (flags & 4) != 0;
    result.offset = readInteger(bytes.data() + 16, 8);
    result.length = readInteger(bytes.data() + 24, 8);
    require(result.offset > 0 && result.offset <= size - trailerSize
            && (result.externalArchive ? result.offset == size - trailerSize
                                       : result.length == size - trailerSize - result.offset)
            && result.length >= 22 && result.length <= maxArchiveBytes,
        "Invalid application archive bounds.");
    result.digest.assign(reinterpret_cast<const char*>(bytes.data() + 32), 64);
    require(validDigest(result.digest), "Invalid application archive digest.");
    auto end = std::find(bytes.begin() + 96, bytes.end(), 0);
    require(end != bytes.end() && std::all_of(end, bytes.end(), [](uint8_t c) { return c == 0; }),
        "Invalid application runtime identifier padding.");
    if (result.installedRuntime) {
        result.engineFingerprint.assign(bytes.begin() + 96, end);
        require(validDigest(result.engineFingerprint), "Invalid application engine fingerprint.");
    } else {
        result.runtimeDirectory.assign(bytes.begin() + 96, end);
        require(validRuntimeDirectory(result.runtimeDirectory),
            "Invalid application runtime directory.");
    }
    return result;
}
//=============================================================================
inline Trailer
read(const std::filesystem::path& filename)
{
    std::ifstream input(filename, std::ios::binary | std::ios::ate);
    require(input.good(), "Cannot open application executable.");
    const auto position = input.tellg();
    require(position >= static_cast<std::streamoff>(trailerSize), "Missing application archive.");
    const auto size = static_cast<uint64_t>(position);
    input.seekg(-static_cast<std::streamoff>(trailerSize), std::ios::end);
    std::array<uint8_t, trailerSize> bytes {};
    input.read(reinterpret_cast<char*>(bytes.data()), bytes.size());
    require(input.good(), "Cannot read application trailer.");
    return decode(bytes, size);
}
//=============================================================================
inline std::filesystem::path
externalArchivePath(const std::filesystem::path& executable)
{
    auto archive = executable;
    archive.replace_extension(".nca");
    return archive;
}
//=============================================================================
} // namespace Nelson::ApplicationArchive
//=============================================================================
