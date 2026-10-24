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
#include <vector>
//=============================================================================
namespace Nelson::NativeLibrary {
inline void
require(bool value, const char* message)
{
    if (!value) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
class Reader
{
public:
    explicit Reader(const std::filesystem::path& path)
        : stream(path, std::ios::binary | std::ios::ate)
    {
        require(stream.good(), "Cannot read native library.");
        const auto end = stream.tellg();
        require(end >= 0, "Cannot measure native library.");
        size = static_cast<uint64_t>(end);
    }

    void
    range(uint64_t offset, uint64_t count) const
    {
        require(offset <= size && count <= size - offset, "Truncated native library.");
    }

    uint64_t
    integer(uint64_t offset, size_t count, bool bigEndian = false)
    {
        require(count > 0 && count <= 8, "Invalid native integer width.");
        range(offset, count);
        std::array<unsigned char, 8> bytes {};
        stream.seekg(static_cast<std::streamoff>(offset));
        stream.read(reinterpret_cast<char*>(bytes.data()), count);
        require(stream.good(), "Cannot read native library metadata.");
        uint64_t result = 0;
        for (size_t i = 0; i < count; ++i) {
            result |= static_cast<uint64_t>(bytes[i]) << (8 * (bigEndian ? count - i - 1 : i));
        }
        return result;
    }

    std::string
    string(uint64_t offset, uint64_t available, bool allowEmpty = false)
    {
        range(offset, available);
        const auto count = static_cast<size_t>(std::min<uint64_t>(available, 4096));
        require(count != 0, "Invalid native library string.");
        std::string text(count, '\0');
        stream.seekg(static_cast<std::streamoff>(offset));
        stream.read(text.data(), count);
        require(stream.good(), "Cannot read native library string.");
        const auto end = text.find('\0');
        require(end != std::string::npos && (allowEmpty || end > 0),
            "Unterminated native library string.");
        text.resize(end);
        return text;
    }

    uint64_t size = 0;

private:
    std::ifstream stream;
};
//=============================================================================
struct Mapping
{
    uint64_t address;
    uint64_t offset;
    uint64_t length;
};
//=============================================================================
inline uint64_t
resolve(Reader& file, const std::vector<Mapping>& mappings, uint64_t address, uint64_t count)
{
    for (const auto& mapping : mappings) {
        if (address >= mapping.address && address - mapping.address <= mapping.length
            && count <= mapping.length - (address - mapping.address)) {
            const auto offset = mapping.offset + address - mapping.address;
            file.range(offset, count);
            return offset;
        }
    }
    throw std::runtime_error("Invalid native library address.");
}
//=============================================================================
inline std::string
mappedString(Reader& file, const std::vector<Mapping>& mappings, uint64_t address)
{
    for (const auto& mapping : mappings) {
        if (address >= mapping.address && address - mapping.address < mapping.length) {
            return file.string(mapping.offset + address - mapping.address,
                mapping.length - (address - mapping.address));
        }
    }
    throw std::runtime_error("Invalid native library string address.");
}
//=============================================================================
struct Information
{
    std::string format;
    std::string architecture;
    std::string identity;
    std::vector<std::string> needed;
    std::vector<std::string> delayed;
    std::vector<std::string> searchPaths;
    std::vector<std::string> rpaths;
    std::vector<std::string> runpaths;
};
//=============================================================================
Information
inspectPE(Reader& file);
Information
inspectELF(Reader& file);
Information
inspectMachO(Reader& file);
//=============================================================================
} // namespace Nelson::NativeLibrary
//=============================================================================
