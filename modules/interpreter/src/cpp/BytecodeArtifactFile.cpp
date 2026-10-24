//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BytecodeIO.hpp"
#include "Nelson_VERSION.h"
#include "characters_encoding.hpp"
#include "picoSHA2/picosha2.h"
#include <array>
#include <cstdio>
#include <filesystem>
#include <fstream>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <dlfcn.h>
#endif
namespace Nelson::BytecodeIO {
//=============================================================================
namespace {
    std::filesystem::path
    nativePath(const std::wstring& filename)
    {
        return std::filesystem::u8path(wstring_to_utf8(filename));
    }
    //=============================================================================
    std::filesystem::path
    interpreterPath()
    {
#ifdef _WIN32
        HMODULE module = nullptr;
        require(GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
                        | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                    reinterpret_cast<LPCWSTR>(&engineFingerprint), &module)
                != 0,
            "Cannot locate the bytecode engine.");
        std::wstring filename(32768, L'\0');
        const auto length
            = GetModuleFileNameW(module, filename.data(), static_cast<DWORD>(filename.size()));
        require(length > 0 && length < filename.size(), "Cannot locate the bytecode engine.");
        filename.resize(length);
        return std::filesystem::path(filename);
#else
        Dl_info info {};
        require(dladdr(reinterpret_cast<const void*>(&engineFingerprint), &info) != 0
                && info.dli_fname != nullptr,
            "Cannot locate the bytecode engine.");
        return std::filesystem::path(info.dli_fname);
#endif
    }
    //=============================================================================
    std::string
    hashFile(const std::filesystem::path& filename)
    {
        std::ifstream input(filename, std::ios::binary);
        require(input.good(), "Cannot read the bytecode engine.");
        picosha2::hash256_one_by_one hash;
        std::array<char, 32768> block;
        while (input) {
            input.read(block.data(), block.size());
            hash.process(block.begin(), block.begin() + input.gcount());
        }
        require(input.eof(), "Cannot read the bytecode engine.");
        hash.finish();
        return picosha2::get_hash_hex_string(hash);
    }
    //=============================================================================
} // namespace
//=============================================================================
std::string
engineFingerprint()
{
    // Exact engine binary identity is computed once. No source checkout is
    // needed on the target, and changing VM semantics invalidates artifacts.
    static const std::string fingerprint = hashFile(interpreterPath());
    return fingerprint;
}
//=============================================================================
std::string
readArtifactPayload(const std::wstring& filename)
{
    std::ifstream input(nativePath(filename), std::ios::binary | std::ios::ate);
    require(input.good(), "Cannot open bytecode artifact.");
    const auto size = input.tellg();
    require(size >= 0 && static_cast<uint64_t>(size) <= maxArtifactBytes,
        "Bytecode artifact size limit exceeded.");
    std::string bytes(static_cast<size_t>(size), '\0');
    input.seekg(0);
    input.read(bytes.data(), static_cast<std::streamsize>(bytes.size()));
    require(input.good(), "Cannot read bytecode artifact.");
    Reader reader(bytes);
    require(read<uint64_t>(reader) == 0x3143424e4e4f534eULL, "Invalid bytecode artifact magic.");
    require(read<uint32_t>(reader) == formatVersion, "Incompatible bytecode format version.");
    require(read<uint32_t>(reader) == 0x01020304, "Incompatible bytecode byte order.");
    require(read<uint8_t>(reader) == sizeof(indexType) && read<uint8_t>(reader) == sizeof(charType),
        "Incompatible bytecode data layout.");
    require(readString(reader) == NELSON_VERSION_STRING, "Incompatible bytecode runtime version.");
    require(readString(reader) == engineFingerprint(), "Incompatible bytecode engine.");
    const auto digest = readString(reader);
    auto payload = readString(reader);
    require(reader.atEnd(), "Unexpected trailing bytecode data.");
    require(digest == picosha2::hash256_hex_string(payload), "Bytecode integrity check failed.");
    return payload;
}
//=============================================================================
void
writeArtifactPayload(const std::wstring& filename, const std::string& payload)
{
    require(payload.size() <= maxArtifactBytes - 1024, "Bytecode artifact size limit exceeded.");
    std::string bytes;
    BinaryWriter writer(bytes);
    writer.writeU64(0x3143424e4e4f534eULL);
    writer.writePod(formatVersion);
    writer.writePod<uint32_t>(0x01020304);
    writer.writeU8(sizeof(indexType));
    writer.writeU8(sizeof(charType));
    writer.writeString(NELSON_VERSION_STRING);
    writer.writeString(engineFingerprint());
    writer.writeString(picosha2::hash256_hex_string(payload));
    writer.writeString(payload);
#ifdef _WIN32
    FILE* output = nullptr;
    const auto status = _wfopen_s(&output, filename.c_str(), L"wbx");
    require(status == 0 && output != nullptr,
        "Cannot create bytecode artifact; destination must not exist.");
#else
    FILE* output = fopen(nativePath(filename).c_str(), "wbx");
    require(output != nullptr, "Cannot create bytecode artifact; destination must not exist.");
#endif
    const bool written = fwrite(bytes.data(), 1, bytes.size(), output) == bytes.size();
    const bool closed = fclose(output) == 0;
    if (!written || !closed) {
        std::error_code ignored;
        std::filesystem::remove(nativePath(filename), ignored);
        throw std::runtime_error("Cannot write bytecode artifact.");
    }
}
//=============================================================================
} // namespace Nelson::BytecodeIO
//=============================================================================
