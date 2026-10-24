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
#ifdef _WIN32
#include <windows.h>
#include <bcrypt.h>
#include "ApplicationArchiveFormat.hpp"
#include <cstdio>
//=============================================================================
namespace Nelson::Installer {
class Hash
{
public:
    Hash()
    {
        ApplicationArchive::require(
            BCryptOpenAlgorithmProvider(&algorithm, BCRYPT_SHA256_ALGORITHM, nullptr, 0) == 0,
            "Cannot initialize installer checksum provider.");
        if (BCryptCreateHash(algorithm, &hash, nullptr, 0, nullptr, 0, 0) != 0) {
            BCryptCloseAlgorithmProvider(algorithm, 0);
            throw std::runtime_error("Cannot initialize installer checksum.");
        }
    }
    ~Hash()
    {
        BCryptDestroyHash(hash);
        BCryptCloseAlgorithmProvider(algorithm, 0);
    }
    Hash(const Hash&) = delete;
    Hash&
    operator=(const Hash&)
        = delete;
    void
    add(uint8_t* bytes, ULONG size)
    {
        ApplicationArchive::require(
            BCryptHashData(hash, bytes, size, 0) == 0, "Cannot update installer checksum.");
    }
    std::string
    finish()
    {
        std::array<uint8_t, 32> bytes {};
        ApplicationArchive::require(
            BCryptFinishHash(hash, bytes.data(), static_cast<ULONG>(bytes.size()), 0) == 0,
            "Cannot finish installer checksum.");
        std::string result;
        for (const auto byte : bytes) {
            result += "0123456789abcdef"[byte >> 4];
            result += "0123456789abcdef"[byte & 15];
        }
        return result;
    }

private:
    BCRYPT_ALG_HANDLE algorithm = nullptr;
    BCRYPT_HASH_HANDLE hash = nullptr;
};
//=============================================================================
inline std::string
transfer(std::istream& input, FILE* output, uint64_t length)
{
    Hash hash;
    std::array<uint8_t, 32768> block {};
    while (length > 0) {
        const auto count = static_cast<size_t>(std::min<uint64_t>(block.size(), length));
        input.read(reinterpret_cast<char*>(block.data()), count);
        ApplicationArchive::require(input.good(), "Cannot read installer payload.");
        ApplicationArchive::require(
            fwrite(block.data(), 1, count, output) == count, "Cannot write installer payload.");
        hash.add(block.data(), static_cast<ULONG>(count));
        length -= count;
    }
    return hash.finish();
}
//=============================================================================
} // namespace Nelson::Installer
#endif
//=============================================================================
