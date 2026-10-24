//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _WIN32
#include <windows.h>
#include <bcrypt.h>
#pragma comment(lib, "bcrypt.lib")
#endif
#include <picosha2.h>
#include <fstream>
#include <vector>
#include "NelsonSHA256.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Files are hashed in fixed-size chunks: large runtime libraries never have to
// fit in memory, and the chunk loop feeds whichever digest engine is fastest.
static constexpr std::streamsize CHUNK_SIZE = 1 << 20;
//=============================================================================
static std::string
hexDigest(const std::vector<unsigned char>& digest);
static bool
hashFileWithPicosha(std::ifstream& inputFile, std::string& hex);
#ifdef _WIN32
static bool
hashFileWithBCrypt(std::ifstream& inputFile, std::string& hex);
#endif
//=============================================================================
std::wstring
computeStringToSHA256(const std::wstring& str)
{
    std::string sha256;
    picosha2::hash256_hex_string(wstring_to_utf8(str), sha256);
    return utf8_to_wstring(sha256);
}
//=============================================================================
std::wstring
computeFileToSHA256(const std::wstring& filename)
{
#ifdef _MSC_VER
    std::ifstream inputFile(filename.c_str(), std::ios::binary);
#else
    std::ifstream inputFile(wstring_to_utf8(filename).c_str(), std::ios::binary);
#endif
    std::string sha256;
    if (!inputFile.is_open()) {
        return {};
    }
#ifdef _WIN32
    // The platform digest engine uses SHA extensions when the CPU has them.
    if (hashFileWithBCrypt(inputFile, sha256)) {
        return utf8_to_wstring(sha256);
    }
    inputFile.clear();
    inputFile.seekg(0, std::ios::beg);
#endif
    hashFileWithPicosha(inputFile, sha256);
    return utf8_to_wstring(sha256);
}
//=============================================================================
static std::string
hexDigest(const std::vector<unsigned char>& digest)
{
    std::string hex;
    picosha2::bytes_to_hex_string(digest.begin(), digest.end(), hex);
    return hex;
}
//=============================================================================
static bool
hashFileWithPicosha(std::ifstream& inputFile, std::string& hex)
{
    picosha2::hash256_one_by_one hasher;
    hasher.init();
    std::vector<unsigned char> buffer(static_cast<size_t>(CHUNK_SIZE));
    while (inputFile) {
        inputFile.read(reinterpret_cast<char*>(buffer.data()), CHUNK_SIZE);
        std::streamsize count = inputFile.gcount();
        if (count > 0) {
            hasher.process(buffer.begin(), buffer.begin() + count);
        }
    }
    hasher.finish();
    std::vector<unsigned char> digest(picosha2::k_digest_size);
    hasher.get_hash_bytes(digest.begin(), digest.end());
    hex = hexDigest(digest);
    return true;
}
//=============================================================================
#ifdef _WIN32
static bool
hashFileWithBCrypt(std::ifstream& inputFile, std::string& hex)
{
    BCRYPT_ALG_HANDLE algorithm = nullptr;
    if (!BCRYPT_SUCCESS(
            BCryptOpenAlgorithmProvider(&algorithm, BCRYPT_SHA256_ALGORITHM, nullptr, 0))) {
        return false;
    }
    BCRYPT_HASH_HANDLE hash = nullptr;
    bool ok = BCRYPT_SUCCESS(BCryptCreateHash(algorithm, &hash, nullptr, 0, nullptr, 0, 0));
    std::vector<unsigned char> buffer(static_cast<size_t>(CHUNK_SIZE));
    while (ok && inputFile) {
        inputFile.read(reinterpret_cast<char*>(buffer.data()), CHUNK_SIZE);
        std::streamsize count = inputFile.gcount();
        if (count > 0) {
            ok = BCRYPT_SUCCESS(BCryptHashData(hash, buffer.data(), static_cast<ULONG>(count), 0));
        }
    }
    std::vector<unsigned char> digest(32);
    if (ok) {
        ok = BCRYPT_SUCCESS(
            BCryptFinishHash(hash, digest.data(), static_cast<ULONG>(digest.size()), 0));
    }
    if (hash != nullptr) {
        BCryptDestroyHash(hash);
    }
    BCryptCloseAlgorithmProvider(algorithm, 0);
    if (ok) {
        hex = hexDigest(digest);
    }
    return ok;
}
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
