//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <fstream>
#include <iterator>
#include "monocypher.h"
#include "monocypher-ed25519.h"
#include "NelsonEd25519.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
hexDigitValue(wchar_t c);
//=============================================================================
bool
ed25519Verify(const uint8_t* message, size_t messageLength,
    const uint8_t signature[ED25519_SIGNATURE_SIZE],
    const uint8_t publicKey[ED25519_PUBLIC_KEY_SIZE])
{
    static const uint8_t emptyMessage[1] = { 0 };
    const uint8_t* data = (messageLength == 0) ? emptyMessage : message;
    return crypto_ed25519_check(signature, publicKey, data, messageLength) == 0;
}
//=============================================================================
void
ed25519Sign(const uint8_t* message, size_t messageLength, const uint8_t seed[ED25519_SEED_SIZE],
    uint8_t signature[ED25519_SIGNATURE_SIZE], uint8_t publicKey[ED25519_PUBLIC_KEY_SIZE])
{
    static const uint8_t emptyMessage[1] = { 0 };
    const uint8_t* data = (messageLength == 0) ? emptyMessage : message;
    // crypto_ed25519_key_pair wipes the seed it receives: work on a copy.
    uint8_t seedCopy[ED25519_SEED_SIZE];
    uint8_t secretKey[ED25519_SIGNATURE_SIZE];
    memcpy(seedCopy, seed, ED25519_SEED_SIZE);
    crypto_ed25519_key_pair(secretKey, publicKey, seedCopy);
    crypto_ed25519_sign(signature, secretKey, data, messageLength);
    crypto_wipe(secretKey, sizeof(secretKey));
}
//=============================================================================
bool
readFileBytes(const std::wstring& filename, std::vector<uint8_t>& bytes)
{
    bytes.clear();
#ifdef _MSC_VER
    std::ifstream inputFile(filename.c_str(), std::ios::binary);
#else
    std::ifstream inputFile(wstring_to_utf8(filename).c_str(), std::ios::binary);
#endif
    if (!inputFile.is_open()) {
        return false;
    }
    bytes.assign((std::istreambuf_iterator<char>(inputFile)), std::istreambuf_iterator<char>());
    inputFile.close();
    return true;
}
//=============================================================================
std::wstring
bytesToHexString(const uint8_t* bytes, size_t length)
{
    static const wchar_t* digits = L"0123456789abcdef";
    std::wstring hex;
    hex.reserve(length * 2);
    for (size_t k = 0; k < length; ++k) {
        hex.push_back(digits[bytes[k] >> 4]);
        hex.push_back(digits[bytes[k] & 0x0F]);
    }
    return hex;
}
//=============================================================================
bool
hexStringToBytes(const std::wstring& hex, std::vector<uint8_t>& bytes)
{
    bytes.clear();
    if (hex.size() % 2 != 0) {
        return false;
    }
    bytes.reserve(hex.size() / 2);
    for (size_t k = 0; k < hex.size(); k += 2) {
        int high = hexDigitValue(hex[k]);
        int low = hexDigitValue(hex[k + 1]);
        if (high < 0 || low < 0) {
            bytes.clear();
            return false;
        }
        bytes.push_back(static_cast<uint8_t>((high << 4) | low));
    }
    return true;
}
//=============================================================================
static int
hexDigitValue(wchar_t c)
{
    if (c >= L'0' && c <= L'9') {
        return c - L'0';
    }
    if (c >= L'a' && c <= L'f') {
        return 10 + (c - L'a');
    }
    if (c >= L'A' && c <= L'F') {
        return 10 + (c - L'A');
    }
    return -1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
