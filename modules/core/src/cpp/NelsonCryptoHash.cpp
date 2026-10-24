//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cwctype>
#include <new>
#include <vector>
#if defined(_WIN32)
#include <windows.h>
#include <bcrypt.h>
#pragma comment(lib, "bcrypt.lib")
#else
#include <cstdio>
#endif
#include <picosha2.h>
#include "monocypher.h"
#include "monocypher-ed25519.h"
#include "NelsonCryptoHash.hpp"
#include "NelsonEd25519.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
sha256Bytes(const uint8_t* data, size_t length, std::vector<uint8_t>& digest);
static std::wstring
hmacSha256Hex(const uint8_t* key, size_t keyLength, const uint8_t* data, size_t length);
//=============================================================================
std::wstring
computeBytesToSHA256Hex(const uint8_t* data, size_t length)
{
    std::vector<uint8_t> digest;
    sha256Bytes(data, length, digest);
    return bytesToHexString(digest.data(), digest.size());
}
//=============================================================================
std::wstring
computeBytesToSHA512Hex(const uint8_t* data, size_t length)
{
    static const uint8_t empty[1] = { 0 };
    uint8_t digest[64];
    crypto_sha512(digest, (length == 0) ? empty : data, length);
    return bytesToHexString(digest, sizeof(digest));
}
//=============================================================================
bool
hmacAlgorithmFromName(const std::wstring& name, HmacAlgorithm& algorithm)
{
    std::wstring lowered(name);
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    if (lowered == L"sha256" || lowered == L"sha-256") {
        algorithm = HmacAlgorithm::SHA256;
        return true;
    }
    if (lowered == L"sha512" || lowered == L"sha-512") {
        algorithm = HmacAlgorithm::SHA512;
        return true;
    }
    return false;
}
//=============================================================================
std::wstring
computeHmacHex(HmacAlgorithm algorithm, const uint8_t* key, size_t keyLength, const uint8_t* data,
    size_t length)
{
    static const uint8_t empty[1] = { 0 };
    const uint8_t* keyPtr = (keyLength == 0) ? empty : key;
    const uint8_t* dataPtr = (length == 0) ? empty : data;
    if (algorithm == HmacAlgorithm::SHA512) {
        uint8_t mac[64];
        crypto_sha512_hmac(mac, keyPtr, keyLength, dataPtr, length);
        return bytesToHexString(mac, sizeof(mac));
    }
    return hmacSha256Hex(keyPtr, keyLength, dataPtr, length);
}
//=============================================================================
std::wstring
computeBlake2bHex(
    const uint8_t* data, size_t length, const uint8_t* key, size_t keyLength, size_t digestSize)
{
    static const uint8_t empty[1] = { 0 };
    uint8_t digest[64];
    const uint8_t* dataPtr = (length == 0) ? empty : data;
    if (keyLength == 0) {
        crypto_blake2b(digest, digestSize, dataPtr, length);
    } else {
        crypto_blake2b_keyed(digest, digestSize, key, keyLength, dataPtr, length);
    }
    return bytesToHexString(digest, digestSize);
}
//=============================================================================
bool
argon2VariantFromName(const std::wstring& name, Argon2Variant& variant)
{
    std::wstring lowered(name);
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    if (lowered == L"argon2id") {
        variant = Argon2Variant::ARGON2ID;
        return true;
    }
    if (lowered == L"argon2i") {
        variant = Argon2Variant::ARGON2I;
        return true;
    }
    if (lowered == L"argon2d") {
        variant = Argon2Variant::ARGON2D;
        return true;
    }
    return false;
}
//=============================================================================
std::wstring
computeArgon2Hex(const Argon2Parameters& parameters, const uint8_t* password, size_t passwordLength,
    const uint8_t* salt, size_t saltLength, const uint8_t* key, size_t keyLength,
    const uint8_t* associatedData, size_t associatedDataLength)
{
    static const uint8_t empty[1] = { 0 };
    std::vector<uint8_t> workArea;
    try {
        workArea.resize(static_cast<size_t>(parameters.memoryKiB) * 1024);
    } catch (const std::bad_alloc&) {
        return L"";
    }
    crypto_argon2_config config;
    switch (parameters.variant) {
    case Argon2Variant::ARGON2D:
        config.algorithm = CRYPTO_ARGON2_D;
        break;
    case Argon2Variant::ARGON2I:
        config.algorithm = CRYPTO_ARGON2_I;
        break;
    default:
        config.algorithm = CRYPTO_ARGON2_ID;
        break;
    }
    config.nb_blocks = parameters.memoryKiB;
    config.nb_passes = parameters.passes;
    config.nb_lanes = parameters.lanes;
    crypto_argon2_inputs inputs;
    inputs.pass = (passwordLength == 0) ? empty : password;
    inputs.pass_size = static_cast<uint32_t>(passwordLength);
    inputs.salt = salt;
    inputs.salt_size = static_cast<uint32_t>(saltLength);
    crypto_argon2_extras extras;
    extras.key = (keyLength == 0) ? nullptr : key;
    extras.key_size = static_cast<uint32_t>(keyLength);
    extras.ad = (associatedDataLength == 0) ? nullptr : associatedData;
    extras.ad_size = static_cast<uint32_t>(associatedDataLength);
    std::vector<uint8_t> hash(parameters.length);
    crypto_argon2(hash.data(), parameters.length, workArea.data(), config, inputs, extras);
    crypto_wipe(workArea.data(), workArea.size());
    return bytesToHexString(hash.data(), hash.size());
}
//=============================================================================
bool
secureRandomBytes(uint8_t* out, size_t length)
{
    if (length == 0) {
        return true;
    }
#if defined(_WIN32)
    NTSTATUS status = BCryptGenRandom(
        nullptr, out, static_cast<ULONG>(length), BCRYPT_USE_SYSTEM_PREFERRED_RNG);
    return status == 0;
#else
    std::FILE* stream = std::fopen("/dev/urandom", "rb");
    if (stream == nullptr) {
        return false;
    }
    size_t read = std::fread(out, 1, length, stream);
    std::fclose(stream);
    return read == length;
#endif
}
//=============================================================================
void
x25519PublicKey(
    const uint8_t secretKey[CRYPTO_X25519_KEY_SIZE], uint8_t publicKey[CRYPTO_X25519_KEY_SIZE])
{
    crypto_x25519_public_key(publicKey, secretKey);
}
//=============================================================================
void
x25519Shared(const uint8_t secretKey[CRYPTO_X25519_KEY_SIZE],
    const uint8_t peerPublicKey[CRYPTO_X25519_KEY_SIZE],
    uint8_t sharedSecret[CRYPTO_X25519_KEY_SIZE])
{
    crypto_x25519(sharedSecret, secretKey, peerPublicKey);
}
//=============================================================================
void
aeadEncrypt(const uint8_t key[CRYPTO_AEAD_KEY_SIZE], const uint8_t nonce[CRYPTO_AEAD_NONCE_SIZE],
    const uint8_t* associatedData, size_t associatedDataLength, const uint8_t* plaintext,
    size_t plaintextLength, uint8_t* ciphertext, uint8_t mac[CRYPTO_AEAD_MAC_SIZE])
{
    crypto_aead_lock(ciphertext, mac, key, nonce, associatedData, associatedDataLength, plaintext,
        plaintextLength);
}
//=============================================================================
bool
aeadDecrypt(const uint8_t key[CRYPTO_AEAD_KEY_SIZE], const uint8_t nonce[CRYPTO_AEAD_NONCE_SIZE],
    const uint8_t* associatedData, size_t associatedDataLength,
    const uint8_t mac[CRYPTO_AEAD_MAC_SIZE], const uint8_t* ciphertext, size_t ciphertextLength,
    uint8_t* plaintext)
{
    return crypto_aead_unlock(plaintext, mac, key, nonce, associatedData, associatedDataLength,
               ciphertext, ciphertextLength)
        == 0;
}
//=============================================================================
static void
sha256Bytes(const uint8_t* data, size_t length, std::vector<uint8_t>& digest)
{
    digest.assign(picosha2::k_digest_size, 0);
    picosha2::hash256(data, data + length, digest.begin(), digest.end());
}
//=============================================================================
// HMAC-SHA256 (RFC 2104) on top of picoSHA2: block size 64 bytes.
static std::wstring
hmacSha256Hex(const uint8_t* key, size_t keyLength, const uint8_t* data, size_t length)
{
    const size_t blockSize = 64;
    std::vector<uint8_t> normalizedKey(key, key + keyLength);
    if (normalizedKey.size() > blockSize) {
        std::vector<uint8_t> hashed;
        sha256Bytes(normalizedKey.data(), normalizedKey.size(), hashed);
        normalizedKey = hashed;
    }
    normalizedKey.resize(blockSize, 0);
    std::vector<uint8_t> inner(blockSize + length);
    std::vector<uint8_t> outer(blockSize + picosha2::k_digest_size);
    for (size_t k = 0; k < blockSize; ++k) {
        inner[k] = normalizedKey[k] ^ 0x36;
        outer[k] = normalizedKey[k] ^ 0x5c;
    }
    std::copy(data, data + length, inner.begin() + blockSize);
    std::vector<uint8_t> innerDigest;
    sha256Bytes(inner.data(), inner.size(), innerDigest);
    std::copy(innerDigest.begin(), innerDigest.end(), outer.begin() + blockSize);
    std::vector<uint8_t> mac;
    sha256Bytes(outer.data(), outer.size(), mac);
    return bytesToHexString(mac.data(), mac.size());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
