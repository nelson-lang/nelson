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
#include "nlsCore_exports.h"
#include <cstddef>
#include <cstdint>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class HmacAlgorithm
{
    SHA256,
    SHA512
};
//=============================================================================
/**
 * SHA-256 (picoSHA2) and SHA-512 (Monocypher) digests of raw bytes, as
 * lowercase hexadecimal text.
 */
NLSCORE_IMPEXP std::wstring
computeBytesToSHA256Hex(const uint8_t* data, size_t length);
//=============================================================================
NLSCORE_IMPEXP std::wstring
computeBytesToSHA512Hex(const uint8_t* data, size_t length);
//=============================================================================
/**
 * Maps 'sha256' / 'sha512' (case-insensitive) to an algorithm. Returns false
 * for any other name.
 */
NLSCORE_IMPEXP bool
hmacAlgorithmFromName(const std::wstring& name, HmacAlgorithm& algorithm);
//=============================================================================
/**
 * HMAC (RFC 2104) of raw bytes with a raw key, as lowercase hexadecimal text.
 */
NLSCORE_IMPEXP std::wstring
computeHmacHex(HmacAlgorithm algorithm, const uint8_t* key, size_t keyLength, const uint8_t* data,
    size_t length);
//=============================================================================
/**
 * BLAKE2b (RFC 7693) digest of 1 to 64 bytes, optionally keyed (key of at most
 * 64 bytes), as lowercase hexadecimal text.
 */
NLSCORE_IMPEXP std::wstring
computeBlake2bHex(
    const uint8_t* data, size_t length, const uint8_t* key, size_t keyLength, size_t digestSize);
//=============================================================================
enum class Argon2Variant
{
    ARGON2D,
    ARGON2I,
    ARGON2ID
};
//=============================================================================
struct Argon2Parameters
{
    Argon2Variant variant = Argon2Variant::ARGON2ID;
    uint32_t memoryKiB = 65536;
    uint32_t passes = 3;
    uint32_t lanes = 1;
    uint32_t length = 32;
};
//=============================================================================
/**
 * Maps 'argon2id' / 'argon2i' / 'argon2d' (case-insensitive) to a variant.
 */
NLSCORE_IMPEXP bool
argon2VariantFromName(const std::wstring& name, Argon2Variant& variant);
//=============================================================================
/**
 * Argon2 (RFC 9106) password hash as lowercase hexadecimal text. The caller
 * validates the parameters (salt >= 8 bytes, memory >= 8 * lanes, passes >= 1,
 * lanes >= 1, length >= 4). Returns an empty string when the work area cannot
 * be allocated.
 */
NLSCORE_IMPEXP std::wstring
computeArgon2Hex(const Argon2Parameters& parameters, const uint8_t* password, size_t passwordLength,
    const uint8_t* salt, size_t saltLength, const uint8_t* key, size_t keyLength,
    const uint8_t* associatedData, size_t associatedDataLength);
//=============================================================================
constexpr size_t CRYPTO_X25519_KEY_SIZE = 32;
constexpr size_t CRYPTO_AEAD_KEY_SIZE = 32;
constexpr size_t CRYPTO_AEAD_NONCE_SIZE = 24;
constexpr size_t CRYPTO_AEAD_MAC_SIZE = 16;
//=============================================================================
/**
 * Fills the buffer with cryptographically secure random bytes. Returns false
 * when the operating system random source is unavailable.
 */
NLSCORE_IMPEXP bool
secureRandomBytes(uint8_t* out, size_t length);
//=============================================================================
/**
 * X25519 (RFC 7748): public key from a 32-byte secret, and raw 32-byte shared
 * secret between a secret and a peer public key. Hash the shared secret (for
 * example with BLAKE2b) before using it as a key.
 */
NLSCORE_IMPEXP void
x25519PublicKey(
    const uint8_t secretKey[CRYPTO_X25519_KEY_SIZE], uint8_t publicKey[CRYPTO_X25519_KEY_SIZE]);
//=============================================================================
NLSCORE_IMPEXP void
x25519Shared(const uint8_t secretKey[CRYPTO_X25519_KEY_SIZE],
    const uint8_t peerPublicKey[CRYPTO_X25519_KEY_SIZE],
    uint8_t sharedSecret[CRYPTO_X25519_KEY_SIZE]);
//=============================================================================
/**
 * XChaCha20-Poly1305 authenticated encryption (24-byte nonce). The MAC (16
 * bytes) authenticates both the ciphertext and the optional associated data.
 * aeadDecrypt returns false when authentication fails.
 */
NLSCORE_IMPEXP void
aeadEncrypt(const uint8_t key[CRYPTO_AEAD_KEY_SIZE], const uint8_t nonce[CRYPTO_AEAD_NONCE_SIZE],
    const uint8_t* associatedData, size_t associatedDataLength, const uint8_t* plaintext,
    size_t plaintextLength, uint8_t* ciphertext, uint8_t mac[CRYPTO_AEAD_MAC_SIZE]);
//=============================================================================
NLSCORE_IMPEXP bool
aeadDecrypt(const uint8_t key[CRYPTO_AEAD_KEY_SIZE], const uint8_t nonce[CRYPTO_AEAD_NONCE_SIZE],
    const uint8_t* associatedData, size_t associatedDataLength,
    const uint8_t mac[CRYPTO_AEAD_MAC_SIZE], const uint8_t* ciphertext, size_t ciphertextLength,
    uint8_t* plaintext);
//=============================================================================
} // namespace Nelson
//=============================================================================
