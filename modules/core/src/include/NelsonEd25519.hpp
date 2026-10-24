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
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr size_t ED25519_SIGNATURE_SIZE = 64;
constexpr size_t ED25519_PUBLIC_KEY_SIZE = 32;
constexpr size_t ED25519_SEED_SIZE = 32;
//=============================================================================
/**
 * Pure Ed25519 (RFC 8032, section 5.1) verification of a signature over the
 * exact bytes of a message. Returns true only when the signature is valid.
 */
NLSCORE_IMPEXP bool
ed25519Verify(const uint8_t* message, size_t messageLength,
    const uint8_t signature[ED25519_SIGNATURE_SIZE],
    const uint8_t publicKey[ED25519_PUBLIC_KEY_SIZE]);
//=============================================================================
/**
 * Pure Ed25519 signature of a message with a 32-byte private seed. The
 * matching public key is returned too. The seed buffer is left unchanged.
 */
NLSCORE_IMPEXP void
ed25519Sign(const uint8_t* message, size_t messageLength, const uint8_t seed[ED25519_SEED_SIZE],
    uint8_t signature[ED25519_SIGNATURE_SIZE], uint8_t publicKey[ED25519_PUBLIC_KEY_SIZE]);
//=============================================================================
/**
 * Reads the whole content of a file as raw bytes. Returns false when the
 * file cannot be opened.
 */
NLSCORE_IMPEXP bool
readFileBytes(const std::wstring& filename, std::vector<uint8_t>& bytes);
//=============================================================================
/**
 * Hexadecimal helpers (lowercase output, case-insensitive input). Decoding
 * returns false when the text is not an even-length hexadecimal string.
 */
NLSCORE_IMPEXP std::wstring
bytesToHexString(const uint8_t* bytes, size_t length);
//=============================================================================
NLSCORE_IMPEXP bool
hexStringToBytes(const std::wstring& hex, std::vector<uint8_t>& bytes);
//=============================================================================
} // namespace Nelson
//=============================================================================
