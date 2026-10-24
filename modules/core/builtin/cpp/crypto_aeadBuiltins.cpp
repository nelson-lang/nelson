//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "crypto_aeadBuiltins.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "NelsonEd25519.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
// boxed = crypto.aead.encrypt(key, nonce, plaintext)
// boxed = crypto.aead.encrypt(key, nonce, plaintext, associatedData)
// The output is a uint8 row vector: mac (16 bytes) followed by the ciphertext.
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_aead_encryptBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    std::vector<uint8_t> key = cryptoFixedBytes(
        argIn[0], CRYPTO_AEAD_KEY_SIZE, L"Nelson:core:aeadKeySizeExpected", L"Key", 1);
    std::vector<uint8_t> nonce = cryptoFixedBytes(
        argIn[1], CRYPTO_AEAD_NONCE_SIZE, L"Nelson:core:aeadNonceSizeExpected", L"Nonce", 2);
    std::vector<uint8_t> plaintext = cryptoMessageBytes(argIn[2], false, 3);
    std::vector<uint8_t> associatedData;
    if (argIn.size() == 4) {
        associatedData = cryptoMessageBytes(argIn[3], false, 4);
    }
    std::vector<uint8_t> boxed(CRYPTO_AEAD_MAC_SIZE + plaintext.size());
    uint8_t mac[CRYPTO_AEAD_MAC_SIZE];
    aeadEncrypt(key.data(), nonce.data(), associatedData.data(), associatedData.size(),
        plaintext.data(), plaintext.size(), boxed.data() + CRYPTO_AEAD_MAC_SIZE, mac);
    std::copy(mac, mac + CRYPTO_AEAD_MAC_SIZE, boxed.begin());
    retval << ArrayOf::uint8RowVector(boxed);
    return retval;
}
//=============================================================================
// plaintext = crypto.aead.decrypt(key, nonce, boxed)
// plaintext = crypto.aead.decrypt(key, nonce, boxed, associatedData)
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_aead_decryptBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    std::vector<uint8_t> key = cryptoFixedBytes(
        argIn[0], CRYPTO_AEAD_KEY_SIZE, L"Nelson:core:aeadKeySizeExpected", L"Key", 1);
    std::vector<uint8_t> nonce = cryptoFixedBytes(
        argIn[1], CRYPTO_AEAD_NONCE_SIZE, L"Nelson:core:aeadNonceSizeExpected", L"Nonce", 2);
    std::vector<uint8_t> boxed = cryptoHexOrBytes(argIn[2], 3);
    if (boxed.size() < CRYPTO_AEAD_MAC_SIZE) {
        Error(L"Nelson:core:aeadCiphertextTooShort",
            L"Encrypted message must be at least 16 bytes (the authentication tag).");
    }
    std::vector<uint8_t> associatedData;
    if (argIn.size() == 4) {
        associatedData = cryptoMessageBytes(argIn[3], false, 4);
    }
    size_t ciphertextLength = boxed.size() - CRYPTO_AEAD_MAC_SIZE;
    std::vector<uint8_t> plaintext(ciphertextLength == 0 ? 1 : ciphertextLength);
    bool ok = aeadDecrypt(key.data(), nonce.data(), associatedData.data(), associatedData.size(),
        boxed.data(), boxed.data() + CRYPTO_AEAD_MAC_SIZE, ciphertextLength, plaintext.data());
    if (!ok) {
        Error(L"Nelson:core:aeadAuthenticationFailed",
            L"Authentication failed: the key, nonce, associated data or encrypted message is "
            L"wrong.");
    }
    plaintext.resize(ciphertextLength);
    retval << ArrayOf::uint8RowVector(plaintext);
    return retval;
}
//=============================================================================
