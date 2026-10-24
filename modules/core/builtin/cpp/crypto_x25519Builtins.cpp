//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "crypto_x25519Builtins.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "NelsonEd25519.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
// publicKey = crypto.x25519.public(secretKey)
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_x25519_publicBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::vector<uint8_t> secretKey = cryptoFixedBytes(
        argIn[0], CRYPTO_X25519_KEY_SIZE, L"Nelson:core:x25519KeySizeExpected", L"Secret key", 1);
    uint8_t publicKey[CRYPTO_X25519_KEY_SIZE];
    x25519PublicKey(secretKey.data(), publicKey);
    retval << ArrayOf::characterArray(bytesToHexString(publicKey, CRYPTO_X25519_KEY_SIZE));
    return retval;
}
//=============================================================================
// sharedSecret = crypto.x25519.shared(secretKey, peerPublicKey)
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_x25519_sharedBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<uint8_t> secretKey = cryptoFixedBytes(
        argIn[0], CRYPTO_X25519_KEY_SIZE, L"Nelson:core:x25519KeySizeExpected", L"Secret key", 1);
    std::vector<uint8_t> peerPublicKey = cryptoFixedBytes(
        argIn[1], CRYPTO_X25519_KEY_SIZE, L"Nelson:core:x25519KeySizeExpected", L"Public key", 2);
    uint8_t sharedSecret[CRYPTO_X25519_KEY_SIZE];
    x25519Shared(secretKey.data(), peerPublicKey.data(), sharedSecret);
    retval << ArrayOf::characterArray(bytesToHexString(sharedSecret, CRYPTO_X25519_KEY_SIZE));
    return retval;
}
//=============================================================================
// [publicKey, secretKey] = crypto.x25519.keypair()
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_x25519_keypairBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 2);
    uint8_t secretKey[CRYPTO_X25519_KEY_SIZE];
    uint8_t publicKey[CRYPTO_X25519_KEY_SIZE];
    if (!secureRandomBytes(secretKey, CRYPTO_X25519_KEY_SIZE)) {
        Error(L"Nelson:core:secureRandomUnavailable",
            L"The operating system secure random source is unavailable.");
    }
    x25519PublicKey(secretKey, publicKey);
    retval << ArrayOf::characterArray(bytesToHexString(publicKey, CRYPTO_X25519_KEY_SIZE));
    if (nLhs > 1) {
        retval << ArrayOf::characterArray(bytesToHexString(secretKey, CRYPTO_X25519_KEY_SIZE));
    }
    return retval;
}
//=============================================================================
