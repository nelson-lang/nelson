//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ed25519_signBuiltin.hpp"
#include "cryptoArgumentsHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonEd25519.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// [signature_hex, publicKey_hex] = crypto.ed25519.sign(message, seed)
// [signature_hex, publicKey_hex] = crypto.ed25519.sign(filename, seed, '-file')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::ed25519_signBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    bool asFile = (argIn.size() == 3) && cryptoIsFileOption(argIn[2], 3);
    std::vector<uint8_t> message = cryptoMessageBytes(argIn[0], asFile, 1);
    std::vector<uint8_t> seed = cryptoHexOrBytes(argIn[1], 2);
    if (seed.size() != ED25519_SEED_SIZE) {
        Error(L"Nelson:core:ed25519SeedSizeExpected",
            L"Private seed must be 32 bytes (uint8 vector) or 64 hexadecimal characters.");
    }
    uint8_t signature[ED25519_SIGNATURE_SIZE];
    uint8_t publicKey[ED25519_PUBLIC_KEY_SIZE];
    ed25519Sign(message.data(), message.size(), seed.data(), signature, publicKey);
    retval << ArrayOf::characterArray(bytesToHexString(signature, ED25519_SIGNATURE_SIZE));
    if (nLhs > 1) {
        retval << ArrayOf::characterArray(bytesToHexString(publicKey, ED25519_PUBLIC_KEY_SIZE));
    }
    return retval;
}
//=============================================================================
