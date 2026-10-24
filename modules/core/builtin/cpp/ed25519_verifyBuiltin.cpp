//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ed25519_verifyBuiltin.hpp"
#include "cryptoArgumentsHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonEd25519.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// tf = crypto.ed25519.verify(message, signature, publicKey)
// tf = crypto.ed25519.verify(filename, signature, publicKey, '-file')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::ed25519_verifyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    bool asFile = (argIn.size() == 4) && cryptoIsFileOption(argIn[3], 4);
    std::vector<uint8_t> message = cryptoMessageBytes(argIn[0], asFile, 1);
    std::vector<uint8_t> signature = cryptoHexOrBytes(argIn[1], 2);
    if (signature.size() != ED25519_SIGNATURE_SIZE) {
        Error(L"Nelson:core:ed25519SignatureSizeExpected",
            L"Signature must be 64 bytes (uint8 vector) or 128 hexadecimal characters.");
    }
    std::vector<uint8_t> publicKey = cryptoHexOrBytes(argIn[2], 3);
    if (publicKey.size() != ED25519_PUBLIC_KEY_SIZE) {
        Error(L"Nelson:core:ed25519PublicKeySizeExpected",
            L"Public key must be 32 bytes (uint8 vector) or 64 hexadecimal characters.");
    }
    bool valid = ed25519Verify(message.data(), message.size(), signature.data(), publicKey.data());
    retval << ArrayOf::logicalScalar(valid);
    return retval;
}
//=============================================================================
