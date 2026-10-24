//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "crypto_blake2bBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
// hexa_hash = crypto.blake2b(message)
// hexa_hash = crypto.blake2b(message, key)
// hexa_hash = crypto.blake2b(message, key, digestSize)
// hexa_hash = crypto.blake2b(filename, ..., '-file')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_blake2bBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    size_t nbArgs = argIn.size();
    bool asFile = false;
    if (nbArgs >= 2 && cryptoIsTextScalar(argIn[nbArgs - 1])
        && argIn[nbArgs - 1].asWideString() == L"-file") {
        asFile = true;
        nbArgs--;
    }
    if (nbArgs > 3) {
        Error(L"Nelson:core:cryptoFileOptionExpected", L"#%d argument: '-file' expected.", 4);
    }
    std::vector<uint8_t> message = cryptoMessageBytes(argIn[0], asFile, 1);
    std::vector<uint8_t> key;
    if (nbArgs >= 2 && !argIn[1].isEmpty()) {
        key = cryptoMessageBytes(argIn[1], false, 2);
    }
    if (key.size() > 64) {
        Error(L"Nelson:core:blake2bKeySizeExpected", L"BLAKE2b key must be at most 64 bytes.");
    }
    size_t digestSize = 64;
    if (nbArgs == 3) {
        double value
            = argIn[2].isNumeric() && argIn[2].isScalar() ? argIn[2].asDoubleScalar() : -1.0;
        if (value < 1 || value > 64 || value != std::floor(value)) {
            Error(L"Nelson:core:blake2bDigestSizeExpected",
                L"BLAKE2b digest size must be an integer between 1 and 64.");
        }
        digestSize = static_cast<size_t>(value);
    }
    retval << ArrayOf::characterArray(
        computeBlake2bHex(message.data(), message.size(), key.data(), key.size(), digestSize));
    return retval;
}
//=============================================================================
