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
#include "crypto_randomBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "NelsonEd25519.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
// bytes = crypto.random(n)          uint8 row vector
// hexa  = crypto.random(n, '-hex')  hexadecimal character vector
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_randomBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    double value = (argIn[0].isNumeric() && argIn[0].isScalar()) ? argIn[0].asDoubleScalar() : -1.0;
    if (value < 1 || value > 1048576 || value != std::floor(value)) {
        Error(L"Nelson:core:randomByteCountExpected",
            L"Number of bytes must be an integer between 1 and 1048576.");
    }
    bool asHex = false;
    if (argIn.size() == 2) {
        if (!cryptoIsTextScalar(argIn[1])) {
            Error(L"Nelson:types:wrongArgumentTypeExpected",
                L"Wrong type for argument #%d: %s expected.", 2, L"'char' or 'string'");
        }
        if (argIn[1].asWideString() != L"-hex") {
            Error(L"Nelson:core:cryptoHexOptionExpected", L"#%d argument: '-hex' expected.", 2);
        }
        asHex = true;
    }
    size_t length = static_cast<size_t>(value);
    std::vector<uint8_t> bytes(length);
    if (!secureRandomBytes(bytes.data(), length)) {
        Error(L"Nelson:core:secureRandomUnavailable",
            L"The operating system secure random source is unavailable.");
    }
    if (asHex) {
        retval << ArrayOf::characterArray(bytesToHexString(bytes.data(), bytes.size()));
    } else {
        retval << ArrayOf::uint8RowVector(bytes);
    }
    return retval;
}
//=============================================================================
