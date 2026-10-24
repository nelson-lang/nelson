//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "crypto_hmacBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
// hexa_mac = crypto.hmac(algorithm, key, message)
// hexa_mac = crypto.hmac(algorithm, key, filename, '-file')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_hmacBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    if (!cryptoIsTextScalar(argIn[0])) {
        Error(L"Nelson:types:wrongArgumentTypeExpected",
            L"Wrong type for argument #%d: %s expected.", 1, L"'char' or 'string'");
    }
    std::wstring algorithmName = argIn[0].asWideString();
    HmacAlgorithm algorithm;
    if (!hmacAlgorithmFromName(algorithmName, algorithm)) {
        Error(L"Nelson:core:hmacAlgorithmExpected",
            L"Unsupported HMAC algorithm: %s ('sha256' or 'sha512' expected).", algorithmName);
    }
    std::vector<uint8_t> key = cryptoMessageBytes(argIn[1], false, 2);
    bool asFile = (argIn.size() == 4) && cryptoIsFileOption(argIn[3], 4);
    std::vector<uint8_t> message = cryptoMessageBytes(argIn[2], asFile, 3);
    retval << ArrayOf::characterArray(
        computeHmacHex(algorithm, key.data(), key.size(), message.data(), message.size()));
    return retval;
}
//=============================================================================
