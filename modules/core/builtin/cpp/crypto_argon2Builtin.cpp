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
#include <cmath>
#include <cwctype>
#include "crypto_argon2Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
static uint32_t
argon2IntegerOption(const ArrayOf& value, const std::wstring& name, double minimum, double maximum);
//=============================================================================
// hexa_hash = crypto.argon2(password, salt)
// hexa_hash = crypto.argon2(password, salt, 'Variant', 'argon2id', 'Memory', 65536,
//                           'Passes', 3, 'Lanes', 1, 'Length', 32,
//                           'Key', key, 'AssociatedData', ad)
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_argon2Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 16);
    nargoutcheck(nLhs, 0, 1);
    if ((argIn.size() - 2) % 2 != 0) {
        Error(
            L"Nelson:core:argon2OptionValueExpected", L"Argon2 options must be name/value pairs.");
    }
    std::vector<uint8_t> password = cryptoMessageBytes(argIn[0], false, 1);
    std::vector<uint8_t> salt = cryptoMessageBytes(argIn[1], false, 2);
    std::vector<uint8_t> key;
    std::vector<uint8_t> associatedData;
    Argon2Parameters parameters;
    for (size_t k = 2; k + 1 < argIn.size(); k += 2) {
        int position = static_cast<int>(k + 1);
        if (!cryptoIsTextScalar(argIn[k])) {
            Error(L"Nelson:types:wrongArgumentTypeExpected",
                L"Wrong type for argument #%d: %s expected.", position, L"'char' or 'string'");
        }
        std::wstring name = argIn[k].asWideString();
        std::wstring lowered(name);
        std::transform(lowered.begin(), lowered.end(), lowered.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
        const ArrayOf& value = argIn[k + 1];
        if (lowered == L"variant") {
            if (!cryptoIsTextScalar(value)) {
                Error(L"Nelson:types:wrongArgumentTypeExpected",
                    L"Wrong type for argument #%d: %s expected.", position + 1,
                    L"'char' or 'string'");
            }
            std::wstring variantName = value.asWideString();
            if (!argon2VariantFromName(variantName, parameters.variant)) {
                Error(L"Nelson:core:argon2VariantExpected",
                    L"Unsupported Argon2 variant: %s ('argon2id', 'argon2i' or 'argon2d' "
                    L"expected).",
                    variantName);
            }
        } else if (lowered == L"memory") {
            parameters.memoryKiB = argon2IntegerOption(value, L"Memory", 8, 4194304);
        } else if (lowered == L"passes") {
            parameters.passes = argon2IntegerOption(value, L"Passes", 1, 4294967295.0);
        } else if (lowered == L"lanes") {
            parameters.lanes = argon2IntegerOption(value, L"Lanes", 1, 16777215);
        } else if (lowered == L"length") {
            parameters.length = argon2IntegerOption(value, L"Length", 4, 1024);
        } else if (lowered == L"key") {
            key = cryptoMessageBytes(value, false, position + 1);
        } else if (lowered == L"associateddata") {
            associatedData = cryptoMessageBytes(value, false, position + 1);
        } else {
            Error(L"Nelson:core:argon2OptionExpected", L"Unknown Argon2 option: %s", name);
        }
    }
    if (salt.size() < 8) {
        Error(L"Nelson:core:argon2SaltSizeExpected", L"Argon2 salt must be at least 8 bytes.");
    }
    if (parameters.memoryKiB < 8 * parameters.lanes) {
        Error(L"Nelson:core:argon2MemoryExpected",
            L"Argon2 memory (KiB) must be at least 8 times the number of lanes.");
    }
    std::wstring hash = computeArgon2Hex(parameters, password.data(), password.size(), salt.data(),
        salt.size(), key.data(), key.size(), associatedData.data(), associatedData.size());
    if (hash.empty()) {
        Error(L"Nelson:core:argon2MemoryAllocationFailed",
            L"Cannot allocate the Argon2 work area (%d KiB).",
            static_cast<int>(parameters.memoryKiB));
    }
    retval << ArrayOf::characterArray(hash);
    return retval;
}
//=============================================================================
static uint32_t
argon2IntegerOption(const ArrayOf& value, const std::wstring& name, double minimum, double maximum)
{
    double number = -1.0;
    if (value.isNumeric() && value.isScalar()) {
        number = value.asDoubleScalar();
    }
    if (number < minimum || number > maximum || number != std::floor(number)) {
        Error(L"Nelson:core:argon2ParameterInvalid",
            L"Argon2 %s must be an integer between %d and %d.", name, static_cast<int>(minimum),
            static_cast<int>(std::min(maximum, 2147483647.0)));
    }
    return static_cast<uint32_t>(number);
}
//=============================================================================
