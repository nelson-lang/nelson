//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <bitset>
#include <stdexcept>
#include "Error.hpp"
#include "i18n.hpp"
#include "BinToNum.hpp"
#include "characters_encoding.hpp"
#include "Transpose.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isValidLength(size_t len)
{
    switch (len) {
    case 1:
    case 32:
    case 64:
        return true;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
static logical
bin2logical(const std::string& s)
{
    unsigned long b;
    try {
        std::bitset<1> bits(s);
        b = bits.to_ulong();
    } catch (std::exception&) {
        Error(_W("Binary character vector may consist only of characters 0 and 1."));
    }
    return *reinterpret_cast<logical*>(&b);
}
//=============================================================================
static single
bin2single(const std::string& s)
{
    unsigned long b;
    try {
        std::bitset<32> bits(s);
        b = bits.to_ulong();
    } catch (std::exception&) {
        Error(_W("Binary character vector may consist only of characters 0 and 1."));
    }
    return *reinterpret_cast<single*>(&b);
}
//=========================================================================
static double
bin2double(const std::string& s)
{
    unsigned long long b;
    try {
        std::bitset<64> bits(s);
        b = bits.to_ullong();
    } catch (std::exception&) {
        Error(_W("Binary character vector may consist only of characters 0 and 1."));
    }
    return *reinterpret_cast<double*>(&b);
}
//=========================================================================
ArrayOf
BinToNum(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    Dimensions dims = A.getDimensions();
    if (dims.isEmpty(false)) {
        res = ArrayOf::emptyConstructor();
    } else {
        stringVector strs;
        indexType commonLength;
        if (A.isRowVector()) {
            std::string s = A.getContentAsCString();
            StringHelpers::trim(s);
            commonLength = s.size();
            if (!isValidLength(commonLength)) {
                Error(_W("Invalid string length: 1, 32, 64 expected."));
            }
            strs.push_back(s);
        } else {
            if (A.isNdArrayCharacterType()) {
                Error(_W("2D char array expected."));
            }
            bool dummy;
            ArrayOf Transposed = Transpose(A, dummy);
            std::wstring wstr = Transposed.getContentAsArrayOfCharacters();
            StringHelpers::trim(wstr);

            commonLength = dims.getColumns();
            if (!isValidLength(commonLength)) {
                Error(_W("Invalid string length: 1, 32, 64 expected."));
            }
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; k = k + commonLength) {
                std::string s = wstring_to_utf8(wstr.substr(k, k + commonLength));
                strs.push_back(s);
            }
        }
        switch (commonLength) {
        case 1: {
            // as logical
            logical* values = (logical*)ArrayOf::allocateArrayOf(
                NLS_LOGICAL, strs.size(), stringVector(), false);
            Dimensions dims(strs.size(), 1);
            res = ArrayOf(NLS_LOGICAL, dims, values);
            for (size_t k = 0; k < strs.size(); ++k) {
                values[k] = bin2logical(strs[k]);
            }
        } break;
        case 32: {
            // as single
            single* values
                = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, strs.size(), stringVector(), false);
            Dimensions dims(strs.size(), 1);
            res = ArrayOf(NLS_SINGLE, dims, values);
            for (size_t k = 0; k < strs.size(); ++k) {
                values[k] = bin2single(strs[k]);
            }
        } break;
        case 64: {
            // as double
            double* values
                = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, strs.size(), stringVector(), false);
            Dimensions dims(strs.size(), 1);
            res = ArrayOf(NLS_DOUBLE, dims, values);
            for (size_t k = 0; k < strs.size(); ++k) {
                values[k] = bin2double(strs[k]);
            }
        } break;
        default: {
        } break;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
