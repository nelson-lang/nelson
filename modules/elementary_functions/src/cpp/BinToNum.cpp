//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <bitset>
#include <stdexcept>
#include <Eigen/Dense>
#include "Error.hpp"
#include "Exception.hpp"
#include "BinToNum.hpp"
#include "characters_encoding.hpp"
#include "Transpose.hpp"
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
    default: { } break; }
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
BinToNum(ArrayOf A, bool& needToOverload)
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
            boost::algorithm::trim(s);
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
            boost::algorithm::trim(wstr);

            commonLength = dims.getColumns();
            if (!isValidLength(commonLength)) {
                Error(_W("Invalid string length: 1, 32, 64 expected."));
            }
            for (size_t k = 0; k < dims.getElementCount(); k = k + commonLength) {
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
        default: { } break; }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
