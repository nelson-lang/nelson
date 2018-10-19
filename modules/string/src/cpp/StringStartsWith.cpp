//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "StringStartsWith.hpp"
#include "Error.hpp"
#include "IsCellOfStrings.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
startsWithString(const std::wstring& A, const std::wstring& pattern, bool bCaseSensitive)
{
    bool res = false;
    if (bCaseSensitive) {
        res = boost::algorithm::starts_with(A, pattern);
    } else {
        res = boost::algorithm::istarts_with(A, pattern);
    }
    return res;
}
//=============================================================================
ArrayOf
StringStartsWith(ArrayOf A, ArrayOf Pattern, bool bCaseSensitive)
{
    ArrayOf res;
    if ((A.isCharacterArray() || (A.isStringArray() && A.isScalar()))
        && (Pattern.isCharacterArray() || (Pattern.isStringArray() && Pattern.isScalar()))) {
        res = ArrayOf::logicalConstructor(startsWithString(
            A.getContentAsWideString(), Pattern.getContentAsWideString(), bCaseSensitive));
    } else {
        if ((A.isCharacterArray()
                || (A.isStringArray() && A.isScalar())
                    && (Pattern.isStringArray() || IsCellOfString(Pattern)))) {
            std::wstring strA = A.getContentAsWideString();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            ArrayOf* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            bool val = false;
            for (size_t k = 0; k < nbPattern; k++) {
                std::wstring pattern = cellPattern[k].getContentAsWideString();
                if (startsWithString(strA, pattern, bCaseSensitive)) {
                    val = true;
                    break;
                }
            }
            res = ArrayOf::logicalConstructor(val);
        } else if ((A.isStringArray() || IsCellOfString(A))
            && (Pattern.isCharacterArray() || (Pattern.isStringArray() && Pattern.isScalar()))) {
            std::wstring pattern = Pattern.getContentAsWideString();
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            logical* result = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, nbA);
            ArrayOf* cellA = (ArrayOf*)(A.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                result[k]
                    = startsWithString(cellA[k].getContentAsWideString(), pattern, bCaseSensitive);
            }
            res = ArrayOf(NLS_LOGICAL, dimA, result);
        } else if ((A.isStringArray() || IsCellOfString(A))
            && (A.isStringArray() || IsCellOfString(Pattern))) {
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            logical* result = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, nbA);
            ArrayOf* cellA = (ArrayOf*)(A.getDataPointer());
            ArrayOf* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                for (size_t l = 0; l < nbPattern; l++) {
                    bool val = startsWithString(cellA[k].getContentAsWideString(),
                        cellPattern[l].getContentAsWideString(), bCaseSensitive);
                    if (val) {
                        result[k] = val;
                        break;
                    }
                }
            }
            res = ArrayOf(NLS_LOGICAL, dimA, result);
        } else {
            Error(_W("char vector or cell of strings expected."));
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
