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
#include "StringCount.hpp"
#include "Error.hpp"
#include "IsCellOfStrings.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static int
countString(const std::wstring& str, const std::wstring& pattern, bool caseSensitive)
{
    if (pattern.length() == 0) {
        return 0;
    }
    int count = 0;
    if (caseSensitive) {
        for (size_t offset = str.find(pattern); offset != std::wstring::npos;
             offset = str.find(pattern, offset + pattern.length())) {
            ++count;
        }
    } else {
        std::wstring upperStr = boost::to_upper_copy(str);
        std::wstring upperPattern = boost::to_upper_copy(pattern);
        for (size_t offset = upperStr.find(upperPattern); offset != std::wstring::npos;
             offset = upperStr.find(upperPattern, offset + upperPattern.length())) {
            ++count;
        }
    }
    return count;
}
//=============================================================================
ArrayOf
StringCount(ArrayOf A, ArrayOf Pattern, bool bCaseSensitive)
{
    ArrayOf res;
    if ((A.isCharacterArray() && Pattern.isCharacterArray())
        || (A.isStringArray() && A.isScalar() && Pattern.isStringArray() && Pattern.isScalar())) {
        res = ArrayOf::doubleConstructor(countString(
            A.getContentAsWideString(), Pattern.getContentAsWideString(), bCaseSensitive));
    } else {
        if ((A.isCharacterArray() || (A.isStringArray() && A.isScalar()))
            && (Pattern.isStringArray() || IsCellOfString(Pattern))) {
            std::wstring strA = A.getContentAsWideString();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            ArrayOf* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            int count = 0;
            for (size_t k = 0; k < nbPattern; k++) {
                std::wstring pattern = cellPattern[k].getContentAsWideString();
                count = count + countString(strA, pattern, bCaseSensitive);
            }
            res = ArrayOf::doubleConstructor(count);
        } else if ((A.isStringArray() || IsCellOfString(A))
            && (Pattern.isCharacterArray() || (Pattern.isStringArray() && Pattern.isScalar()))) {
            std::wstring pattern = Pattern.getContentAsWideString();
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            double* result = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nbA);
            ArrayOf* cellA = (ArrayOf*)(A.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                result[k] = countString(cellA[k].getContentAsWideString(), pattern, bCaseSensitive);
            }
            res = ArrayOf(NLS_DOUBLE, dimA, result);
        } else if ((A.isStringArray() || IsCellOfString(A))
            && (Pattern.isStringArray() || IsCellOfString(Pattern))) {
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            double* result = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nbA);
            ArrayOf* cellA = (ArrayOf*)(A.getDataPointer());
            ArrayOf* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                for (size_t l = 0; l < nbPattern; l++) {
                    result[k] = result[k]
                        + countString(cellA[k].getContentAsWideString(),
                              cellPattern[l].getContentAsWideString(), bCaseSensitive);
                }
            }
            res = ArrayOf(NLS_DOUBLE, dimA, result);
        } else {
            Error(_W("char vector or cell of strings expected."));
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
