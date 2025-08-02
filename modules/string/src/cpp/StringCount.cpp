//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringCount.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
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
        std::wstring upperStr = StringHelpers::to_upper_copy(str);
        std::wstring upperPattern = StringHelpers::to_upper_copy(pattern);
        for (size_t offset = upperStr.find(upperPattern); offset != std::wstring::npos;
             offset = upperStr.find(upperPattern, offset + upperPattern.length())) {
            ++count;
        }
    }
    return count;
}
//=============================================================================
ArrayOf
StringCount(const ArrayOf& A, const ArrayOf& Pattern, bool bCaseSensitive)
{
    ArrayOf res;
    if ((A.isCharacterArray() && Pattern.isCharacterArray())
        || (A.isStringArray() && A.isScalar() && Pattern.isStringArray() && Pattern.isScalar())) {
        res = ArrayOf::doubleConstructor(countString(
            A.getContentAsWideString(), Pattern.getContentAsWideString(), bCaseSensitive));
    } else {
        if ((A.isCharacterArray() || (A.isStringArray() && A.isScalar()))
            && (Pattern.isStringArray() || Pattern.isCellArrayOfCharacterVectors())) {
            std::wstring strA = A.getContentAsWideString();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            auto* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            int count = 0;
            for (size_t k = 0; k < nbPattern; k++) {
                std::wstring pattern = cellPattern[k].getContentAsWideString();
                count = count + countString(strA, pattern, bCaseSensitive);
            }
            res = ArrayOf::doubleConstructor(count);
        } else if ((A.isStringArray() || A.isCellArrayOfCharacterVectors())
            && (Pattern.isCharacterArray() || (Pattern.isStringArray() && Pattern.isScalar()))) {
            std::wstring pattern = Pattern.getContentAsWideString();
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            double* result = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, nbA, stringVector(), false));
            auto* cellA = (ArrayOf*)(A.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                result[k] = countString(cellA[k].getContentAsWideString(), pattern, bCaseSensitive);
            }
            res = ArrayOf(NLS_DOUBLE, dimA, result);
        } else if ((A.isStringArray() || A.isCellArrayOfCharacterVectors())
            && (Pattern.isStringArray() || Pattern.isCellArrayOfCharacterVectors())) {
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            double* result = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, nbA, stringVector(), true));
            auto* cellA = (ArrayOf*)(A.getDataPointer());
            auto* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
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
} // namespace Nelson
//=============================================================================
