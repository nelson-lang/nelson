//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringEndsWith.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
endsWithString(const std::wstring& A, const std::wstring& pattern, bool bCaseSensitive)
{
    bool res = false;
    if (bCaseSensitive) {
        res = StringHelpers::ends_with(A, pattern);
    } else {
        res = StringHelpers::iends_with(A, pattern);
    }
    return res;
}
//=============================================================================
ArrayOf
StringEndsWith(const ArrayOf& A, const ArrayOf& Pattern, bool bCaseSensitive)
{
    ArrayOf res;
    if ((A.isCharacterArray() || (A.isStringArray() && A.isScalar()))
        && (Pattern.isCharacterArray() || (Pattern.isStringArray() && Pattern.isScalar()))) {
        res = ArrayOf::logicalConstructor(endsWithString(
            A.getContentAsWideString(), Pattern.getContentAsWideString(), bCaseSensitive));
    } else {
        if ((A.isCharacterArray()
                || (A.isStringArray() && A.isScalar())
                    && (Pattern.isStringArray() || Pattern.isCellArrayOfCharacterVectors()))) {
            std::wstring strA = A.getContentAsWideString();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            auto* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            bool val = false;
            for (size_t k = 0; k < nbPattern; k++) {
                std::wstring pattern = cellPattern[k].getContentAsWideString();
                if (endsWithString(strA, pattern, bCaseSensitive)) {
                    val = true;
                    break;
                }
            }
            res = ArrayOf::logicalConstructor(val);
        } else if ((A.isStringArray() || A.isCellArrayOfCharacterVectors())
            && (Pattern.isCharacterArray() || (Pattern.isStringArray() && Pattern.isScalar()))) {
            std::wstring pattern = Pattern.getContentAsWideString();
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            logical* result = static_cast<logical*>(
                ArrayOf::allocateArrayOf(NLS_LOGICAL, nbA, stringVector(), true));
            auto* cellA = (ArrayOf*)(A.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                result[k] = static_cast<Nelson::logical>(
                    endsWithString(cellA[k].getContentAsWideString(), pattern, bCaseSensitive));
            }
            res = ArrayOf(NLS_LOGICAL, dimA, result);
        } else if ((A.isStringArray() || A.isCellArrayOfCharacterVectors())
            && (A.isStringArray() || Pattern.isCellArrayOfCharacterVectors())) {
            Dimensions dimA = A.getDimensions();
            size_t nbA = dimA.getElementCount();
            Dimensions dimPattern = Pattern.getDimensions();
            size_t nbPattern = dimPattern.getElementCount();
            logical* result = static_cast<logical*>(
                ArrayOf::allocateArrayOf(NLS_LOGICAL, nbA, stringVector(), true));
            auto* cellA = (ArrayOf*)(A.getDataPointer());
            auto* cellPattern = (ArrayOf*)(Pattern.getDataPointer());
            for (size_t k = 0; k < nbA; k++) {
                for (size_t l = 0; l < nbPattern; l++) {
                    bool val = endsWithString(cellA[k].getContentAsWideString(),
                        cellPattern[l].getContentAsWideString(), bCaseSensitive);
                    if (val) {
                        result[k] = static_cast<Nelson::logical>(val);
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
} // namespace Nelson
//=============================================================================
