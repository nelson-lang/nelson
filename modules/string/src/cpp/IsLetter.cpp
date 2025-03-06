//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include <cwctype>
#include "IsLetter.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline ArrayOf
isWideCharAlpha(const ArrayOf& A)
{
    std::wstring str = A.getContentAsWideString();
    logical* ptrRes = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, str.length());
    ArrayOf result = ArrayOf(NLS_LOGICAL, Dimensions(1, str.length()), ptrRes);
    ompIndexType nbElements = (ompIndexType)str.length();
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        ptrRes[k] = (logical)isUnicodeLetter(str[k]);
    }
    return result;
}
//=============================================================================
ArrayOf
IsLetter(const ArrayOf& A)
{
    ArrayOf result;
    if (A.isCharacterArray()) {
        result = isWideCharAlpha(A);
    } else if (A.isStringArray()) {
        if (A.isScalar()) {
            result = isWideCharAlpha(A);
        } else {
            Error(_W("Input must be a text scalar."));
        }
    } else {
        logical* ptrRes = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.getElementCount(), stringVector(), true);
        result = ArrayOf(NLS_LOGICAL, A.getDimensions(), ptrRes);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
