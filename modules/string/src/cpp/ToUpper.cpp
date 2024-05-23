//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToUpper.hpp"
#include "Error.hpp"
#include <algorithm>
#include "StringHelpers.hpp"
#include <cctype>
#include <string>
#include "nlsBuildConfig.h"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToUpper(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isRowVectorCharacterArray()) {
        return ArrayOf::characterArrayConstructor(ToUpper(A.getContentAsWideString()));
    }
    if (A.getDataClass() == NLS_CELL_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.getDataPointer());
        indexType elementCount = A.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!element[k].isRowVectorCharacterArray()) {
                Error(ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
            }
            element[k]
                = ArrayOf::characterArrayConstructor(ToUpper(element[k].getContentAsWideString()));
        }
        return res;
    }
    if (A.getDataClass() == NLS_STRING_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.getDataPointer());
        ompIndexType elementCount = A.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (!element[k].isRowVectorCharacterArray()) {
                element[k] = ArrayOf::emptyConstructor();
            } else {
                element[k] = ArrayOf::characterArrayConstructor(
                    ToUpper(element[k].getContentAsWideString()));
            }
        }
        return res;
    }
    needToOverload = true;

    return res;
}
//=============================================================================
std::wstring
ToUpper(const std::wstring& A)
{
    return StringHelpers::to_upper_copy(A);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
