//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToLower.hpp"
#include "Error.hpp"
#include <algorithm>
#include "StringHelpers.hpp"
#include <cctype>
#include <string>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ToLower(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isRowVectorCharacterArray()) {
        return ArrayOf::characterArrayConstructor(ToLower(A.getContentAsWideString()));
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
                = ArrayOf::characterArrayConstructor(ToLower(element[k].getContentAsWideString()));
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
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (element[k].isRowVectorCharacterArray()) {
                element[k] = ArrayOf::characterArrayConstructor(
                    ToLower(element[k].getContentAsWideString()));
            } else {
                element[k] = ArrayOf::emptyConstructor();
            }
        }
        return res;
    }
    needToOverload = true;

    return res;
}
//=============================================================================
std::wstring
ToLower(const std::wstring& A)
{
    return StringHelpers::to_lower_copy(A);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
