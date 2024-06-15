//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <string>
#include "StringDeblank.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring&
rtrim(std::wstring& s)
{
    const wchar_t* ws = L" \t\n\r\f\v";
    s.erase(s.find_last_not_of(ws) + 1);
    return s;
}
//=============================================================================
static std::wstring&
Deblank(std::wstring& s)
{
    return rtrim(s);
}
//=============================================================================
ArrayOf
StringDeblank(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isEmpty()) {
        res = ArrayOf::emptyConstructor();
        res.promoteType(A.getDataClass());
        return res;
    }
    if (A.isRowVectorCharacterArray()) {
        std::wstring str = A.getContentAsWideString();
        res = ArrayOf::characterArrayConstructor(Deblank(str));
    } else if (A.getDataClass() == NLS_CELL_ARRAY) {
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.getDataPointer());
        indexType elementCount = A.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!element[k].isRowVectorCharacterArray()) {
                Error(ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
            }
            std::wstring str = element[k].getContentAsWideString();
            element[k] = ArrayOf::characterArrayConstructor(Deblank(str));
        }
    } else if (A.getDataClass() == NLS_STRING_ARRAY) {
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.getDataPointer());
        ompIndexType elementCount = A.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (element[k].isRowVectorCharacterArray()) {
                std::wstring str = element[k].getContentAsWideString();
                element[k] = ArrayOf::characterArrayConstructor(Deblank(str));
            }
        }
    } else {
        needToOverload = true;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
