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
#include "StringTrim.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* ws = L" \t\n\r\f\v";
//=============================================================================
inline std::wstring&
rtrim(std::wstring& s)
{
    s.erase(s.find_last_not_of(ws) + 1);
    return s;
}
//=============================================================================
inline std::wstring&
ltrim(std::wstring& s)
{
    s.erase(0, s.find_first_not_of(ws));
    return s;
}
//=============================================================================
inline std::wstring&
Trim(std::wstring& s)
{
    return ltrim(rtrim(s));
}
//=============================================================================
ArrayOf
StringTrim(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isRowVectorCharacterArray()) {
        std::wstring str = A.getContentAsWideString();
        return ArrayOf::characterArrayConstructor(Trim(str));
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
            std::wstring str = element[k].getContentAsWideString();
            element[k] = ArrayOf::characterArrayConstructor(Trim(str));
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
                std::wstring str = element[k].getContentAsWideString();
                element[k] = ArrayOf::characterArrayConstructor(Trim(str));
            }
        }
        return res;
    }
    needToOverload = true;

    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
