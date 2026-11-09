//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdint>
#include <cstring>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// strcmp/strcmpi of two non-empty char rows, every character compared; false when this case
// does not apply (empty or other values, non-ASCII text without case)
inline bool
tryCompareCharRows(const ArrayOf& a, const ArrayOf& b, bool caseSensitive, bool& equal)
{
    if (!a.isRowVectorCharacterArray() || !b.isRowVectorCharacterArray() || a.isEmpty()
        || b.isEmpty()) {
        return false;
    }
    const indexType count = a.elementCount();
    if (count != b.elementCount()) {
        equal = false;
        return true;
    }
    const auto* textA = static_cast<const charType*>(a.dataPointer());
    const auto* textB = static_cast<const charType*>(b.dataPointer());
    if (caseSensitive) {
        equal = std::memcmp(textA, textB, static_cast<size_t>(count) * sizeof(charType)) == 0;
        return true;
    }
    auto lower = [](uint32_t c) { return (c >= 'A' && c <= 'Z') ? c + ('a' - 'A') : c; };
    equal = true;
    for (indexType k = 0; k < count; ++k) {
        const auto charA = static_cast<uint32_t>(textA[k]);
        const auto charB = static_cast<uint32_t>(textB[k]);
        if (charA > 127 || charB > 127) {
            return false;
        }
        if (equal && lower(charA) != lower(charB)) {
            equal = false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
