//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsCellOfStrings.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsCellOfString(const ArrayOf& cellarr)
{
    if (cellarr.getDataClass() != NLS_CELL_ARRAY) {
        return false;
    }
    if (cellarr.isEmpty()) {
        return true;
    }
    ArrayOf* arg = (ArrayOf*)(cellarr.getDataPointer());
    indexType elementCount = cellarr.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        if (!arg[k].isCharacterArray()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
}
//=============================================================================
