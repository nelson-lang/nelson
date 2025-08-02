//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Blanks.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Blanks(indexType nbBlanks)
{
    ArrayOf res;
    Dimensions dimsRes(1, nbBlanks);
    charType* ptrChar
        = (charType*)ArrayOf::allocateArrayOf(NLS_CHAR, nbBlanks, stringVector(), false);
    res = ArrayOf(NLS_CHAR, dimsRes, ptrChar);
    wmemset(ptrChar, L' ', nbBlanks);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
