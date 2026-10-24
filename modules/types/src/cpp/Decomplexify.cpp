//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
decomplexify(const ArrayOf& A)
{
    NelsonType classA = A.dataClass();
    if ((classA == NLS_DCOMPLEX || classA == NLS_SCOMPLEX) && A.allReal()) {
        NelsonType realClass = (classA == NLS_DCOMPLEX) ? NLS_DOUBLE : NLS_SINGLE;
        if (A.isEmpty() && !A.isSparse()) {
            // nothing to convert: a new empty real array
            return ArrayOf(realClass, A.dimensions(), static_cast<void*>(nullptr), false);
        }
        ArrayOf res = A;
        res.promoteType(realClass);
        return res;
    }
    return A;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
