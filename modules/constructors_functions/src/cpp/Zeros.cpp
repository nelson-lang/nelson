//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Zeros.hpp"
#include "NelsonTypeTraits.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Zeros(NelsonType cl)
{
    Dimensions dims(1, 1);
    return Zeros(dims, cl);
}
//=============================================================================
ArrayOf
Zeros(Dimensions& dims, NelsonType cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(cl);
        return res;
    }
    if (!TypeTraits::isNumeric(cl) && cl != NLS_LOGICAL) {
        Error(L"Nelson:constructors_functions:typeNotSupported", L"Type not supported.");
    }
    // allocateArrayOf with initializeValues zero-fills; complex classes store two
    // real components per element.
    indexType nbElements = dims.elementCount();
    void* mat = nullptr;
    if (nbElements != 0) {
        indexType storageCount = TypeTraits::isComplex(cl) ? nbElements * 2 : nbElements;
        mat = ArrayOf::allocateArrayOf(cl, storageCount, stringVector(), true);
    }
    return ArrayOf(cl, dims, mat, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
