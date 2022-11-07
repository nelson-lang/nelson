//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Cast.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Cast(const ArrayOf& arrayIn, NelsonType destinationClass, bool isSparse)
{
    ArrayOf res;
    res = arrayIn;
    NelsonType originClass = res.getDataClass();
    if (originClass == NLS_SCOMPLEX || originClass == NLS_DCOMPLEX) {
        if (destinationClass == NLS_DOUBLE) {
            destinationClass = NLS_DCOMPLEX;
        }
        if (destinationClass == NLS_SINGLE) {
            destinationClass = NLS_SCOMPLEX;
        }
    }
    if ((originClass == NLS_SCOMPLEX || originClass == NLS_DCOMPLEX)
        && (destinationClass != NLS_SCOMPLEX && destinationClass != NLS_DCOMPLEX)) {
        Error(_W("Invalid conversion from complex."));
    }
    res.promoteType(destinationClass);
    if (isSparse) {
        res.makeSparse();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
