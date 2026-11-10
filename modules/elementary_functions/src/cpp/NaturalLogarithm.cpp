//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NaturalLogarithm.hpp"
#include "LogarithmKernels.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
NaturalLogarithm(ArrayOf A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
        return logarithmDense<double, LogarithmBase::E>(A, NLS_DOUBLE, NLS_DCOMPLEX, false);
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return logarithmDense<single, LogarithmBase::E>(A, NLS_SINGLE, NLS_SCOMPLEX, false);
    default:
        needToOverload = true;
        break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
