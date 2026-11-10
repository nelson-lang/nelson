//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <complex>
#include "Logarithm10.hpp"
#include "LogarithmKernels.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Logarithm10(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
        return logarithmDense<double, LogarithmBase::Ten>(A, NLS_DOUBLE, NLS_DCOMPLEX, true);
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return logarithmDense<single, LogarithmBase::Ten>(A, NLS_SINGLE, NLS_SCOMPLEX, true);
    default:
        needToOverload = true;
        break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
