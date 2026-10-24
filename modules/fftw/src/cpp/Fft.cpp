//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Fft.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FftHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Fft(const ArrayOf& X, indexType n, indexType dim)
{
    ArrayOf A = prepareFftwInput(X, "fft");
    if (n == 0 || A.isEmpty()) {
        return emptyFftwResult(A, n, dim);
    }
    if (A.dataClass() == NLS_SCOMPLEX || A.dataClass() == NLS_SINGLE) {
        return scomplexFFTW(A, n, dim, false);
    }
    return dcomplexFFTW(A, n, dim, false);
}
//=============================================================================
ArrayOf
Fft(const ArrayOf& X, indexType n)
{
    indexType dim = computeDim(X);
    return Fft(X, n, dim);
}
//=============================================================================
ArrayOf
Fft(const ArrayOf& X)
{
    indexType dim = computeDim(X);
    indexType n = X.dimensionLength(static_cast<int>(dim));
    return Fft(X, n, dim);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
