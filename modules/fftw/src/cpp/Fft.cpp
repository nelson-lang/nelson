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
    if (X.isReferenceType() || X.isHandle()) {
        Error(
            _("Undefined function 'fft' for input arguments of type") + " '" + ClassName(X) + "'.");
    }
    if (X.isScalar() || X.isEmpty()) {
        return ArrayOf(X);
    }
    if (X.getDataClass() == NLS_SCOMPLEX || X.getDataClass() == NLS_SINGLE) {
        return scomplexFFTW(X, n, dim, false);
    }
    return dcomplexFFTW(X, n, dim, false);
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
    indexType n = X.getDimensionLength(static_cast<int>(dim));
    return Fft(X, n, dim);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
