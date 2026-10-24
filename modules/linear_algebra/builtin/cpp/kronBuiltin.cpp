//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "kronBuiltin.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T>
ArrayOf
denseKron(const ArrayOf& A, const ArrayOf& B, NelsonType outClass)
{
    const indexType m = A.dimensionLength(0);
    const indexType n = A.dimensionLength(1);
    const indexType p = B.dimensionLength(0);
    const indexType q = B.dimensionLength(1);
    Dimensions dims(m * p, n * q);
    auto* output = static_cast<T*>(
        ArrayOf::allocateArrayOf(outClass, dims.elementCount(), stringVector(), false));
    const auto* a = static_cast<const T*>(A.dataPointer());
    const auto* b = static_cast<const T*>(B.dataPointer());
    const indexType outRows = m * p;
    for (indexType colA = 0; colA < n; ++colA) {
        for (indexType colB = 0; colB < q; ++colB) {
            const indexType outCol = colA * q + colB;
            for (indexType rowA = 0; rowA < m; ++rowA) {
                const T av = a[colA * m + rowA];
                const indexType outRowBase = rowA * p;
                for (indexType rowB = 0; rowB < p; ++rowB) {
                    output[outCol * outRows + outRowBase + rowB] = av * b[colB * p + rowB];
                }
            }
        }
    }
    return ArrayOf(outClass, dims, output);
}
//=============================================================================
bool
isDenseRealFloatingMatrix(const ArrayOf& value)
{
    return !value.isSparse() && !value.isComplex() && value.is2D()
        && (value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_SINGLE);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::kronBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    const ArrayOf& A = argIn[0];
    const ArrayOf& B = argIn[1];
    if (!isDenseRealFloatingMatrix(A) || !isDenseRealFloatingMatrix(B)) {
        Error(L"Nelson:kron:unsupportedBuiltinType",
            L"Builtin kron supports dense real single or double matrices.");
    }
    ArrayOfVector retval;
    if (A.dataClass() == NLS_SINGLE || B.dataClass() == NLS_SINGLE) {
        ArrayOf ASingle = A;
        ArrayOf BSingle = B;
        ASingle.promoteType(NLS_SINGLE);
        BSingle.promoteType(NLS_SINGLE);
        retval << denseKron<single>(ASingle, BSingle, NLS_SINGLE);
    } else {
        retval << denseKron<double>(A, B, NLS_DOUBLE);
    }
    return retval;
}
//=============================================================================
