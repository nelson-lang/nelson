//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "InverseFft.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FftHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
InverseFft(const ArrayOf& X, indexType n, indexType dim)
{
    ArrayOf A = prepareFftwInput(X, "ifft");
    if (n == 0 || A.isEmpty()) {
        return emptyFftwResult(A, n, dim);
    }
    if (A.dataClass() == NLS_SCOMPLEX || A.dataClass() == NLS_SINGLE) {
        return scomplexFFTW(A, n, dim, true);
    }
    return dcomplexFFTW(A, n, dim, true);
}
//=============================================================================
ArrayOf
InverseFft(const ArrayOf& X, indexType n)
{
    indexType dim = computeDim(X);
    return InverseFft(X, n, dim);
}
//=============================================================================
ArrayOf
InverseFft(const ArrayOf& X)
{
    indexType dim = computeDim(X);
    indexType n = X.dimensionLength(static_cast<int>(dim));
    return InverseFft(X, n, dim);
}
//=============================================================================
// 'symmetric' option: the input spectrum is treated as conjugate symmetric
// along the working dimension. Only the first floor(n/2) + 1 bins are used,
// the upper half is rebuilt as the conjugate mirror of the lower half, and the
// imaginary parts of the DC (and Nyquist, for even n) bins are ignored. The
// result is real.
//=============================================================================
template <class T>
static void
symmetrizeSpectrum(T* ob, indexType planecount, indexType planesize, indexType n)
{
    indexType half = n / 2;
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            indexType lineOffset = i * planesize * n + j;
            // DC bin must be real
            ob[2 * (lineOffset) + 1] = (T)0;
            if ((n % 2 == 0) && (n > 1)) {
                // Nyquist bin must be real
                ob[2 * (lineOffset + half * planesize) + 1] = (T)0;
            }
            for (indexType k = half + 1; k < n; k++) {
                indexType idxDst = 2 * (lineOffset + k * planesize);
                indexType idxSrc = 2 * (lineOffset + (n - k) * planesize);
                ob[idxDst] = ob[idxSrc];
                ob[idxDst + 1] = -ob[idxSrc + 1];
            }
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
symmetrizePadded(
    const ArrayOf& A, NelsonType complexClass, indexType n, indexType dim, bool asComplexInput)
{
    Dimensions inDim = A.dimensions();
    Dimensions outDim(inDim);
    outDim[dim] = n;
    indexType planecount = 1;
    indexType planesize = 1;
    indexType linesize = inDim[dim];
    for (indexType d = dim + 1; d < inDim.length(); d++) {
        planecount *= inDim[d];
    }
    for (indexType d = 0; d < dim; d++) {
        planesize *= inDim[d];
    }
    T* ob = (T*)ArrayOf::allocateArrayOf(complexClass, outDim.elementCount(), stringVector(), true);
    outDim.simplify();
    ArrayOf res = ArrayOf(complexClass, outDim, ob);
    indexType copyIn = n;
    if (linesize < copyIn) {
        copyIn = linesize;
    }
    auto* datapointer = (T*)A.dataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            for (indexType k = 0; k < copyIn; k++) {
                indexType cplxOffset = asComplexInput ? 2 : 1;
                indexType idxSrc = cplxOffset * (i * planesize * linesize + j + k * planesize);
                indexType idxDst = 2 * (i * planesize * n + j + k * planesize);
                ob[idxDst] = datapointer[idxSrc];
                ob[idxDst + 1] = asComplexInput ? datapointer[idxSrc + 1] : (T)0;
            }
        }
    }
    symmetrizeSpectrum<T>(ob, planecount, planesize, n);
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
takeRealPart(const ArrayOf& C, NelsonType realClass)
{
    if (C.dataClass() == realClass) {
        return C;
    }
    Dimensions dims = C.dimensions();
    indexType nbElements = dims.elementCount();
    T* ptr = (T*)ArrayOf::allocateArrayOf(realClass, nbElements, stringVector(), false);
    auto* src = (const T*)C.dataPointer();
    for (indexType k = 0; k < nbElements; k++) {
        ptr[k] = src[2 * k];
    }
    return ArrayOf(realClass, dims, ptr);
}
//=============================================================================
ArrayOf
InverseFftSymmetric(const ArrayOf& X, indexType n, indexType dim)
{
    ArrayOf A = prepareFftwInput(X, "ifft");
    if (n == 0 || A.isEmpty()) {
        return emptyFftwResult(A, n, dim);
    }
    bool asSingle = (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX);
    ArrayOf symmetrized;
    ArrayOf res;
    if (asSingle) {
        symmetrized
            = symmetrizePadded<single>(A, NLS_SCOMPLEX, n, dim, A.dataClass() == NLS_SCOMPLEX);
        res = scomplexFFTW(symmetrized, n, dim, true);
        return takeRealPart<single>(res, NLS_SINGLE);
    }
    symmetrized = symmetrizePadded<double>(A, NLS_DCOMPLEX, n, dim, A.dataClass() == NLS_DCOMPLEX);
    res = dcomplexFFTW(symmetrized, n, dim, true);
    return takeRealPart<double>(res, NLS_DOUBLE);
}
//=============================================================================
ArrayOf
InverseFftSymmetric(const ArrayOf& X, indexType n)
{
    indexType dim = computeDim(X);
    return InverseFftSymmetric(X, n, dim);
}
//=============================================================================
ArrayOf
InverseFftSymmetric(const ArrayOf& X)
{
    indexType dim = computeDim(X);
    indexType n = X.dimensionLength(static_cast<int>(dim));
    return InverseFftSymmetric(X, n, dim);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
