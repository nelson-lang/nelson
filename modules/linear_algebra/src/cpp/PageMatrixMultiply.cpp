//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#undef EIGEN_USE_MKL_VML
#undef EIGEN_USE_MKL
#include <Eigen/Dense>
#include "PageMatrixMultiply.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class EigenMat, class Scalar>
static void
pagemul_loop(Scalar* pA, Eigen::Index rA, Eigen::Index cA, indexType pagesA, Scalar* pB,
    Eigen::Index rB, Eigen::Index cB, indexType pagesB, int transpA, int transpB, Scalar* pC,
    Eigen::Index rC, Eigen::Index cC, indexType nPages)
{
    indexType strideA = (indexType)(rA * cA);
    indexType strideB = (indexType)(rB * cB);
    indexType strideC = (indexType)(rC * cC);
    OMP_PARALLEL_FOR_LOOP(nPages)
    for (ompIndexType k = 0; k < (ompIndexType)nPages; ++k) {
        indexType ka = (pagesA == 1) ? 0 : (indexType)k;
        indexType kb = (pagesB == 1) ? 0 : (indexType)k;
        Eigen::Map<EigenMat> a(pA + ka * strideA, rA, cA);
        Eigen::Map<EigenMat> b(pB + kb * strideB, rB, cB);
        Eigen::Map<EigenMat> c(pC + (indexType)k * strideC, rC, cC);
        EigenMat opA = (transpA == 0) ? EigenMat(a)
            : (transpA == 1)          ? EigenMat(a.transpose())
                                      : EigenMat(a.adjoint());
        EigenMat opB = (transpB == 0) ? EigenMat(b)
            : (transpB == 1)          ? EigenMat(b.transpose())
                                      : EigenMat(b.adjoint());
        c.noalias() = opA * opB;
    }
}
//=============================================================================
ArrayOf
PageMatrixMultiply(
    const ArrayOf& A, int transpA, const ArrayOf& B, int transpB, bool& needToOverload)
{
    needToOverload = false;
    auto supported = [](const ArrayOf& V) {
        NelsonType c = V.dataClass();
        return (c == NLS_DOUBLE || c == NLS_SINGLE || c == NLS_DCOMPLEX || c == NLS_SCOMPLEX)
            && !V.isSparse();
    };
    if (!supported(A) || !supported(B)) {
        needToOverload = true;
        return {};
    }
    bool anyComplex = (A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        || (B.dataClass() == NLS_DCOMPLEX || B.dataClass() == NLS_SCOMPLEX);
    bool anySingle = (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX)
        || (B.dataClass() == NLS_SINGLE || B.dataClass() == NLS_SCOMPLEX);
    NelsonType outType;
    if (anyComplex) {
        outType = anySingle ? NLS_SCOMPLEX : NLS_DCOMPLEX;
    } else {
        outType = anySingle ? NLS_SINGLE : NLS_DOUBLE;
    }

    ArrayOf AA(A);
    ArrayOf BB(B);
    AA.promoteType(outType);
    BB.promoteType(outType);

    Dimensions dimsA = AA.dimensions();
    Dimensions dimsB = BB.dimensions();
    Eigen::Index rA = (Eigen::Index)dimsA.at(0, false);
    Eigen::Index cA = (Eigen::Index)dimsA.at(1, false);
    Eigen::Index rB = (Eigen::Index)dimsB.at(0, false);
    Eigen::Index cB = (Eigen::Index)dimsB.at(1, false);

    indexType pagesA = 1;
    for (indexType i = 2; i < (indexType)dimsA.length(); ++i) {
        pagesA *= dimsA.at(i, false);
    }
    indexType pagesB = 1;
    for (indexType i = 2; i < (indexType)dimsB.length(); ++i) {
        pagesB *= dimsB.at(i, false);
    }
    if (!(pagesA == pagesB || pagesA == 1 || pagesB == 1)) {
        Error("Nelson:dimagree:pageDimensionsInputsMatch1",
            "The page dimensions of the inputs must match or be 1.");
    }

    Eigen::Index opAr = (transpA == 0) ? rA : cA;
    Eigen::Index opAc = (transpA == 0) ? cA : rA;
    Eigen::Index opBr = (transpB == 0) ? rB : cB;
    Eigen::Index opBc = (transpB == 0) ? cB : rB;
    if (opAc != opBr) {
        Error("Nelson:dimagree:incorrectDimensionsMatrixMultiplicationPages",
            "Incorrect dimensions for matrix multiplication of the pages.");
    }
    Eigen::Index rC = opAr;
    Eigen::Index cC = opBc;
    indexType nPages = (pagesA >= pagesB) ? pagesA : pagesB;

    // output dimensions: [rC, cC, <trailing pages of the operand with more pages>]
    Dimensions dimsTrailingSrc = (pagesA >= pagesB) ? dimsA : dimsB;
    Dimensions retDims;
    retDims[0] = (indexType)rC;
    retDims[1] = (indexType)cC;
    for (indexType i = 2; i < (indexType)dimsTrailingSrc.length(); ++i) {
        retDims[i] = dimsTrailingSrc.at(i, false);
    }
    retDims.simplify();

    void* ptrC = ArrayOf::allocateArrayOf(outType, retDims.elementCount());
    ArrayOf res(outType, retDims, ptrC);

    switch (outType) {
    case NLS_DOUBLE: {
        pagemul_loop<Eigen::MatrixXd, double>((double*)AA.dataPointer(), rA, cA, pagesA,
            (double*)BB.dataPointer(), rB, cB, pagesB, transpA, transpB, (double*)ptrC, rC, cC,
            nPages);
    } break;
    case NLS_SINGLE: {
        pagemul_loop<Eigen::MatrixXf, single>((single*)AA.dataPointer(), rA, cA, pagesA,
            (single*)BB.dataPointer(), rB, cB, pagesB, transpA, transpB, (single*)ptrC, rC, cC,
            nPages);
    } break;
    case NLS_DCOMPLEX: {
        pagemul_loop<Eigen::MatrixXcd, std::complex<double>>(
            reinterpret_cast<std::complex<double>*>((double*)AA.dataPointer()), rA, cA, pagesA,
            reinterpret_cast<std::complex<double>*>((double*)BB.dataPointer()), rB, cB, pagesB,
            transpA, transpB, reinterpret_cast<std::complex<double>*>((double*)ptrC), rC, cC,
            nPages);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        pagemul_loop<Eigen::MatrixXcf, std::complex<single>>(
            reinterpret_cast<std::complex<single>*>((single*)AA.dataPointer()), rA, cA, pagesA,
            reinterpret_cast<std::complex<single>*>((single*)BB.dataPointer()), rB, cB, pagesB,
            transpA, transpB, reinterpret_cast<std::complex<single>*>((single*)ptrC), rC, cC,
            nPages);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
