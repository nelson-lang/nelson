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
#define _USE_MATH_DEFINES
#endif
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl.h>
#endif
#include <Eigen/Dense>
#include <cmath>
#include <complex>
#include <functional>
#include "TrigonometricFunctions.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Decomplexify.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Cos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().cos();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::cos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXf> matEigen(
            (single*)A.getDataPointer(), R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXf> matRes(
            (single*)R.getDataPointer(), R.getDimensions().getElementCount());
        matRes = matEigen.array().cos();
#else
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = cos(ptrA[k]);
        }
#endif
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);

#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().cos();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::cos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXd> matEigen(
            (double*)A.getDataPointer(), R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXd> matRes(
            (double*)R.getDataPointer(), R.getDimensions().getElementCount());
        matRes = matEigen.array().cos();
#else
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = cos(ptrA[k]);
        }
#endif
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sin(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::sin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.getDimensions().getElementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = std::sin(ptrA[k]);
        }
#endif
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::sin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.getDimensions().getElementCount());
        matRes = matEigen.array().sin();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = sin(ptrA[k]);
        }
#endif
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::tan(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.getDimensions().getElementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = tan(ptrA[k]);
        }
#endif
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = tan(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.getDimensions().getElementCount());
        matRes = matEigen.array().tan();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = tan(ptrA[k]);
        }
#endif
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Cosh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::cosh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = cosh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::cosh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = cosh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Sinh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::sinh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = sinh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::sinh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = sinh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Tanh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::tanh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = tanh(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::tanh(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = tanh(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Acos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().acos();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::acos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.promoteType(NLS_SCOMPLEX);
            R = Acos(R, needToOverload);
        } else {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, A.getElementCount(), stringVector(), false);
            R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
            single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.getDimensions().getElementCount());
            Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.getDimensions().getElementCount());
            matRes = matEigen.array().acos();
#else
            OMP_PARALLEL_FOR_LOOP(A.getElementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
                ptrR[k] = acos(ptrA[k]);
            }
#endif
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().acos();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::acos(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.promoteType(NLS_DCOMPLEX);
            R = Acos(R, needToOverload);
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.getElementCount(), stringVector(), false);
            R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
            double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.getDimensions().getElementCount());
            Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.getDimensions().getElementCount());
            matRes = matEigen.array().acos();
#else
            OMP_PARALLEL_FOR_LOOP(A.getElementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
                ptrR[k] = acos(ptrA[k]);
            }
#endif
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Asin(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcf> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcf> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().asin();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::asin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::fabs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.promoteType(NLS_SCOMPLEX);
            R = Asin(R, needToOverload);
        } else {
            single* ptrR = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, A.getElementCount(), stringVector(), false);
            R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
            single* ptrA = (single*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXf> matEigen(ptrA, R.getDimensions().getElementCount());
            Eigen::Map<Eigen::VectorXf> matRes(ptrR, R.getDimensions().getElementCount());
            matRes = matEigen.array().asin();
#else
            OMP_PARALLEL_FOR_LOOP(A.getElementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
                ptrR[k] = asin(ptrA[k]);
            }
#endif
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
#if defined(_NLS_WITH_VML)
        Eigen::Map<Eigen::VectorXcd> matEigen(Az, R.getDimensions().getElementCount());
        Eigen::Map<Eigen::VectorXcd> matRes(Rz, R.getDimensions().getElementCount());
        matRes = matEigen.array().asin();
#else
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::asin(Az[k]);
        }
#endif
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.getDataPointer();
        bool needToConvertAsComplex = false;
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            if (std::abs(ptrA[i]) > 1) {
                needToConvertAsComplex = true;
                break;
            }
        }
        if (needToConvertAsComplex) {
            R = A;
            R.promoteType(NLS_DCOMPLEX);
            R = Asin(R, needToOverload);
        } else {
            double* ptrR = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, A.getElementCount(), stringVector(), false);
            R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
            double* ptrA = (double*)A.getDataPointer();
#if defined(_NLS_WITH_VML)
            Eigen::Map<Eigen::VectorXd> matEigen(ptrA, R.getDimensions().getElementCount());
            Eigen::Map<Eigen::VectorXd> matRes(ptrR, R.getDimensions().getElementCount());
            matRes = matEigen.array().asin();
#else
            OMP_PARALLEL_FOR_LOOP(A.getElementCount())
            for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
                ptrR[k] = asin(ptrA[k]);
            }
#endif
        }
    } break;
    }
    return R;
}
//=============================================================================
ArrayOf
Atan(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
        auto* Rz = reinterpret_cast<singlecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::atan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_SINGLE, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = atan(ptrA[k]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>(ptrR);
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            Rz[k] = std::atan(Az[k]);
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, A.getElementCount(), stringVector(), false);
        R = ArrayOf(NLS_DOUBLE, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            ptrR[k] = atan(ptrA[k]);
        }
    } break;
    }
    return R;
}
//=============================================================================
template <typename T>
static inline void
arctanh(bool asComplex, T xr, T xi, T& yr, T& yi)
{
    if ((xr >= 0 && std::isinf(xr)) && (xi == 0)) {
        yr = 0;
        yi = (T)M_PI / 2.0;
        return;
    }
    if ((xr < 0 && std::isinf(xr)) && (xi == 0)) {
        yr = 0;
        yi = -(T)M_PI / 2.0;
        return;
    }
    std::complex<T> v(xr, xi);
    std::complex<T> r = std::atanh(v);
    yr = r.real();
    yi = r.imag();

    if (!asComplex) {
        if (std::isnan(yr) && std::isnan(yi)) {
            yi = 0;
        }
    }
}
//=============================================================================
ArrayOf
Atanh(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf R;
    if (A.isEmpty()) {
        R = A;
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return R;
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getElementCount());
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); k = k + 2) {
            arctanh<single>(true, ptrA[k], ptrA[k + 1], ptrR[k], ptrR[k + 1]);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrR = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, A.getElementCount());
        R = ArrayOf(NLS_SCOMPLEX, A.getDimensions(), ptrR);
        single* ptrA = (single*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            arctanh<single>(false, ptrA[k], 0, ptrR[2 * k], ptrR[(2 * k) + 1]);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getElementCount());
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); k = k + 2) {
            arctanh<double>(true, ptrA[k], ptrA[k + 1], ptrR[k], ptrR[k + 1]);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptrR = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, A.getElementCount());
        R = ArrayOf(NLS_DCOMPLEX, A.getDimensions(), ptrR);
        double* ptrA = (double*)A.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(A.getElementCount())
        for (ompIndexType k = 0; k < (ompIndexType)A.getElementCount(); ++k) {
            arctanh<double>(false, ptrA[k], 0, ptrR[2 * k], ptrR[(2 * k) + 1]);
        }
    } break;
    }
    return decomplexify(R);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
