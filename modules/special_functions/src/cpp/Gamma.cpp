//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <cmath>
#include <functional>
#include <limits>
#include "Gamma.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static T
gammaT(T v)
{
    if ((v == 0) || std::isinf(v) || (v >= 172)) {
        return std::numeric_limits<T>::infinity();
    }
    if ((v <= 0) && (floor(v) == v)) {
        return std::numeric_limits<T>::infinity();
    }
    return std::tgamma(v);
}
//=============================================================================
template <class T>
ArrayOf
Gamma(const ArrayOf& arrayIn)
{
    ArrayOf res;
    Dimensions dimsIn = arrayIn.getDimensions();
    T* ptrOut = static_cast<T*>(const_cast<void*>(static_cast<const void*>(ArrayOf::allocateArrayOf(
        arrayIn.getDataClass(), dimsIn.getElementCount(), stringVector(), false))));

    T* ptrIn
        = static_cast<T*>(const_cast<void*>(static_cast<const void*>(arrayIn.getDataPointer())));
    if (dimsIn.isEmpty(false)) {
        return ArrayOf(arrayIn.getDataClass(), dimsIn, ptrOut);
    }
    if (dimsIn.isScalar()) {
        ptrOut[0] = gammaT<T>(ptrIn[0]);
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
        for (ompIndexType k = 0; k < (ompIndexType)dimsIn.getElementCount(); k++) {
            ptrOut[k] = gammaT<T>(ptrIn[k]);
        }
#else
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matOut(ptrOut, dimsIn.getElementCount());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matIn(ptrIn, dimsIn.getElementCount());
        matOut = matIn.unaryExpr(std::ref(gammaT<T>));
#endif
    }
    return ArrayOf(arrayIn.getDataClass(), dimsIn, ptrOut);
}
//=============================================================================
ArrayOf
Gamma(const ArrayOf& arrayIn)
{
    ArrayOf res;
    if (!arrayIn.isEmpty()) {
        if (arrayIn.getDataClass() == NLS_DOUBLE) {
            return Gamma<double>(arrayIn);
        } else {
            return Gamma<single>(arrayIn);
        }
    } else {
        res = arrayIn;
        res.ensureSingleOwner();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
