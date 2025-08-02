//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpsabi"
#endif
#include <boost/math/special_functions/beta.hpp>
#if defined(__GNUC__)
#pragma GCC diagnostic pop
#endif
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "BetaIncomplete.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline Dimensions
maxDimensions(Dimensions& a, Dimensions& b)
{
    Dimensions ret;
    for (unsigned int i = 0; i < std::min(a.getLength(), b.getLength()); i++) {
        ret[i] = (a.getAt(i, false) > b.getAt(i, false)) ? a.getAt(i, false) : b.getAt(i, false);
    }
    return ret;
}
//=============================================================================
template <class T>
ArrayOf
BetaIncomplete(const Dimensions& retDims, NelsonType destinationType, indexType maxLen,
    const ArrayOf& X, const ArrayOf& Y, const ArrayOf& Z, bool isLower)
{
    T* result = (T*)ArrayOf::allocateArrayOf(
        destinationType, retDims.getElementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(destinationType, retDims, result);
    auto* ptrX = (T*)X.getDataPointer();
    auto* ptrY = (T*)Y.getDataPointer();
    auto* ptrZ = (T*)Z.getDataPointer();
    if (isLower) {
        OMP_PARALLEL_FOR_LOOP(maxLen)
        for (ompIndexType i = 0; i < (ompIndexType)maxLen; ++i) {
            T x = (X.isScalar()) ? ptrX[0] : ptrX[i];
            T y = (Y.isScalar()) ? ptrY[0] : ptrY[i];
            T z = (Z.isScalar()) ? ptrZ[0] : ptrZ[i];
            try {
                result[i] = boost::math::ibeta(y, z, x);
            } catch (...) {
                result[i] = 1.0;
            }
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(maxLen)
        for (ompIndexType i = 0; i < (ompIndexType)maxLen; ++i) {
            T x = (X.isScalar()) ? ptrX[0] : ptrX[i];
            T y = (Y.isScalar()) ? ptrY[0] : ptrY[i];
            T z = (Z.isScalar()) ? ptrZ[0] : ptrZ[i];
            try {
                result[i] = 1 - boost::math::ibeta(y, z, x);
            } catch (...) {
                result[i] = 0.;
            }
        }
    }
    return res;
}
//=============================================================================
ArrayOf
BetaIncomplete(
    const ArrayOf& X, const ArrayOf& Y, const ArrayOf& Z, bool isLower, bool& needOverload)
{
    ArrayOf res;
    if (X.getDataClass() == NLS_DOUBLE || X.getDataClass() == NLS_SINGLE && !X.isSparse()) {
        needOverload = false;
    } else {
        needOverload = true;
        return res;
    }
    if (Y.getDataClass() == NLS_DOUBLE || Y.getDataClass() == NLS_SINGLE && !Y.isSparse()) {
        needOverload = false;
    } else {
        needOverload = true;
        return res;
    }
    if (Z.getDataClass() == NLS_DOUBLE || Z.getDataClass() == NLS_SINGLE && !Z.isSparse()) {
        needOverload = false;
    } else {
        needOverload = true;
        return res;
    }
    indexType maxLen
        = std::max(X.getElementCount(), std::max(Y.getElementCount(), Z.getElementCount()));
    Dimensions dimsX = X.getDimensions();
    Dimensions dimsY = Y.getDimensions();
    Dimensions dimsZ = Z.getDimensions();
    Dimensions dimsMaxYZ = maxDimensions(dimsY, dimsZ);
    Dimensions retDims = maxDimensions(dimsX, dimsMaxYZ);
    if (!(X.isScalar()) && !retDims.equals(X.getDimensions())) {
        Error(_("Wrong size for #1 argument."));
    }
    if (!(Y.isScalar()) && !retDims.equals(Y.getDimensions())) {
        Error(_("Wrong size for #2 argument."));
    }
    if (!(Z.isScalar()) && !retDims.equals(Z.getDimensions())) {
        Error(_("Wrong size for #3 argument."));
    }
    if (!Y.isPositive()) {
        Error(_("Wrong value for #2 argument. positive value expected."));
    }
    if (!Z.isPositive()) {
        Error(_("Wrong value for #3 argument. positive value expected."));
    }
    if (X.getDataClass() == NLS_DOUBLE) {
        ArrayOf YY(Y);
        ArrayOf ZZ(Z);
        YY.promoteType(NLS_DOUBLE);
        ZZ.promoteType(NLS_DOUBLE);

        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matX(
            (double*)X.getDataPointer(), 1, dimsX.getElementCount());
        bool isBetween0and1 = ((matX.array() >= 0).all() && (matX.array() <= 1).all());
        if (!isBetween0and1) {
            Error(_("Wrong value for #1 argument. [0, 1] values expected."));
        }
        res = BetaIncomplete<double>(retDims, NLS_DOUBLE, maxLen, X, YY, ZZ, isLower);
    } else if (X.getDataClass() == NLS_SINGLE) {
        ArrayOf YY(Y);
        ArrayOf ZZ(Z);
        YY.promoteType(NLS_SINGLE);
        ZZ.promoteType(NLS_SINGLE);

        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matX(
            (single*)X.getDataPointer(), 1, dimsX.getElementCount());
        bool isBetween0and1 = ((matX.array() >= 0).all() && (matX.array() <= 1).all());
        if (!isBetween0and1) {
            Error(_("Wrong value for #1 argument. [0, 1] values expected."));
        }
        res = BetaIncomplete<single>(retDims, NLS_SINGLE, maxLen, X, YY, ZZ, isLower);
    } else {
        needOverload = true;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
