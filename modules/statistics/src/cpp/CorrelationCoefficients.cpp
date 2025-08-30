//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <algorithm>
#include "CorrelationCoefficients.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "RealPart.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
CorrelationCoefficients(const T* data, indexType R, indexType C, NelsonType destination)
{
    if (R == 0 && C == 0) {
        Dimensions dimsOut(1, 1);
        T* outData = (T*)ArrayOf::allocateArrayOf(destination, 1);
        outData[0] = (T)std::nan("");
        return ArrayOf(destination, dimsOut, outData);
    }
    if (R == 1 && C != 1) {
        std::swap(R, C);
    }
    T* outData = (T*)ArrayOf::allocateArrayOf(destination, C * C);
    Dimensions dimsOut(C, C);
    ArrayOf res = ArrayOf(destination, dimsOut, outData);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> RR((T*)outData, C, C);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> X2((T*)data, R, C);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> ones(R, 1);
    X2 = X2 - ones.setOnes(R, 1) * X2.colwise().sum() / R;
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> c = (X2.adjoint() * X2 / (R - 1)).conjugate();
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> d = c.diagonal();
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> dd = d * d.transpose();
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> sq = dd.array().sqrt();
    RR = c.array() / sq.array();
    return res;
}
//=============================================================================
ArrayOf
CorrelationCoefficients(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    Dimensions dimsA = A.getDimensions();
    if (!dimsA.is2D() && !dimsA.isVector()) {
        Error(_W("Inputs must be 2-D."));
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        res = CorrelationCoefficients<double>(
            (double*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns(), NLS_DOUBLE);
    } break;
    case NLS_SINGLE: {
        res = CorrelationCoefficients<single>(
            (single*)A.getDataPointer(), dimsA.getRows(), dimsA.getColumns(), NLS_SINGLE);
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf realA = RealPart(A);
        res = CorrelationCoefficients<single>(
            (single*)realA.getDataPointer(), dimsA.getRows(), dimsA.getColumns(), NLS_SINGLE);
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf realA = RealPart(A);
        res = CorrelationCoefficients<double>(
            (double*)realA.getDataPointer(), dimsA.getRows(), dimsA.getColumns(), NLS_DOUBLE);
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
