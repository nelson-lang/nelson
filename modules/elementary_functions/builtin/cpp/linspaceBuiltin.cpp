//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <cmath>
#include <complex>
#include "linspaceBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Points are built from both ends with a step computed as high/(n-1) - low/(n-1)
// (no overflow for huge endpoints: linspace(1e308, -1e308, 3) is 1e308 0 -1e308).
// Two infinite endpoints of opposite signs give the endpoints on each half and 0
// in the middle; of the same sign, every point is that endpoint.
template <class T>
ArrayOf
linspaceReal(NelsonType destinationClass, T startValue, T endValue, Eigen::Index n)
{
    ArrayOf res;
    if (n <= 0) {
        Dimensions dims(1, 0);
        res = ArrayOf::empty(dims);
        res.promoteType(destinationClass);
        return res;
    }
    T* val = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
    Dimensions dims(1, n);
    res = ArrayOf(destinationClass, dims, val);
    if (n == 1) {
        val[0] = endValue;
        return res;
    }
    const Eigen::Index n1 = n - 1;
    if (std::isinf(startValue) && std::isinf(endValue)) {
        for (Eigen::Index k = 0; k <= n1; ++k) {
            if (startValue == endValue || 2 * k < n1) {
                val[k] = startValue;
            } else if (2 * k > n1) {
                val[k] = endValue;
            } else {
                val[k] = T(0);
            }
        }
        return res;
    }
    T step = (endValue - startValue) / static_cast<T>(n1);
    if (std::isfinite(step) || std::isinf(startValue) || std::isinf(endValue)) {
        // an infinite endpoint propagates through the step (Inf or NaN)
        for (Eigen::Index k = 0; k <= n1; ++k) {
            val[k] = startValue + static_cast<T>(k) * step;
        }
    } else {
        // the difference of finite endpoints overflowed: weight the endpoints
        for (Eigen::Index k = 0; k <= n1; ++k) {
            T t = static_cast<T>(k) / static_cast<T>(n1);
            val[k] = startValue * (T(1) - t) + endValue * t;
        }
    }
    val[0] = startValue;
    val[n1] = endValue;
    return res;
}
//=============================================================================
template <class T>
ArrayOf
linspaceComplex(NelsonType destinationClass, std::complex<T> startValue, std::complex<T> endValue,
    Eigen::Index n)
{
    ArrayOf res;
    if (n <= 0) {
        Dimensions dims(1, 0);
        res = ArrayOf::empty(dims);
        res.promoteType(destinationClass);
    } else {
        T* val = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
        std::complex<T>* cz = reinterpret_cast<std::complex<T>*>((T*)val);
        Dimensions dims(1, n);
        res = ArrayOf(destinationClass, dims, val);
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> Range(cz, n);
        Range
            = Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>::LinSpaced(n, startValue, endValue);
        cz[0] = startValue;
        cz[n - 1] = endValue;
    }
    return res;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::linspaceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    Eigen::Index n;
    bool nComplex = false;
    bool nPointsIsNaN = false;
    if (argIn.size() == 2) {
        n = (Eigen::Index)100.;
    } else {
        ArrayOf param3 = argIn[2];
        if (!param3.isScalar()) {
            Error(L"Nelson:elementary_functions:inputsParametersScalars",
                L"Inputs parameters must be scalars.");
        }
        if (param3.isComplex()) {
            Error(L"Nelson:elementary_functions:thirdArgumentIntegerValue",
                L"Third argument should be an integer value.");
        } else {
            param3.promoteType(NLS_DOUBLE);
            double nd = param3.asDoubleScalar();
            if (std::isnan(nd)) {
                // NaN points: a single NaN
                nPointsIsNaN = true;
                n = (Eigen::Index)1;
            } else if (nd > 0 && std::isinf(nd)) {
                Error(L"Nelson:elementary_functions:requestedArrayTooLarge",
                    L"Requested array exceeds the maximum possible variable size.");
            } else {
                // non integer number of points: n is floored
                n = (nd < 0) ? (Eigen::Index)0 : (Eigen::Index)std::floor(nd);
            }
        }
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (!param1.isScalar() || !param2.isScalar()) {
        Error(L"Nelson:elementary_functions:inputsParametersScalars",
            L"Inputs parameters must be scalars.");
    }
    if (nPointsIsNaN) {
        param2 = ArrayOf::doubleScalar(std::nan(""));
        if (param1.dataClass() == NLS_SINGLE || param1.dataClass() == NLS_SCOMPLEX) {
            param2.promoteType(NLS_SINGLE);
        }
    }
    ArrayOf res;
    NelsonType destinationClass;
    if (param1.isComplex() || param2.isComplex()) {
        if (param1.dataClass() == NLS_SCOMPLEX || param2.dataClass() == NLS_SCOMPLEX) {
            destinationClass = NLS_SCOMPLEX;
            std::complex<single> startValue = param1.asSingleComplexScalar();
            std::complex<single> endValue = param2.asSingleComplexScalar();
            res = linspaceComplex<single>(destinationClass, startValue, endValue, n);
        } else {
            destinationClass = NLS_DCOMPLEX;
            std::complex<double> startValue = param1.asDoubleComplexScalar();
            std::complex<double> endValue = param2.asDoubleComplexScalar();
            res = linspaceComplex<double>(destinationClass, startValue, endValue, n);
        }
    } else {
        if (param1.dataClass() == NLS_SINGLE || param2.dataClass() == NLS_SINGLE) {
            destinationClass = NLS_SINGLE;
            param1.promoteType(destinationClass);
            param2.promoteType(destinationClass);
            single startValue = param1.asSingleScalar();
            single endValue = param2.asSingleScalar();
            res = linspaceReal<single>(destinationClass, startValue, endValue, n);
        } else {
            destinationClass = NLS_DOUBLE;
            param1.promoteType(destinationClass);
            param2.promoteType(destinationClass);
            double startValue = param1.asDoubleScalar();
            double endValue = param2.asDoubleScalar();
            res = linspaceReal<double>(destinationClass, startValue, endValue, n);
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
