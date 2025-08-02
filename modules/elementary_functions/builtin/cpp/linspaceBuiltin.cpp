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
#include <complex>
#include "linspaceBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
ArrayOf
linspaceReal(NelsonType destinationClass, T startValue, T endValue, Eigen::Index n)
{
    ArrayOf res;
    if (n <= 0) {
        Dimensions dims(1, 0);
        res = ArrayOf::emptyConstructor(dims);
        res.promoteType(destinationClass);
    } else {
        T* val = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
        Dimensions dims(1, n);
        res = ArrayOf(destinationClass, dims, val);
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> Range(val, n);
        Range = Eigen::Matrix<T, Eigen::Dynamic, 1>::LinSpaced(n, startValue, endValue);
        val[0] = startValue;
        val[n - 1] = endValue;
    }
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
        res = ArrayOf::emptyConstructor(dims);
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
    if (argIn.size() == 2) {
        n = (Eigen::Index)100.;
    } else {
        ArrayOf param3 = argIn[2];
        if (!param3.isScalar()) {
            Error(_W("Inputs parameters must be scalars."));
        }
        if (param3.isComplex()) {
            Error(_W("Third argument should be an integer value."));
        } else {
            // special case about NaN
            double nd = param3.getContentAsDoubleScalar();
            if (std::isnan(nd)) {
                n = (Eigen::Index)1;
            } else {
                param3.promoteType(NLS_INT64);
                int64 nn = param3.getContentAsInteger64Scalar();
                n = (Eigen::Index)nn;
            }
        }
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (!param1.isScalar() || !param2.isScalar()) {
        Error(_W("Inputs parameters must be scalars."));
    }
    ArrayOf res;
    NelsonType destinationClass;
    if (param1.isComplex() || param2.isComplex()) {
        if (param1.getDataClass() == NLS_SCOMPLEX || param2.getDataClass() == NLS_SCOMPLEX) {
            destinationClass = NLS_SCOMPLEX;
            std::complex<single> startValue = param1.getContentAsSingleComplexScalar();
            std::complex<single> endValue = param2.getContentAsSingleComplexScalar();
            res = linspaceComplex<single>(destinationClass, startValue, endValue, n);
        } else {
            destinationClass = NLS_DCOMPLEX;
            std::complex<double> startValue = param1.getContentAsDoubleComplexScalar();
            std::complex<double> endValue = param2.getContentAsDoubleComplexScalar();
            res = linspaceComplex<double>(destinationClass, startValue, endValue, n);
        }
    } else {
        if (param1.getDataClass() == NLS_SINGLE || param2.getDataClass() == NLS_SINGLE) {
            destinationClass = NLS_SINGLE;
            param1.promoteType(destinationClass);
            param2.promoteType(destinationClass);
            single startValue = param1.getContentAsSingleScalar();
            single endValue = param2.getContentAsSingleScalar();
            res = linspaceReal<single>(destinationClass, startValue, endValue, n);
        } else {
            destinationClass = NLS_DOUBLE;
            param1.promoteType(destinationClass);
            param2.promoteType(destinationClass);
            double startValue = param1.getContentAsDoubleScalar();
            double endValue = param2.getContentAsDoubleScalar();
            res = linspaceReal<double>(destinationClass, startValue, endValue, n);
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
