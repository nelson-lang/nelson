//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include <complex>
#include "linspaceBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
ArrayOf
linspaceReal(Class destinationClass, T startValue, T endValue, Eigen::Index n)
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
linspaceComplex(
    Class destinationClass, std::complex<T> startValue, std::complex<T> endValue, Eigen::Index n)
{
    ArrayOf res;
    if (n <= 0) {
        Dimensions dims(1, 0);
        res = ArrayOf::emptyConstructor(dims);
        res.promoteType(destinationClass);
    } else {
        T* val = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
        std::complex<T>* cz
            = reinterpret_cast<std::complex<T>*>((T*)val);
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
Nelson::ElementaryFunctionsGateway::linspaceBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2 && argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
            param3.promoteType(NLS_INT64);
            int64 nn = param3.getContentAsInteger64Scalar();
            n = (Eigen::Index)nn;
        }
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (!param1.isScalar() || !param2.isScalar()) {
        Error(_W("Inputs parameters must be scalars."));
    }
    ArrayOf res;
    Class destinationClass;
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
    retval.push_back(res);
    return retval;
}
//=============================================================================
