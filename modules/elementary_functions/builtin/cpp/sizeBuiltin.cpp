//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include <algorithm>
#include <vector>
#include "sizeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
collectSizeDimensions(const ArrayOf& arg, int position, std::vector<indexType>& dimsVal);
static double
queriedDimension(const Dimensions& sze, indexType dimension);
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::sizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bChooseDimension = false;
    std::vector<indexType> dimsVal;
    nargincheck(argIn, 1);
    if (argIn.size() == 2 && argIn[1].isRowVectorCharacterArray()) {
        bChooseDimension = true;
        std::wstring str = argIn[1].asWideString();
        if ((str == L"r") || (str == L"c")) {
            dimsVal.push_back((str == L"r") ? 1 : 2);
        } else {
            Error(L"Nelson:elementary_functions:wrongValueArgument2R",
                L"Wrong value for argument #%d. 'r' or 'c' expected", 2);
        }
    } else if (argIn.size() == 2) {
        // size(A, dims) with a scalar or a vector of dimensions
        bChooseDimension = true;
        collectSizeDimensions(argIn[1], 2, dimsVal);
    } else if (argIn.size() > 2) {
        // size(A, d1, d2, ...): one scalar dimension per argument
        bChooseDimension = true;
        for (size_t k = 1; k < argIn.size(); ++k) {
            if (!argIn[k].isScalar()) {
                Error(L"Nelson:elementary_functions:wrongSizeArgumentScalar",
                    L"Wrong size for argument #%d: scalar expected.", static_cast<int>(k + 1));
            }
            collectSizeDimensions(argIn[k], static_cast<int>(k + 1), dimsVal);
        }
    }
    ArrayOf param1 = argIn[0];
    Dimensions sze(param1.dimensions());
    sze.simplify();
    if (bChooseDimension) {
        nargoutcheck(nLhs, 0, static_cast<int>(std::max<size_t>(dimsVal.size(), 1)));
        if (nLhs > 1) {
            // one output per queried dimension
            if (static_cast<size_t>(nLhs) != dimsVal.size()) {
                Error(L"Nelson:elementary_functions:sizeOutputsDimensions",
                    L"Number of outputs must match the number of queried dimensions.");
            }
            for (size_t k = 0; k < dimsVal.size(); ++k) {
                retval << ArrayOf::doubleScalar(queriedDimension(sze, dimsVal[k]));
            }
            return retval;
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsVal.size());
        Dimensions outDims(1, dimsVal.size());
        ArrayOf res = ArrayOf(NLS_DOUBLE, outDims, ptr);
        for (indexType k = 0; k < (indexType)dimsVal.size(); k++) {
            ptr[k] = queriedDimension(sze, dimsVal[k]);
        }
        retval << res;
    } else {
        if (nLhs > 1) {
            for (int i = 0; i < nLhs - 1; i++) {
                retval << ArrayOf::doubleScalar(static_cast<double>(sze[i]));
            }
            // the last output collapses the remaining dimensions
            double last = static_cast<double>(sze[nLhs - 1]);
            for (indexType i = nLhs; i < sze.length(); i++) {
                last *= static_cast<double>(sze[i]);
            }
            retval << ArrayOf::doubleScalar(last);
        } else {
            double* dims = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, sze.length(), stringVector(), true));
            Dimensions retDim(2);
            if (sze.length() == 0) {
                retDim[0] = 0;
                retDim[1] = 0;
            } else {
                for (indexType i = 0; i < sze.length(); i++) {
                    dims[i] = static_cast<double>(sze[i]);
                }
                retDim[0] = 1;
                retDim[1] = sze.length();
            }
            retval << ArrayOf(NLS_DOUBLE, retDim, dims);
        }
    }
    return retval;
}
//=============================================================================
// dimension numbers are real positive integers (scalar, row or column vector;
// an empty vector queries nothing)
static void
collectSizeDimensions(const ArrayOf& arg, int position, std::vector<indexType>& dimsVal)
{
    if (!(arg.isNumeric() || arg.dataClass() == NLS_LOGICAL) || arg.isSparse() || arg.isComplex()) {
        Error(L"Nelson:elementary_functions:wrongTypeArgument2Numeric",
            L"Wrong type for argument #%d. numeric values expected", position);
    }
    if (!arg.isEmpty() && !arg.isScalar() && !arg.isVector()) {
        Error(L"Nelson:elementary_functions:wrongSizeArgument2Row",
            L"Wrong size for argument #%d: row vector or scalar expected.", position);
    }
    ArrayOf values(arg);
    values.promoteType(NLS_DOUBLE);
    const auto* ptr = static_cast<const double*>(values.dataPointer());
    for (indexType k = 0; k < values.elementCount(); ++k) {
        if (!std::isfinite(ptr[k]) || std::floor(ptr[k]) != ptr[k] || ptr[k] < 1
            || ptr[k] > static_cast<double>(std::numeric_limits<indexType>::max())) {
            Error(L"Nelson:elementary_functions:dimensionPositiveInteger",
                L"Dimension argument must be a positive integer scalar within indexing range.");
        }
        dimsVal.push_back(static_cast<indexType>(ptr[k]));
    }
}
//=============================================================================
static double
queriedDimension(const Dimensions& sze, indexType dimension)
{
    if (dimension > sze.length()) {
        return 1.0;
    }
    return static_cast<double>(sze.at(dimension - 1));
}
//=============================================================================
