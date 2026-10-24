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
#include "diffBuiltin.hpp"
#include "Difference.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
asRealScalar(const ArrayOf& value, double& result);
static double
parseDifferenceOrder(const ArrayOf& value);
static indexType
parseDifferenceDimension(const ArrayOf& value, const ArrayOf& x);
//=============================================================================
// x = diff(X), x = diff(X, n), x = diff(X, n, dim)
ArrayOfVector
Nelson::LinearAlgebraGateway::diffBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    double order = 1;
    if (argIn.size() > 1) {
        order = parseDifferenceOrder(argIn[1]);
    }
    indexType dim = 0;
    if (argIn.size() > 2) {
        dim = parseDifferenceDimension(argIn[2], argIn[0]);
    }
    DifferenceSubtraction subtract = [eval](const ArrayOf& a, const ArrayOf& b) {
        ArrayOfVector operands;
        operands << a << b;
        return eval->minusOperator(operands);
    };
    ArrayOfVector retval;
    retval << Difference(argIn[0], order, dim, subtract);
    return retval;
}
//=============================================================================
static bool
asRealScalar(const ArrayOf& value, double& result)
{
    if (!value.isScalar() || value.isSparse() || value.isComplex()
        || !(value.isNumeric() || value.isLogical() || value.isCharacterArray())) {
        return false;
    }
    ArrayOf asDouble = value;
    asDouble.promoteType(NLS_DOUBLE);
    result = asDouble.asDoubleScalar();
    return true;
}
//=============================================================================
static double
parseDifferenceOrder(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return 1;
    }
    double order = 0;
    if (!asRealScalar(value, order) || !(order > 0) || (order != std::round(order))) {
        Error(L"Nelson:diff:differenceOrderMustBePositiveInteger",
            L"%s must be a positive integer scalar.", L"Difference order N");
    }
    return order;
}
//=============================================================================
static indexType
parseDifferenceDimension(const ArrayOf& value, const ArrayOf& x)
{
    Dimensions dims = x.dimensions();
    dims.simplify();
    double nd = static_cast<double>(std::max<indexType>(dims.length(), 2));
    double dim = 0;
    if (!asRealScalar(value, dim) || (dim != std::round(dim)) || !(dim > 0) || !(dim < nd + 1)) {
        Error(L"Nelson:getdimarg:dimensionMustBePositiveInteger",
            L"Dimension argument must be a positive integer scalar within indexing range.");
    }
    return static_cast<indexType>(dim);
}
//=============================================================================
