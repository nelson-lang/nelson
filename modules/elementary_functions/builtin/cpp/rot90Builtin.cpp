//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rot90Builtin.hpp"
#include <cmath>
#include "Rotate90.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static int
normalizeRotationCount(const ArrayOf& value);
static double
rotationCountAsDouble(const ArrayOf& value);
static double
realNumericScalarAsDouble(const ArrayOf& value);
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::rot90Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int k = 1;
    if (argIn.size() == 2) {
        k = normalizeRotationCount(argIn[1]);
    }
    bool needToOverload = false;
    ArrayOf res = Rotate90(argIn[0], k, needToOverload);
    if (needToOverload) {
        OverloadRequired("rot90");
    }
    retval << res;
    return retval;
}
//=============================================================================
static int
normalizeRotationCount(const ArrayOf& value)
{
    if (value.isEmpty() || !value.isScalar()) {
        Error("Nelson:rot90:kNonScalar", "k must be a scalar.");
    }
    double k = rotationCountAsDouble(value);
    if (!std::isfinite(k) || std::floor(k) != k) {
        Error("Nelson:rot90:kNonInteger", "k must be an integer.");
    }
    int rotationCount = static_cast<int>(std::fmod(k, 4.));
    if (rotationCount < 0) {
        rotationCount += 4;
    }
    return rotationCount;
}
//=============================================================================
static double
rotationCountAsDouble(const ArrayOf& value)
{
    ArrayOf local(value);
    if (local.isSparse()) {
        local.makeDense();
    }
    if (local.isCharacterArray()) {
        const charType* ptr = static_cast<const charType*>(local.dataPointer());
        return static_cast<double>(ptr[0]);
    }
    if (local.isLogical()) {
        return local.asLogicalScalar() ? 1. : 0.;
    }
    if (!local.isNumeric()) {
        Error("Nelson:elementary_functions:invalidDataTypeArgumentNumeric",
            "Invalid data type. Argument must be numeric, char, or logical.");
    }
    return realNumericScalarAsDouble(local);
}
//=============================================================================
static double
realNumericScalarAsDouble(const ArrayOf& value)
{
    if (value.isComplex()) {
        double realPart = 0.;
        double imagPart = 0.;
        if (value.dataClass() == NLS_DCOMPLEX) {
            doublecomplex z = value.asDoubleComplexScalar();
            realPart = z.real();
            imagPart = z.imag();
        } else {
            singlecomplex z = value.asSingleComplexScalar();
            realPart = static_cast<double>(z.real());
            imagPart = static_cast<double>(z.imag());
        }
        if (imagPart != 0.) {
            Error("Nelson:validators:mustBeReal", "%s must be real.", "Argument");
        }
        return realPart;
    }
    return value.asDoubleScalar();
}
//=============================================================================
