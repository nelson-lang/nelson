//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <numeric>
#include <string>
#include "typecastBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringToClass.hpp"
#include "ClassToString.hpp"
#include "NelsonTypeTraits.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static size_t
supportedClassByteSize(NelsonType type)
{
    // typecast only accepts the fixed-width real numeric classes.
    if (TypeTraits::isInteger(type) || TypeTraits::isFloat(type)) {
        return static_cast<size_t>(TypeTraits::byteSize(type));
    }
    return 0;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::typecastBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    const ArrayOf& X = argIn[0];
    if (X.isSparse()) {
        Error(L"Nelson:elementary_functions:typecastInputArgumentFull",
            L"typecast: the input argument must be full.");
    }
    if (X.isComplex()) {
        Error(L"Nelson:elementary_functions:typecastInputArgumentNoncomplex",
            L"typecast: the input argument must be noncomplex.");
    }
    size_t srcSize = supportedClassByteSize(X.dataClass());
    if (srcSize == 0) {
        Error(L"Nelson:elementary_functions:typecastInputArgumentNumericScalar",
            L"typecast: the input argument must be a numeric scalar or vector.");
    }
    Dimensions dimsX = X.dimensions();
    bool isScalarOrVector = dimsX.isEmpty(false) || dimsX.isScalar() || dimsX.isVector();
    if (!isScalarOrVector) {
        Error(L"Nelson:elementary_functions:firstArgumentVectorEmpty",
            L"First argument must be a vector or empty.");
    }

    bool haveError = false;
    std::string destClassName = argIn[1].asUtf8String();
    NelsonType destClass = StringToClass(destClassName, haveError);
    size_t destSize = haveError ? 0 : supportedClassByteSize(destClass);
    if (destSize == 0) {
        Error(L"Nelson:elementary_functions:typecastDestinationClassNumericType",
            L"typecast: the destination class must be a numeric type.");
    }

    indexType count = X.elementCount();
    size_t totalBytes = (size_t)count * srcSize;
    if ((totalBytes % destSize) != 0) {
        size_t required = destSize / std::gcd(srcSize, destSize);
        std::string msg = "The first input must contain a multiple of " + std::to_string(required)
            + " elements to convert from real " + ClassToString(X.dataClass()) + " ("
            + std::to_string(srcSize * 8) + " bits) to real " + ClassToString(destClass) + " ("
            + std::to_string(destSize * 8) + " bits).";
        Error("Nelson:typecast:invalidSize", msg);
    }
    indexType destCount = (indexType)(totalBytes / destSize);

    // Preserve orientation: a column vector stays a column, everything else
    // (scalar, row vector, empty) becomes a row.
    bool isColumn = (dimsX.columns() == 1 && dimsX.rows() != 1);
    Dimensions destDims = isColumn ? Dimensions(destCount, destCount == 0 ? 0 : 1)
                                   : Dimensions(destCount == 0 ? 0 : 1, destCount);

    void* out = ArrayOf::allocateArrayOf(destClass, destCount);
    if (totalBytes > 0) {
        memcpy(out, X.dataPointer(), totalBytes);
    }
    retval << ArrayOf(destClass, destDims, out, false);
    return retval;
}
//=============================================================================
