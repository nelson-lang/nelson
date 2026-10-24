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
#include "emptyBuiltin.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringToClass.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Backs `TypeName.empty` / `TypeName.empty(sz)`: the interpreter lowers the
// dotted call to `__empty__(classname, sz...)`. Not for direct use.
//=============================================================================
static indexType
sizeArgumentToLength(double value)
{
    if (!std::isfinite(value)) {
        Error(L"Nelson:emptyArray:invalidSize", L"Size inputs must be finite integers.");
    }
    if (value < 0) {
        value = 0;
    }
    auto length = static_cast<indexType>(value);
    if (static_cast<double>(length) != value) {
        Error(L"Nelson:emptyArray:invalidSize", L"Size inputs must be finite integers.");
    }
    return length;
}
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::emptyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    const std::string className = argIn[0].asUtf8String();
    NelsonType destClass = StringToClass(className);

    std::vector<indexType> dimensionsVector;
    if (argIn.size() == 1) {
        dimensionsVector = { 0, 0 };
    } else if (argIn.size() == 2) {
        const ArrayOf& sizeArgument = argIn[1];
        if (!sizeArgument.isNumeric() || !sizeArgument.isVector()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"numeric vector");
        }
        std::vector<double> values = sizeArgument.asDoubleVector();
        if (values.empty()) {
            dimensionsVector = { 0, 0 };
        } else if (values.size() == 1) {
            indexType length = sizeArgumentToLength(values[0]);
            dimensionsVector = { length, length };
        } else {
            for (double value : values) {
                dimensionsVector.push_back(sizeArgumentToLength(value));
            }
        }
    } else {
        for (size_t k = 1; k < argIn.size(); ++k) {
            if (!argIn[k].isNumeric() || !argIn[k].isScalar()) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.",
                    static_cast<int>(k + 1), L"numeric scalar");
            }
            dimensionsVector.push_back(sizeArgumentToLength(argIn[k].asDoubleScalar()));
        }
    }

    Dimensions dimensions(dimensionsVector);
    dimensions.simplify();
    if (dimensions.elementCount() != 0) {
        Error(L"Nelson:emptyArray:notEmpty",
            L"At least one dimension must be zero to create an empty array.");
    }
    switch (destClass) {
    case NLS_CELL_ARRAY: {
        retval << ArrayOf::emptyCell(dimensions);
    } break;
    case NLS_STRUCT_ARRAY: {
        retval << ArrayOf::emptyStruct(stringVector(), dimensions);
    } break;
    case NLS_FUNCTION_HANDLE: {
        // same field layout as ArrayOf::functionHandle
        stringVector fieldnames { "handle", "owner" };
        retval << ArrayOf(NLS_FUNCTION_HANDLE, dimensions, nullptr, false, fieldnames);
    } break;
    default: {
        retval << ArrayOf(destClass, dimensions, (void*)nullptr, false);
    } break;
    }
    return retval;
}
//=============================================================================
