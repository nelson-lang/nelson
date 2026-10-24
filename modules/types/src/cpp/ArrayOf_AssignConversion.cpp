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
#include "ArrayOfAssignConversion.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static StringToDoubleAssignmentConverter stringToDoubleAssignmentConverter = nullptr;
//=============================================================================
static bool
isNumericCharOrLogicalClass(NelsonType type);
//=============================================================================
static void
raiseNotConvertible(const ArrayOf& destination, const ArrayOf& rightData);
//=============================================================================
static void
convertStringValue(const ArrayOf& destination, ArrayOf& rightData);
//=============================================================================
static void
convertToFloatingClass(ArrayOf& destination, ArrayOf& rightData, NelsonType complexClass);
//=============================================================================
static void
checkNoNaN(const ArrayOf& rightData);
//=============================================================================
void
setStringToDoubleAssignmentConverter(StringToDoubleAssignmentConverter converter)
{
    stringToDoubleAssignmentConverter = converter;
}
//=============================================================================
bool
convertAssignmentValueToDestinationClass(ArrayOf& destination, ArrayOf& rightData)
{
    const NelsonType destinationClass = destination.dataClass();
    const NelsonType valueClass = rightData.dataClass();
    if (destinationClass == valueClass || !isNumericCharOrLogicalClass(destinationClass)) {
        return false;
    }
    if (!isNumericCharOrLogicalClass(valueClass) && valueClass != NLS_STRING_ARRAY) {
        return false;
    }
    if (destinationClass == NLS_DOUBLE && destination.isEmpty()) {
        return false;
    }
    if (valueClass == NLS_STRING_ARRAY) {
        convertStringValue(destination, rightData);
        if (rightData.dataClass() == destinationClass) {
            return true;
        }
    }
    switch (destinationClass) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        convertToFloatingClass(destination, rightData, NLS_DCOMPLEX);
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        convertToFloatingClass(destination, rightData, NLS_SCOMPLEX);
    } break;
    case NLS_LOGICAL: {
        if (rightData.isComplex()) {
            Error(L"Nelson:types:conversionNotSupported:conversionLogicalFromComplexNotPossible",
                L"Conversion to logical from complex is not possible.");
        }
        checkNoNaN(rightData);
        rightData.promoteType(NLS_LOGICAL);
    } break;
    case NLS_CHAR: {
        if (rightData.isComplex()) {
            Error(L"Nelson:types:conversionNotSupported:conversionCharFromComplexNotPossible",
                L"Conversion to char from complex is not possible.");
        }
        rightData.promoteType(NLS_CHAR);
    } break;
    default: {
        // integer destination: values round and saturate.
        if (rightData.isComplex() && !rightData.allReal()) {
            Error("Nelson:integer:cannotConvertFromComplex",
                "Invalid conversion from complex matrix to '%s' matrix.", ClassName(destination));
        }
        rightData.promoteType(destinationClass);
    } break;
    }
    return true;
}
//=============================================================================
bool
isNumericCharOrLogicalClass(NelsonType type)
{
    return (type >= NLS_DOUBLE && type <= NLS_LOGICAL) || (type == NLS_CHAR);
}
//=============================================================================
void
raiseNotConvertible(const ArrayOf& destination, const ArrayOf& rightData)
{
    Error("Nelson:types:assignmentNotConvertible",
        "Unable to perform assignment because value of type '%s' is not convertible to "
        "'%s'.",
        ClassName(rightData), ClassName(destination));
}
//=============================================================================
void
convertStringValue(const ArrayOf& destination, ArrayOf& rightData)
{
    switch (destination.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        if (stringToDoubleAssignmentConverter == nullptr) {
            raiseNotConvertible(destination, rightData);
        }
        rightData = stringToDoubleAssignmentConverter(rightData);
    } break;
    case NLS_CHAR: {
        rightData = ArrayOf::characterArrayFromStringArray(rightData, false);
    } break;
    default: {
        raiseNotConvertible(destination, rightData);
    } break;
    }
}
//=============================================================================
void
convertToFloatingClass(ArrayOf& destination, ArrayOf& rightData, NelsonType complexClass)
{
    if (rightData.isComplex()) {
        if (destination.dataClass() != complexClass) {
            destination.promoteType(complexClass);
        }
        rightData.promoteType(complexClass);
    } else {
        rightData.promoteType(destination.dataClass());
    }
}
//=============================================================================
void
checkNoNaN(const ArrayOf& rightData)
{
    if (rightData.isSparse()) {
        return;
    }
    const indexType count = rightData.elementCount();
    bool hasNaN = false;
    if (rightData.dataClass() == NLS_DOUBLE) {
        const auto* values = static_cast<const double*>(rightData.dataPointer());
        for (indexType k = 0; k < count && !hasNaN; ++k) {
            hasNaN = std::isnan(values[k]);
        }
    } else if (rightData.dataClass() == NLS_SINGLE) {
        const auto* values = static_cast<const single*>(rightData.dataPointer());
        for (indexType k = 0; k < count && !hasNaN; ++k) {
            hasNaN = std::isnan(values[k]);
        }
    }
    if (hasNaN) {
        Error(L"Nelson:types:conversionNotSupported:conversionLogicalWithNaNNotPossible",
            L"Conversion to logical with NaN is not possible.");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
