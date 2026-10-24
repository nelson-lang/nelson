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
#include "FindCommonColonType.hpp"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ClassToString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
getColonCommonBasicType(const ArrayOf& A, const ArrayOf& B);
static NelsonType
getColonCommonType(NelsonType typeA, NelsonType typeB);
static void
validateCharacterOperands(const ArrayOfVector& argIn);
static void
validateLogicalOperands(const ArrayOfVector& argIn);
static void
validateDoubleOperandsOfIntegerRange(const ArrayOfVector& argIn, NelsonType integerType);
static void
validateCharacterOperandsForTwoArgs(const ArrayOfVector& argIn);
static void
validateCharacterOperandsForThreeArgs(const ArrayOfVector& argIn);
static NelsonType
determineCommonType(const ArrayOfVector& argIn);
//=============================================================================
bool
findColonCommonType(
    const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse, std::string& typeName)
{
    isSparse = false;
    commonType = NLS_UNKNOWN;

    const size_t numArgs = argIn.size();
    if (numArgs == 0) {
        return false;
    }

    isSparse = argIn[0].isSparse();
    for (int i = 1; i < numArgs; ++i) {
        isSparse = isSparse || argIn[i].isSparse();
    }

    bool isMixed = false;
    commonType = argIn[0].dataClass();
    for (int i = 1; i < numArgs; ++i) {
        NelsonType argType = argIn[i].dataClass();
        if (commonType != argType) {
            isMixed = true;
            break;
        }
    }

    if (!isMixed) {
        if (commonType == NLS_LOGICAL) {
            commonType = NLS_DOUBLE;
            typeName = NLS_DOUBLE_STR;
            return true;
        }
        if (argIn[0].isClassType()) {
            typeName = argIn[0].classType();
        } else if (argIn[0].isHandle()) {
            if (argIn[0].isEmpty()) {
                typeName = NLS_HANDLE_STR;
            } else {
                typeName = argIn[0].handleCategory();
            }
        } else if (argIn[0].isGraphicsObject()) {
            typeName = NLS_GO_HANDLE_STR;
        } else {
            typeName = ClassToString(commonType);
        }
        return true;
    }

    bool hasReference = false;
    for (int i = 0; i < numArgs; ++i) {
        NelsonType argType = argIn[i].dataClass();
        if (argType > NLS_CHAR) {
            hasReference = true;
            if (argType == NLS_STRING_ARRAY || argType == NLS_CELL_ARRAY) {
                commonType = argType;
                typeName
                    = (argType == NLS_STRING_ARRAY) ? NLS_STRING_ARRAY_STR : NLS_CELL_ARRAY_STR;
                return true;
            } else if (argType == NLS_CLASS_ARRAY || argType == NLS_CLASSDEF_ARRAY) {
                commonType = argType;
                typeName = argIn[i].classType();
                return true;
            } else if (argType == NLS_STRUCT_ARRAY) {
                commonType = NLS_STRUCT_ARRAY;
                typeName = NLS_STRUCT_ARRAY_STR;
                return true;
            } else if (argType == NLS_HANDLE) {
                commonType = NLS_HANDLE;
                typeName = argIn[i].handleCategory();
                return true;
            } else if (argType == NLS_GO_HANDLE) {
                commonType = NLS_GO_HANDLE;
                typeName = NLS_GO_HANDLE_STR;
                return true;
            }
        }
    }

    if (!hasReference) {
        validateCharacterOperands(argIn);
        validateLogicalOperands(argIn);
        commonType = determineCommonType(argIn);
        if (IS_INTEGER_TYPE(commonType)) {
            validateDoubleOperandsOfIntegerRange(argIn, commonType);
        }

        if (commonType == NLS_UNKNOWN) {
            Error(L"Nelson:colon:mixedCharOperands",
                L"Colon operands must be all the same type, or mixed with real scalar doubles.");
        }
    }
    if (commonType == NLS_LOGICAL) {
        // logical operands count as doubles (true:3 is 1:3)
        commonType = NLS_DOUBLE;
    }
    typeName = ClassToString(commonType);
    if (isSparse) {
        typeName = NLS_SPARSE_STR + typeName;
    }

    return commonType != NLS_UNKNOWN;
}
//=============================================================================
void
validateCharacterOperands(const ArrayOfVector& argIn)
{
    if (argIn.size() == 2) {
        validateCharacterOperandsForTwoArgs(argIn);
    } else {
        // numArgs == 3
        validateCharacterOperandsForThreeArgs(argIn);
    }
}
//=============================================================================
void
validateLogicalOperands(const ArrayOfVector& argIn)
{
    // logical operands count as doubles, and only combine with doubles
    bool hasLogical = false;
    bool hasOther = false;
    for (const ArrayOf& arg : argIn) {
        NelsonType argType = arg.dataClass();
        if (argType == NLS_LOGICAL) {
            hasLogical = true;
        } else if (argType != NLS_DOUBLE) {
            hasOther = true;
        }
    }
    if (hasLogical && hasOther) {
        Error(L"Nelson:colon:mixedCharOperands",
            L"Colon operands must be all the same type, or mixed with real scalar doubles.");
    }
}
//=============================================================================
void
validateDoubleOperandsOfIntegerRange(const ArrayOfVector& argIn, NelsonType integerType)
{
    // a double operand combined with an integer class is an integer value of that class
    int bits = 8;
    if (integerType == NLS_INT16 || integerType == NLS_UINT16) {
        bits = 16;
    } else if (integerType == NLS_INT32 || integerType == NLS_UINT32) {
        bits = 32;
    } else if (integerType == NLS_INT64 || integerType == NLS_UINT64) {
        bits = 64;
    }
    bool isSigned = IS_SIGNED_INTEGER_TYPE(integerType);
    double lowest = isSigned ? -std::ldexp(1.0, bits - 1) : 0.0;
    double highest = std::ldexp(1.0, isSigned ? bits - 1 : bits);
    for (const ArrayOf& arg : argIn) {
        if (arg.isEmpty()) {
            // an empty operand gives an empty range whatever the other operands
            return;
        }
    }
    for (size_t k = 0; k < argIn.size(); k++) {
        const ArrayOf& arg = argIn[k];
        bool isStep = (argIn.size() == 3 && k == 1);
        if (arg.dataClass() != NLS_DOUBLE || !arg.isScalar() || arg.isComplex()) {
            continue;
        }
        double value = arg.asDoubleScalar();
        // the step is only required to be an integer value, not to fit in the class
        if (std::trunc(value) != value || (!isStep && (value < lowest || value >= highest))) {
            Error(L"Nelson:colon:mixedNonDoubleOperands",
                L"Colon operands must be all the same type, or mixed with real double scalar.");
        }
    }
}
//=============================================================================
void
validateCharacterOperandsForTwoArgs(const ArrayOfVector& argIn)
{
    if (argIn[0].isCharacterArray() || argIn[1].isCharacterArray()) {
        Error(L"Nelson:colon:charOperandsMustBeChar",
            L"For colon operator with char operands, first and last operands must be char.");
    }
}
//=============================================================================
void
validateCharacterOperandsForThreeArgs(const ArrayOfVector& argIn)
{
    if (argIn[1].isCharacterArray()
        || (argIn[0].isCharacterArray() != argIn[2].isCharacterArray())) {
        Error(L"Nelson:colon:charOperandsMustBeChar",
            L"For colon operator with char operands, first and last operands must be char.");
    }
}
//=============================================================================
NelsonType
determineCommonType(const ArrayOfVector& argIn)
{
    if (argIn.size() == 2) {
        return getColonCommonBasicType(argIn[0], argIn[1]);
    } else {
        // numArgs == 3
        NelsonType commonType2 = getColonCommonBasicType(argIn[1], argIn[2]);
        return getColonCommonType(getColonCommonBasicType(argIn[0], argIn[1]), commonType2);
    }
}
//=============================================================================
NelsonType
getColonCommonType(NelsonType typeA, NelsonType typeB)
{
    if (typeA == typeB) {
        return typeA;
    }
    if (typeA == NLS_DOUBLE || typeA == NLS_DCOMPLEX) {
        return typeB;
    }
    if (typeB == NLS_DOUBLE || typeA == NLS_DCOMPLEX) {
        return typeA;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
NelsonType
getColonCommonBasicType(const ArrayOf& A, const ArrayOf& B)
{
    NelsonType commonType = NLS_UNKNOWN;
    NelsonType classA = (A.dataClass() == NLS_LOGICAL) ? NLS_DOUBLE : A.dataClass();
    NelsonType classB = (B.dataClass() == NLS_LOGICAL) ? NLS_DOUBLE : B.dataClass();

    if (IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB)) {
        bool isIntegerA
            = IS_INTEGER_TYPE(classA) || (A.isDoubleClass() && (A.isEmpty() || A.isIntegerValue()));
        bool isIntegerB
            = IS_INTEGER_TYPE(classB) || (B.isDoubleClass() && (B.isEmpty() || B.isIntegerValue()));
        bool isSupportedMixedInteger = false;
        if (IS_INTEGER_TYPE(classA) && IS_INTEGER_TYPE(classB)) {
            isSupportedMixedInteger = (classA == classB);
        } else {
            isSupportedMixedInteger = (isIntegerA && isIntegerB);
        }

        if (!isSupportedMixedInteger && ((IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB)))) {
            Error(L"Nelson:colon:mixedNonDoubleOperands",
                L"Colon operands must be all the same type, or mixed with real double scalar.");
        }
    }
    return getColonCommonType(classA, classB);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
