//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FindCommonColonType.hpp"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassToString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
getColonCommonBasicType(const ArrayOf& A, const ArrayOf& B);
static NelsonType
getColonCommonType(NelsonType typeA, NelsonType typeB);
//=============================================================================
bool
findColonCommonType(
    const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse, std::string& typeName)
{
    isSparse = false;
    commonType = NLS_UNKNOWN;

    const size_t numArgs = argIn.size();
    if (numArgs == 0) {
        return false; // Nothing to process, return false
    }

    isSparse = argIn[0].isSparse();
    for (int i = 1; i < numArgs; ++i) {
        isSparse = isSparse || argIn[i].isSparse();
    }

    bool isMixed = false;
    commonType = argIn[0].getDataClass();
    for (int i = 1; i < numArgs; ++i) {
        NelsonType argType = argIn[i].getDataClass();
        if (commonType != argType) {
            isMixed = true;
            break;
        }
    }

    if (!isMixed) {
        typeName = ClassToString(commonType);
        return true;
    }

    bool hasReference = false;
    for (int i = 0; i < numArgs; ++i) {
        NelsonType argType = argIn[i].getDataClass();
        if (argType > NLS_CHAR) {
            hasReference = true;
            if (argType == NLS_STRING_ARRAY || argType == NLS_CELL_ARRAY) {
                commonType = argType;
                typeName
                    = (argType == NLS_STRING_ARRAY) ? NLS_STRING_ARRAY_STR : NLS_CELL_ARRAY_STR;
                return true;
            } else if (argType == NLS_CLASS_ARRAY) {
                commonType = NLS_CLASS_ARRAY;
                typeName = argIn[i].getClassType();
                return true;
            } else if (argType == NLS_STRUCT_ARRAY) {
                commonType = NLS_STRUCT_ARRAY;
                typeName = NLS_STRUCT_ARRAY_STR;
                return true;
            } else if (argType == NLS_HANDLE) {
                commonType = NLS_HANDLE;
                typeName = argIn[i].getHandleCategory();
                return true;
            } else if (argType == NLS_GO_HANDLE) {
                commonType = NLS_GO_HANDLE;
                typeName = NLS_GO_HANDLE_STR;
                return true;
            }
        }
    }

    if (!hasReference) {
        commonType = getColonCommonBasicType(argIn[0], argIn[1]);
        if (numArgs > 2) {
            NelsonType commonType2 = getColonCommonBasicType(argIn[1], argIn[2]);
            commonType = getColonCommonType(commonType, commonType2);
        }
    }

    typeName = ClassToString(commonType);
    if (isSparse) {
        typeName = NLS_SPARSE_STR + typeName;
    }

    return commonType != NLS_UNKNOWN;
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
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();

    if (classA == NLS_CHAR || classB == NLS_CHAR) {
        Error(_W("For colon operator with char operands, first and last operands must be char."),
            L"Nelson:colon:mixedCharOperands");
    }

    if (IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB)) {
        bool isIntegerA = IS_INTEGER_TYPE(classA) || (A.isDoubleClass() && A.isIntegerValue());
        bool isIntegerB = IS_INTEGER_TYPE(classB) || (B.isDoubleClass() && B.isIntegerValue());
        bool isSupportedMixedInteger = false;
        if (IS_INTEGER_TYPE(classA) && IS_INTEGER_TYPE(classB)) {
            isSupportedMixedInteger = (classA == classB);
        } else {
            isSupportedMixedInteger = (isIntegerA && isIntegerB);
        }

        if (!isSupportedMixedInteger && ((IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB)))) {
            Error(_W("Colon operands must be all the same type, or mixed with real double "
                     "scalar."),
                L"Nelson:colon:mixedNonDoubleOperands");
        }
    }
    return getColonCommonType(classA, classB);
}
//=============================================================================
}
