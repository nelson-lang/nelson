//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "VertCat.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Operators.hpp"
#include "Evaluator.hpp"
#include "ClassdefConcatenationHelpers.hpp"
#include "ConcatenationConversionErrors.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "FindCommonConcatenateType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
stringVertCat(const ArrayOfVector& v, NelsonType commonType);
static ArrayOf
cellVertCat(const ArrayOfVector& v, NelsonType commonType);
//=============================================================================
ArrayOf
Evaluator::vertcatOperator(const ArrayOfVector& v)
{
    switch (v.size()) {
    case 0: {
        return ArrayOf::empty();
    }
    case 1: {
        return v[0];
    }
    default: {
        ClassdefConcatenationHelpers::rejectScalarClassConcatenation(v);
        ArrayOfVector handleArgs;
        if (ClassdefConcatenationHelpers::prepareHandleArguments(
                this, v, VERTCAT_OPERATOR_STR, handleArgs)) {
            return vertcatOperator(handleArgs);
        }
        ArrayOfVector missingArgs;
        if (convertMissingOperands(v, missingArgs)) {
            return vertcatOperator(missingArgs);
        }
        ArrayOfVector loweredEnumArgs;
        if (ClassdefConcatenationHelpers::lowerBuiltinBaseEnumerationOperands(
                this, v, loweredEnumArgs)) {
            return vertcatOperator(loweredEnumArgs);
        }
        std::string commonTypeName = NLS_UNKNOWN_STR;
        NelsonType commonType = NLS_UNKNOWN;
        bool isSparse = false;
        bool isComplex = false;

        ArrayOf res;
        ArrayOfVector catArgs(v);
        if (FindCommonConcatenateType(v, commonType, isSparse, isComplex, commonTypeName)) {
            bool overloadWasFound = false;
            res = callOperatorOverload(
                this, v, VERTCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            }
        }
        ArrayOfVector convertedClassdefArgs;
        std::string convertedClassdefTypeName;
        if (ClassdefConcatenationHelpers::prepareArguments(
                this, v, convertedClassdefTypeName, convertedClassdefArgs)) {
            catArgs = convertedClassdefArgs;
            commonTypeName = convertedClassdefTypeName;
            commonType = NLS_CLASSDEF_ARRAY;
            isSparse = false;
            isComplex = false;
        }
        if (isSparse) {
            checkSparseConcatenationOperands(v, commonType);
        }
        if (isSparse
            && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_SINGLE
                && commonType != NLS_SCOMPLEX && commonType != NLS_LOGICAL
                && commonType != NLS_STRING_ARRAY)) {
            raiseUnsupportedSparseConcatenation(v, commonType);
            Error("Nelson:UnableToConvert", "Attempt to convert to unimplemented sparse type");
        }
        prepareMissingConcatenation(catArgs, commonType);
        if (commonType == NLS_STRUCT_ARRAY) {
            prepareStructConcatenationOperands(catArgs);
        }
        if (!isSparse) {
            isComplex = concatenationIsComplex(catArgs, commonType);
        }
        checkComplexIntegerConcatenation(catArgs, commonType);
        switch (commonType) {
        case NLS_LOGICAL: {
            if (isSparse) {
                return VertCat(catArgs, commonType);
            }
            return VertCat(catArgs, commonType);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX: {
            if (isSparse) {
                return VertCat(catArgs, commonType);
            }
            return VertCat(catArgs, isComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
        } break;
        case NLS_SINGLE:
        case NLS_SCOMPLEX: {
            if (isSparse) {
                return VertCat(catArgs, commonType);
            }
            return VertCat(catArgs, isComplex ? NLS_SCOMPLEX : NLS_SINGLE);
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64:
        case NLS_STRUCT_ARRAY:
        case NLS_MISSING_ARRAY:
        case NLS_CLASSDEF_ARRAY:
        case NLS_CLASS_ARRAY:
        case NLS_GO_HANDLE: {
            return VertCat(catArgs, commonType);
        } break;
        case NLS_CHAR: {
            checkCharConcatenationOperands(catArgs);
            return VertCat(catArgs, commonType);
        } break;
        case NLS_CELL_ARRAY: {
            return cellVertCat(catArgs, commonType);
        } break;
        case NLS_STRING_ARRAY: {
            return stringVertCat(catArgs, commonType);
        } break;
        case NLS_FUNCTION_HANDLE: {
            return concatenateFunctionHandles(catArgs, 0);
        } break;
        case NLS_HANDLE: {
            bool overloadWasFound = false;
            res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, catArgs,
                VERTCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            } else {
                OverloadRequired(VERTCAT_OPERATOR_STR);
            }
        } break;
        case NLS_UNKNOWN:
        default: {
            std::string msg = formatI18nMessage(
                Nelson::resolveMessageTemplate("Nelson:interpreter:operatorSupportedOperandsType",
                    "Operator '%s' is not supported for operands of type '%s'."),
                VERTCAT_OPERATOR_STR, ClassToString(commonType));
            Error("Nelson:UndefinedFunction", msg);
        } break;
        }
    } break;
    }
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
stringVertCat(const ArrayOfVector& v, NelsonType commonType)
{
    checkStringConcatenationOperands(v);
    ArrayOfVector _argIn(v);
    for (ompIndexType k = 0; k < (ompIndexType)v.size(); ++k) {
        if (!v[k].isStringArray()) {
            bool needOverload = false;
            _argIn[k] = ArrayOf::toStringArray(v[k], needOverload);
            if (needOverload) {
                Error(L"Nelson:interpreter:cannotPromoteToString",
                    L"Cannot promote to string array.");
            }
        } else {
            _argIn[k] = v[k];
        }
    }
    return VertCat(_argIn, commonType);
}
//=============================================================================
ArrayOf
cellVertCat(const ArrayOfVector& v, NelsonType commonType)
{
    ArrayOfVector _argIn;
    _argIn.reserve(v.size());
    for (auto k : v) {
        if (k.isEmpty()) {
            _argIn.push_back(k);
        } else {
            _argIn.push_back(ArrayOf::cellArrayFrom(k));
        }
    }
    return VertCat(_argIn, commonType);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
