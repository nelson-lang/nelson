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
#include "Subtraction.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "OverloadRequired.hpp"
#include "FindCommonType.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::minusOperator(const ArrayOfVector& args)
{
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    ArrayOf res;
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOperatorOverload(
            this, args, MINUS_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    // A - B: subtract between a complex and a real operand without promoting.
    if (dotMinusComplexRealScaling(args[0], args[1], res)) {
        return res;
    }

    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_SINGLE && commonType != NLS_DCOMPLEX
            && commonType != NLS_SCOMPLEX && commonType != NLS_LOGICAL)) {
        Error("Nelson:UnableToConvert", "Attempt to convert to unimplemented sparse type");
    }

    if (isComplex && (commonType == NLS_DOUBLE)) {
        commonType = NLS_DCOMPLEX;
    }
    if (isComplex && (commonType == NLS_SINGLE)) {
        commonType = NLS_SCOMPLEX;
    }

    ArrayOf A;
    ArrayOf B;
    bool integerDoublePair = false;
    if (commonType == NLS_STRING_ARRAY) {
        bool needToOverload = false;
        A = ArrayOf::toStringArray(args[0], needToOverload);
        if (needToOverload) {
            OverloadRequired(PLUS_OPERATOR_STR);
        }
        B = ArrayOf::toStringArray(args[1], needToOverload);
        if (needToOverload) {
            OverloadRequired(PLUS_OPERATOR_STR);
        }
    } else {
        A = args[0];
        B = args[1];

        if (A.dataClass() != B.dataClass()) {
            if (A.isIntegerType()) {
                bool isCompatible = isScalarDoubleOrChar(B);
                if (!isCompatible) {
                    Error(L"Nelson:interpreter:integerClassMismatch",
                        L"Integers can only be combined with integers of the same class, or scalar "
                        L"doubles.");
                }
                // a real double scalar is combined exactly by the operator itself
                integerDoublePair = isRealDoubleScalar(B);
                if (!integerDoublePair) {
                    A.promoteType(commonType);
                    B.promoteType(commonType);
                }
            } else if (B.isIntegerType()) {
                bool isCompatible = isScalarDoubleOrChar(A);
                if (!isCompatible) {
                    Error(L"Nelson:interpreter:integerClassMismatch",
                        L"Integers can only be combined with integers of the same class, or scalar "
                        L"doubles.");
                }
                integerDoublePair = isRealDoubleScalar(A);
                if (!integerDoublePair) {
                    A.promoteType(commonType);
                    B.promoteType(commonType);
                }
            } else {
                if (commonType <= NLS_CHAR) {
                    if (A.dataClass() <= NLS_CHAR) {
                        A.promoteType(commonType);
                    }
                    if (B.dataClass() <= NLS_CHAR) {
                        B.promoteType(commonType);
                    }
                }
            }
        }
        if (commonType <= NLS_CHAR && !integerDoublePair) {
            if (A.dataClass() != commonType) {
                A.promoteType(commonType);
            }
            if (B.dataClass() != commonType) {
                B.promoteType(commonType);
            }
        }
    }

    switch (commonType) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        if (isSparse) {
            return Subtraction(A, B);
        }
        return Subtraction(A, B);
    } break;
    case NLS_LOGICAL: {
        if (isSparse) {
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            return Subtraction(A, B);
        }
        A.promoteType(NLS_DOUBLE);
        B.promoteType(NLS_DOUBLE);
        return Subtraction(A, B);
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_CHAR: {
        return Subtraction(A, B);
    } break;
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASSDEF_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE: {
        ArrayOfVector params;
        params << A;
        params << B;
        bool overloadWasFound = false;
        res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, params, MINUS_OPERATOR_STR,
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        } else {
            OverloadRequired(MINUS_OPERATOR_STR);
        }
    } break;

    case NLS_UNKNOWN:
    default: {
        std::string msg = formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:interpreter:operatorSupportedOperandsType",
                "Operator '%s' is not supported for operands of type '%s'."),
            "-", ClassToString(commonType));
        Error("Nelson:UndefinedFunction", msg);
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
