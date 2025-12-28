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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "Addition.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "FindCommonType.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::plusOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    // Optimization: Evaluate expressions directly into args to avoid intermediate copies
    ArrayOfVector args;
    args.reserve(2);
    args.push_back(expression(t->down));
    args.push_back(expression(t->down->right));
    ArrayOf retval = this->plusOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::plusOperator(const ArrayOfVector& args)
{
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    ArrayOf res;
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(this,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args,
            PLUS_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_LOGICAL)) {
        Error(_("Attempt to convert to unimplemented sparse type"), "Nelson:UnableToConvert");
    }

    if (isComplex && (commonType == NLS_DOUBLE)) {
        commonType = NLS_DCOMPLEX;
    }
    if (isComplex && (commonType == NLS_SINGLE)) {
        commonType = NLS_SCOMPLEX;
    }

    ArrayOf A;
    ArrayOf B;
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

        if (A.getDataClass() != B.getDataClass()) {
            if (A.isIntegerType()) {
                bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
                if (!isCompatible) {
                    Error(_W(
                        "Integers can only be combined with integers of the same class, or scalar "
                        "doubles."));
                }
                A.promoteType(commonType);
                B.promoteType(commonType);
            } else if (B.isIntegerType()) {
                bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
                if (!isCompatible) {
                    Error(_W(
                        "Integers can only be combined with integers of the same class, or scalar "
                        "doubles."));
                }
                A.promoteType(commonType);
                B.promoteType(commonType);
            } else {
                if (commonType <= NLS_CHAR) {
                    if (A.getDataClass() <= NLS_CHAR) {
                        A.promoteType(commonType);
                    }
                    if (B.getDataClass() <= NLS_CHAR) {
                        B.promoteType(commonType);
                    }
                }
            }
        }

        if (isSparse) {
            A.makeSparse();
            B.makeSparse();
        }
    }

    switch (commonType) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        if (isSparse) {
            ArrayOfVector params;
            params << A;
            params << B;
            bool overloadWasFound = false;
            res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, params, PLUS_OPERATOR_STR,
                NLS_SPARSE_DOUBLE_STR, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            } else {
                OverloadRequired(PLUS_OPERATOR_STR);
            }
        }
        return Addition(A, B);
    } break;
    case NLS_LOGICAL: {
        if (isSparse) {
            ArrayOfVector params;
            params << A;
            params << B;
            bool overloadWasFound = false;
            res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, params, PLUS_OPERATOR_STR,
                NLS_SPARSE_LOGICAL_STR, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            } else {
                OverloadRequired(PLUS_OPERATOR_STR);
            }
        }
        A.promoteType(NLS_DOUBLE);
        B.promoteType(NLS_DOUBLE);
        return Addition(A, B);
    } break;
    case NLS_STRING_ARRAY:
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
        return Addition(A, B);
    } break;
    case NLS_STRUCT_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE: {
        ArrayOfVector params;
        params << A;
        params << B;
        bool overloadWasFound = false;
        res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, params, PLUS_OPERATOR_STR,
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        } else {
            OverloadRequired(PLUS_OPERATOR_STR);
        }
    } break;

    case NLS_UNKNOWN:
    default: {
        std::string msg
            = fmt::sprintf(_("Operator '%s' is not supported for operands of type '%s'."), "+",
                ClassToString(commonType));
        Error(msg, "Nelson:UndefinedFunction");
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
