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
#include "RelationOperatorsHelpers.hpp"
#include "OverloadRequired.hpp"
#include "OverloadHelpers.hpp"
#include "FindCommonType.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
relationalOperator(Evaluator* eval, const std::string& operatorName, const std::string& symbolName,
    bool compareAlsoImagPart, const ArrayOfVector& args,
    ArrayOf (*relationalOperator)(const ArrayOf& A, const ArrayOf& B, bool& needToOverload))
{
    const std::string functionName = operatorName;
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    ArrayOf res;
    if (FindCommonTypeRelationalOperators(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args, functionName,
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }

    bool needToOverload = false;
    if (commonType == NLS_STRING_ARRAY || (commonType <= NLS_CHAR && !isSparse)) {
        NelsonType _commonType = commonType;
        if (compareAlsoImagPart && commonType == NLS_DOUBLE && isComplex) {
            _commonType = NLS_DCOMPLEX;
        }
        if (compareAlsoImagPart && commonType == NLS_SINGLE && isComplex) {
            _commonType = NLS_SCOMPLEX;
        }

        ArrayOf A(args[0]);
        ArrayOf B(args[1]);
        if (commonType == NLS_STRING_ARRAY) {
            if (!A.isStringArray()) {
                A = ArrayOf::toStringArray(A, needToOverload);
            }
            if (!B.isStringArray()) {
                B = ArrayOf::toStringArray(B, needToOverload);
            }
            if (needToOverload) {
                Error(_W("Cannot promote to string array."));
            }
        } else {
            A.promoteType(_commonType);
            B.promoteType(_commonType);
        }
        res = relationalOperator(A, B, needToOverload);
        if (!needToOverload) {
            return res;
        }
    }

    switch (commonType) {
    case NLS_UNKNOWN:
    case NLS_FUNCTION_HANDLE: {
        std::string msg
            = fmt::sprintf(_("Operator '%s' is not supported for operands of type '%s'."),
                symbolName, ClassToString(commonType));
        Error(msg, "Nelson:UndefinedFunction");
    } break;
    }
    ArrayOfVector retval;
    bool overloadWasFound = false;
    retval = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, args, functionName,
        commonTypeName, commonType, overloadWasFound);
    if (!overloadWasFound) {
        OverloadRequired(functionName);
    }
    return retval[0];
}
//=============================================================================
}
//=============================================================================
