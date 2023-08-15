//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "VertCat.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Operators.hpp"
#include "Evaluator.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
integerVertCat(const ArrayOfVector& v, NelsonType commonType);
static ArrayOf
realVertCat(const ArrayOfVector& v, NelsonType destinationType, NelsonType complexType);
static ArrayOf
cellVertCat(const ArrayOfVector& v);
static ArrayOf
stringVertCat(const ArrayOfVector& v);
//=============================================================================
ArrayOf
Evaluator::vertcatOperator(const ArrayOfVector& v)
{
    ArrayOf res;
    switch (v.size()) {
    case 0: {
        res = ArrayOf::emptyConstructor();
    } break;
    case 1: {
        res = v[0];
    } break;
    default: {
        NelsonType commonType;
        bool isSparse;
        std::string commonTypeName;

        ArrayOf res;
        if (FindCommonConcatenateType(v, commonType, isSparse, commonTypeName)) {
            bool overloadWasFound = false;
            res = callOverloadedFunction(
                this, v, VERTCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            }
        }
        if (isSparse
            && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX
                && commonType != NLS_LOGICAL)) {
            Error(_("Attempt to convert to unimplemented sparse type"), "Nelson:UnableToConvert");
        }
        switch (commonType) {
        case NLS_LOGICAL: {
            if (isSparse) {
                FunctionDef* funcDef = nullptr;
                std::string overloadTypeName
                    = overloadFunctionName(commonTypeName, VERTCAT_OPERATOR_STR);
                getContext()->lookupFunction(overloadTypeName, funcDef);
                if (funcDef) {
                    return funcDef->evaluateFunction(this, v, 1)[0];
                } else {
                    OverloadRequired(VERTCAT_OPERATOR_STR);
                }
            }
            return integerVertCat(v, commonType);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX: {
            if (isSparse) {
                FunctionDef* funcDef = nullptr;
                std::string overloadTypeName
                    = overloadFunctionName(commonTypeName, VERTCAT_OPERATOR_STR);
                getContext()->lookupFunction(overloadTypeName, funcDef);
                if (funcDef) {
                    return funcDef->evaluateFunction(this, v, 1)[0];
                } else {
                    OverloadRequired(VERTCAT_OPERATOR_STR);
                }
            }
            return realVertCat(v, NLS_DOUBLE, NLS_DCOMPLEX);
        } break;
        case NLS_SINGLE:
        case NLS_SCOMPLEX: {
            return realVertCat(v, NLS_SINGLE, NLS_SCOMPLEX);
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64:
        case NLS_CHAR: {
            return integerVertCat(v, commonType);
        } break;
        case NLS_STRUCT_ARRAY: {
            return VertCat(v);
        } break;
        case NLS_CELL_ARRAY: {
            return cellVertCat(v);
        } break;
        case NLS_STRING_ARRAY: {
            return stringVertCat(v);
        } break;
        case NLS_GO_HANDLE: {
            return VertCat(v);
        } break;
        case NLS_FUNCTION_HANDLE: {
            std::string msg = _(
                "Nonscalar arrays of function handles are not allowed; use cell arrays instead.");
            std::string id = "Nelson:err_non_scalar_function_handles";
            Error(msg, id);
        } break;
        case NLS_CLASS_ARRAY: {
            return VertCat(v);
        } break;
        case NLS_HANDLE: {
            bool overloadWasFound = false;
            res = callOverloadedFunction(
                this, v, VERTCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (!overloadWasFound) {
                OverloadRequired(VERTCAT_OPERATOR_STR);
            }
        } break;
        case NLS_UNKNOWN:
        default: {
            std::string msg
                = fmt::sprintf(_("Operator 'vertcat' is not supported for operands of type '%s'."),
                    ClassToString(commonType));
            Error(msg, "Nelson:UndefinedFunction");
        } break;
        }
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
integerVertCat(const ArrayOfVector& v, NelsonType commonType)
{
    ArrayOfVector _argIn(v);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)v.size(); ++k) {
        ArrayOf element = v[k];
        element.promoteType(commonType);
        _argIn[k] = element;
    }
    return VertCat(_argIn);
}
//=============================================================================
ArrayOf
realVertCat(const ArrayOfVector& v, NelsonType destinationType, NelsonType complexType)
{
    bool haveComplex = false;
    for (auto arg : v) {
        if (!haveComplex && arg.isComplex()) {
            haveComplex = true;
            break;
        }
    }
    ArrayOfVector _argIn(v);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)v.size(); ++k) {
        ArrayOf element = v[k];
        element.promoteType(haveComplex ? complexType : destinationType);
        _argIn[k] = element;
    }
    return VertCat(_argIn);
}
//=============================================================================
ArrayOf
cellVertCat(const ArrayOfVector& v)
{
    ArrayOfVector _argIn(v);
    for (ompIndexType k = 0; k < (ompIndexType)v.size(); ++k) {
        if (v[k].isEmpty()) {
            _argIn[k] = v[k];
        } else {
            _argIn[k] = ArrayOf::toCell(v[k]);
        }
    }
    return VertCat(_argIn);
}
//=============================================================================
ArrayOf
stringVertCat(const ArrayOfVector& v)
{
    ArrayOfVector _argIn(v);
    for (ompIndexType k = 0; k < (ompIndexType)v.size(); ++k) {
        if (!v[k].isStringArray()) {
            bool needOverload = false;
            _argIn[k] = ArrayOf::toStringArray(v[k], needOverload);
            if (needOverload) {
                Error(_W("Cannot promote to string array."));
            }
        } else {
            _argIn[k] = v[k];
        }
    }
    return VertCat(_argIn);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
