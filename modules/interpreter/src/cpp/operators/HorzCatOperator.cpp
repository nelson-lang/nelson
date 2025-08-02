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
#include "HorzCat.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Operators.hpp"
#include "Evaluator.hpp"
#include "FindCommonConcatenateType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
stringHorzCat(const ArrayOfVector& v, NelsonType commonType);
static ArrayOf
cellHorzCat(const ArrayOfVector& v, NelsonType commonType);
//=============================================================================
ArrayOf
Evaluator::horzcatOperator(const ArrayOfVector& v)
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
        std::string commonTypeName = NLS_UNKNOWN_STR;
        NelsonType commonType = NLS_UNKNOWN;
        bool isSparse = false;
        bool isComplex = false;

        ArrayOf res;
        if (FindCommonConcatenateType(v, commonType, isSparse, isComplex, commonTypeName)) {
            bool overloadWasFound = false;
            res = callOverloadedFunction(this,
                NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), v,
                HORZCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
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
                bool overloadWasFound = false;
                res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, v, HORZCAT_OPERATOR_STR,
                    NLS_SPARSE_LOGICAL_STR, commonType, overloadWasFound);
                if (overloadWasFound) {
                    return res;
                } else {
                    OverloadRequired(HORZCAT_OPERATOR_STR);
                }
            }
            return HorzCat(v, commonType);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX: {
            if (isSparse) {
                bool overloadWasFound = false;
                res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, v, HORZCAT_OPERATOR_STR,
                    NLS_SPARSE_DOUBLE_STR, commonType, overloadWasFound);
                if (overloadWasFound) {
                    return res;
                } else {
                    OverloadRequired(HORZCAT_OPERATOR_STR);
                }
            }
            return HorzCat(v, isComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
        } break;
        case NLS_SINGLE:
        case NLS_SCOMPLEX: {
            return HorzCat(v, isComplex ? NLS_SCOMPLEX : NLS_SINGLE);
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
        case NLS_GO_HANDLE:
        case NLS_CLASS_ARRAY: {
            return HorzCat(v, commonType);
        } break;
        case NLS_CELL_ARRAY: {
            return cellHorzCat(v, commonType);
        } break;
        case NLS_CHAR: {
            if (isComplex) {
                Error(_("Complex values cannot be converted to chars."));
            }
            return HorzCat(v, commonType);
        } break;
        case NLS_STRING_ARRAY: {
            return stringHorzCat(v, commonType);
        } break;
        case NLS_FUNCTION_HANDLE: {
            std::string msg = _(
                "Nonscalar arrays of function handles are not allowed; use cell arrays instead.");
            std::string id = "Nelson:err_non_scalar_function_handles";
            Error(msg, id);
        } break;
        case NLS_HANDLE: {
            bool overloadWasFound = false;
            res = callOverloadedFunction(this, NLS_OVERLOAD_OBJECT_TYPES_ONLY, v,
                HORZCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            } else {
                OverloadRequired(HORZCAT_OPERATOR_STR);
            }
        } break;
        case NLS_UNKNOWN:
        default: {
            std::string msg
                = fmt::sprintf(_("Operator '%s' is not supported for operands of type '%s'."),
                    HORZCAT_OPERATOR_STR, ClassToString(commonType));
            Error(msg, "Nelson:UndefinedFunction");
        } break;
        }
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
stringHorzCat(const ArrayOfVector& v, NelsonType commonType)
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
    return HorzCat(_argIn, commonType);
}
//=============================================================================
ArrayOf
cellHorzCat(const ArrayOfVector& v, NelsonType commonType)
{
    ArrayOfVector _argIn;
    _argIn.reserve(v.size());
    for (auto k : v) {
        if (k.isEmpty()) {
            _argIn.push_back(k);
        } else {
            _argIn.push_back(ArrayOf::toCell(k));
        }
    }
    return HorzCat(_argIn, commonType);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
