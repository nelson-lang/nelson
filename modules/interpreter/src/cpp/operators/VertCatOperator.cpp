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
#include "i18n.hpp"
#include "Operators.hpp"
#include "Evaluator.hpp"
#include "FindCommonConcatenateType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "OverloadRequired.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
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
                VERTCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            }
        }
        if (isSparse
            && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX
                && commonType != NLS_LOGICAL)) {
            raiseError2(_E("nelson:runtime:sparseNotImplemented"));
        }
        switch (commonType) {
        case NLS_LOGICAL: {
            if (isSparse) {
                bool overloadWasFound = false;
                res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, v, VERTCAT_OPERATOR_STR,
                    NLS_SPARSE_LOGICAL_STR, commonType, overloadWasFound);
                if (overloadWasFound) {
                    return res;
                } else {
                    OverloadRequired(VERTCAT_OPERATOR_STR);
                }
            }
            return VertCat(v, commonType);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX: {
            if (isSparse) {
                bool overloadWasFound = false;
                res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, v, VERTCAT_OPERATOR_STR,
                    NLS_SPARSE_DOUBLE_STR, commonType, overloadWasFound);
                if (overloadWasFound) {
                    return res;
                } else {
                    OverloadRequired(VERTCAT_OPERATOR_STR);
                }
            }
            return VertCat(v, isComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
        } break;
        case NLS_SINGLE:
        case NLS_SCOMPLEX: {
            return VertCat(v, isComplex ? NLS_SCOMPLEX : NLS_SINGLE);
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
        case NLS_CLASS_ARRAY:
        case NLS_GO_HANDLE: {
            return VertCat(v, commonType);
        } break;
        case NLS_CHAR: {
            if (isComplex) {
                raiseError(L"Nelson:interpreter:ERROR_COMPLEX_VALUES_CANNOT_BE_CONVERTED_TO_CHARS",
                    ERROR_COMPLEX_VALUES_CANNOT_BE_CONVERTED_TO_CHARS);
            }
            return VertCat(v, commonType);
        } break;
        case NLS_CELL_ARRAY: {
            return cellVertCat(v, commonType);
        } break;
        case NLS_STRING_ARRAY: {
            return stringVertCat(v, commonType);
        } break;
        case NLS_FUNCTION_HANDLE: {
            raiseError(L"Nelson:interpreter:ERROR_NONSCALAR_ARRAYS_OF_FUNCTION_HANDLES_NOT_ALLOWED",
                ERROR_NONSCALAR_ARRAYS_OF_FUNCTION_HANDLES_NOT_ALLOWED);
        } break;
        case NLS_HANDLE: {
            bool overloadWasFound = false;
            res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, v, VERTCAT_OPERATOR_STR,
                commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            } else {
                OverloadRequired(VERTCAT_OPERATOR_STR);
            }
        } break;
        case NLS_UNKNOWN:
        default: {
            raiseError(L"Nelson:interpreter:ERROR_OPERATOR_NOT_SUPPORTED",
                ERROR_OPERATOR_NOT_SUPPORTED, utf8_to_wstring(VERTCAT_OPERATOR_STR),
                utf8_to_wstring(ClassToString(commonType)));
        } break;
        }
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
stringVertCat(const ArrayOfVector& v, NelsonType commonType)
{
    ArrayOfVector _argIn(v);
    for (ompIndexType k = 0; k < (ompIndexType)v.size(); ++k) {
        if (!v[k].isStringArray()) {
            bool needOverload = false;
            _argIn[k] = ArrayOf::toStringArray(v[k], needOverload);
            if (needOverload) {
                raiseError(L"Nelson:interpreter:ERROR_CANNOT_PROMOTE_TO_STRING_ARRAY",
                    ERROR_CANNOT_PROMOTE_TO_STRING_ARRAY);
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
            _argIn.push_back(ArrayOf::toCell(k));
        }
    }
    return VertCat(_argIn, commonType);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
