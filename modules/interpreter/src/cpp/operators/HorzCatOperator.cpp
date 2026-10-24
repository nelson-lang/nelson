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
#include "HorzCat.hpp"
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
stringHorzCat(const ArrayOfVector& v, NelsonType commonType);
static ArrayOf
cellHorzCat(const ArrayOfVector& v, NelsonType commonType);
static bool
canFastScalarRowCat(const ArrayOfVector& v, NelsonType& type);
static ArrayOf
fastScalarRowCat(const ArrayOfVector& v, NelsonType type);
//=============================================================================
ArrayOf
Evaluator::horzcatOperator(const ArrayOfVector& v)
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
        NelsonType fastType = NLS_UNKNOWN;
        if (canFastScalarRowCat(v, fastType)) {
            return fastScalarRowCat(v, fastType);
        }
        ArrayOfVector handleArgs;
        if (ClassdefConcatenationHelpers::prepareHandleArguments(
                this, v, HORZCAT_OPERATOR_STR, handleArgs)) {
            return horzcatOperator(handleArgs);
        }
        ArrayOfVector missingArgs;
        if (convertMissingOperands(v, missingArgs)) {
            return horzcatOperator(missingArgs);
        }
        ArrayOfVector loweredEnumArgs;
        if (ClassdefConcatenationHelpers::lowerBuiltinBaseEnumerationOperands(
                this, v, loweredEnumArgs)) {
            return horzcatOperator(loweredEnumArgs);
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
                this, v, HORZCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
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
                return HorzCat(catArgs, commonType);
            }
            return HorzCat(catArgs, commonType);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX: {
            if (isSparse) {
                return HorzCat(catArgs, commonType);
            }
            return HorzCat(catArgs, isComplex ? NLS_DCOMPLEX : NLS_DOUBLE);
        } break;
        case NLS_SINGLE:
        case NLS_SCOMPLEX: {
            if (isSparse) {
                return HorzCat(catArgs, commonType);
            }
            return HorzCat(catArgs, isComplex ? NLS_SCOMPLEX : NLS_SINGLE);
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
        case NLS_GO_HANDLE:
        case NLS_CLASSDEF_ARRAY:
        case NLS_CLASS_ARRAY: {
            return HorzCat(catArgs, commonType);
        } break;
        case NLS_CELL_ARRAY: {
            return cellHorzCat(catArgs, commonType);
        } break;
        case NLS_CHAR: {
            checkCharConcatenationOperands(catArgs);
            return HorzCat(catArgs, commonType);
        } break;
        case NLS_STRING_ARRAY: {
            return stringHorzCat(catArgs, commonType);
        } break;
        case NLS_FUNCTION_HANDLE: {
            return concatenateFunctionHandles(catArgs, 1);
        } break;
        case NLS_HANDLE: {
            bool overloadWasFound = false;
            res = callOverloadedFunction(this, NLS_OVERLOAD_OBJECT_TYPES_ONLY, catArgs,
                HORZCAT_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
            if (overloadWasFound) {
                return res;
            } else {
                OverloadRequired(HORZCAT_OPERATOR_STR);
            }
        } break;
        case NLS_UNKNOWN:
        default: {
            std::string msg = formatI18nMessage(
                Nelson::resolveMessageTemplate("Nelson:interpreter:operatorSupportedOperandsType",
                    "Operator '%s' is not supported for operands of type '%s'."),
                HORZCAT_OPERATOR_STR, ClassToString(commonType));
            Error("Nelson:UndefinedFunction", msg);
        } break;
        }
    } break;
    }
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
stringHorzCat(const ArrayOfVector& v, NelsonType commonType)
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
            _argIn.push_back(ArrayOf::cellArrayFrom(k));
        }
    }
    return HorzCat(_argIn, commonType);
}
//=============================================================================
bool
canFastScalarRowCat(const ArrayOfVector& v, NelsonType& type)
{
    if (v.empty()) {
        return false;
    }
    type = v[0].dataClass();
    switch (type) {
    case NLS_LOGICAL:
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
        break;
    default:
        return false;
    }
    for (const auto& value : v) {
        if (value.dataClass() != type || value.isSparse() || !value.isScalar()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <typename T>
ArrayOf
fastScalarRowCatAs(const ArrayOfVector& v, NelsonType type)
{
    indexType count = static_cast<indexType>(v.size());
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(type, count, stringVector(), false));
    for (indexType k = 0; k < count; ++k) {
        data[k] = *static_cast<const T*>(v[static_cast<size_t>(k)].dataPointer());
    }
    return ArrayOf(type, Dimensions(1, count), data);
}
//=============================================================================
ArrayOf
fastScalarRowCat(const ArrayOfVector& v, NelsonType type)
{
    switch (type) {
    case NLS_LOGICAL:
        return fastScalarRowCatAs<logical>(v, type);
    case NLS_DOUBLE:
        return fastScalarRowCatAs<double>(v, type);
    case NLS_SINGLE:
        return fastScalarRowCatAs<single>(v, type);
    case NLS_INT8:
        return fastScalarRowCatAs<int8>(v, type);
    case NLS_UINT8:
        return fastScalarRowCatAs<uint8>(v, type);
    case NLS_INT16:
        return fastScalarRowCatAs<int16>(v, type);
    case NLS_UINT16:
        return fastScalarRowCatAs<uint16>(v, type);
    case NLS_INT32:
        return fastScalarRowCatAs<int32>(v, type);
    case NLS_UINT32:
        return fastScalarRowCatAs<uint32>(v, type);
    case NLS_INT64:
        return fastScalarRowCatAs<int64>(v, type);
    case NLS_UINT64:
        return fastScalarRowCatAs<uint64>(v, type);
    default:
        return ArrayOf::empty();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
