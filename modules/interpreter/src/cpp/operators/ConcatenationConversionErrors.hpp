//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringConcatenationConversion.hpp"
#include "StructConcatenationFields.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// These identifiers already carry another template in the identifier-keyed
// catalog, so the messages are resolved text-keyed and raised with RaiseError.
//=============================================================================
inline void
raiseUnableToConvert(const std::string& from, const std::string& to, const std::string& reason)
{
    RaiseError(formatI18nMessage(_("The following error occurred converting from %s to %s:\n%s"),
                   from, to, reason),
        "Nelson:UnableToConvert");
}
//=============================================================================
// A struct concatenation drops the empty operands of any class; the first
// other operand that is not a struct cannot be converted, even before the
// field names are compared. Every struct but a 0x0 one without fields, empty
// or not, must then have the fields of the first one, which it takes in its
// order. The kernel matches two nonempty structs itself but drops an empty
// operand, so the fields are matched here only when an empty struct takes part.
inline void
prepareStructConcatenationOperands(ArrayOfVector& args)
{
    bool haveEmptyStruct = false;
    for (const ArrayOf& arg : args) {
        if (!arg.isStruct() && !arg.isEmpty()) {
            std::string from = ClassName(arg);
            raiseUnableToConvert(from, NLS_STRUCT_ARRAY_STR,
                formatI18nMessage(_("Conversion to struct from %s is not possible."), from));
        }
        haveEmptyStruct = haveEmptyStruct || (arg.isStruct() && arg.isEmpty());
    }
    if (!haveEmptyStruct) {
        return;
    }
    bool haveFirst = false;
    stringVector fieldnames;
    for (ArrayOf& arg : args) {
        if (!arg.isStruct() || (arg.isEmpty(true) && arg.fieldNames().empty())) {
            continue;
        }
        if (!haveFirst) {
            haveFirst = true;
            fieldnames = arg.fieldNames();
        } else {
            arg = matchStructFields(arg, fieldnames);
        }
    }
}
//=============================================================================
// A char result converts every operand: a logical one, even empty, cannot
// become a char, nor can a nonempty complex one; the first one names it.
inline void
checkCharConcatenationOperands(const ArrayOfVector& args)
{
    for (const ArrayOf& arg : args) {
        if (arg.isLogical()) {
            raiseUnableToConvert(NLS_LOGICAL_STR, NLS_CHAR_STR,
                _("Conversion to char from logical is not possible."));
        }
        if (arg.isComplex() && !arg.isEmpty()) {
            raiseUnableToConvert(ClassToString(arg.dataClass()), NLS_CHAR_STR,
                _("Complex values cannot be converted to chars"));
        }
    }
}
//=============================================================================
// A floating result is complex next to a nonempty complex operand, or next to
// an empty complex one of the result class itself ([complex([]), 1] is complex,
// [complex([]), single(1)] is not).
inline bool
concatenationIsComplex(const ArrayOfVector& args, NelsonType commonType)
{
    for (const ArrayOf& arg : args) {
        if (!arg.isComplex()) {
            continue;
        }
        if (!arg.isEmpty()) {
            return true;
        }
        NelsonType realType = arg.dataClass() == NLS_SCOMPLEX ? NLS_SINGLE : NLS_DOUBLE;
        if (realType == commonType) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Why the first operand cannot become the result class, or an empty string
// when it can: a function handle or a struct only from its own class or an
// empty (a sparse empty for a struct), an integer or a char never from sparse.
inline std::string
sparseConcatenationConversionFailure(
    const ArrayOf& first, NelsonType commonType, const std::string& from)
{
    switch (commonType) {
    case NLS_FUNCTION_HANDLE: {
        if (first.isFunctionHandle() || (!first.isSparse() && first.isEmpty())) {
            return {};
        }
        return _("Too many output arguments.");
    }
    case NLS_STRUCT_ARRAY: {
        if (first.isStruct() || first.isEmpty()) {
            return {};
        }
        return formatI18nMessage(_("Conversion to struct from %s is not possible."), from);
    }
    case NLS_CHAR: {
        if (!first.isSparse()) {
            return {};
        }
        if (first.isLogical()) {
            return _("Conversion to char from logical is not possible.");
        }
        if (first.isComplex()) {
            return _("Complex values cannot be converted to chars");
        }
        return _("Attempt to convert to unimplemented sparse type");
    }
    case NLS_CELL_ARRAY: {
        return {};
    }
    default: {
        if (!first.isSparse()) {
            return {};
        }
        return _("Attempt to convert to unimplemented sparse type");
    }
    }
}
//=============================================================================
// A function handle, a struct, a cell, an integer or a char has no sparse
// form. The first operand decides the error (for a struct, after the leading
// non-sparse 2-D empties that a struct concatenation drops): one that cannot
// become the result class names that conversion, then one still N-D once
// converted cannot become sparse (a non-cell becomes a 1x1 cell), otherwise
// the result would be a sparse array of that class. Other classes return.
inline void
raiseUnsupportedSparseConcatenation(const ArrayOfVector& args, NelsonType commonType)
{
    if (commonType != NLS_FUNCTION_HANDLE && commonType != NLS_STRUCT_ARRAY
        && commonType != NLS_CELL_ARRAY && commonType != NLS_CHAR && !IS_INTEGER_TYPE(commonType)) {
        return;
    }
    size_t k = 0;
    if (commonType == NLS_STRUCT_ARRAY) {
        while (k + 1 < args.size() && !args[k].isStruct() && !args[k].isSparse()
            && args[k].isEmpty() && args[k].is2D()) {
            ++k;
        }
    }
    const ArrayOf& first = args[k];
    std::string from = first.isSparse() ? ClassToString(first.dataClass()) : ClassName(first);
    std::string failure = sparseConcatenationConversionFailure(first, commonType, from);
    if (!failure.empty()) {
        raiseUnableToConvert(from, ClassToString(commonType), failure);
    }
    if (!first.is2D() && (commonType != NLS_CELL_ARRAY || first.isCell())) {
        RaiseError(_("Concatenation involves conversion to sparse of an array with more than 2 "
                     "dimensions."),
            "Nelson:catenate:dimensionMismatch");
    }
    Error("Nelson:unimplementedSparseType", "Attempt to convert to unimplemented sparse type");
}
//=============================================================================
// Missing operands take the class of the first operand that is not missing: a
// double or a single holds them as NaN (complex next to a complex operand), a
// string as <missing>; any other class raises. Sparse and object operands are
// left to the sparse check and to the overloads. True when args were converted.
inline bool
convertMissingOperands(const ArrayOfVector& args, ArrayOfVector& converted)
{
    const ArrayOf* first = nullptr;
    bool hasMissing = false;
    bool hasComplex = false;
    for (const ArrayOf& arg : args) {
        switch (arg.dataClass()) {
        case NLS_CLASS_ARRAY:
        case NLS_CLASSDEF_ARRAY:
        case NLS_HANDLE:
        case NLS_GO_HANDLE: {
            return false;
        }
        case NLS_MISSING_ARRAY: {
            hasMissing = true;
        } break;
        default: {
            if (arg.isSparse()) {
                return false;
            }
            hasComplex = hasComplex || arg.isComplex();
            if (first == nullptr) {
                first = &arg;
            }
        } break;
        }
    }
    if (!hasMissing || first == nullptr) {
        return false;
    }
    NelsonType target = NLS_STRING_ARRAY;
    switch (first->dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        target = hasComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        target = hasComplex ? NLS_SCOMPLEX : NLS_SINGLE;
    } break;
    case NLS_STRING_ARRAY: {
    } break;
    default: {
        RaiseError(formatI18nMessage(_("Conversion to %s from missing is not possible."),
                       ClassToString(first->dataClass())),
            "Nelson:invalidConversion");
    } break;
    }
    converted = args;
    for (ArrayOf& arg : converted) {
        if (arg.dataClass() != NLS_MISSING_ARRAY) {
            continue;
        }
        if (target == NLS_STRING_ARRAY) {
            bool needToOverload = false;
            arg = ArrayOf::toStringArray(arg, needToOverload);
        } else {
            arg.promoteType(target);
        }
    }
    return true;
}
//=============================================================================
// A sparse concatenation converts a missing operand to the class of the first
// operand that is not missing: only a double, a single or a string holds it.
// Into a sparse double, single or logical result the first N-D operand cannot
// become sparse; a missing one next to a leading sparse operand would be
// expanded as an N-D sparse array.
inline void
checkSparseConcatenationOperands(const ArrayOfVector& args, NelsonType commonType)
{
    const ArrayOf* first = nullptr;
    const ArrayOf* firstNd = nullptr;
    bool hasMissing = false;
    for (const ArrayOf& arg : args) {
        bool isMissing = arg.dataClass() == NLS_MISSING_ARRAY;
        hasMissing = hasMissing || isMissing;
        if (first == nullptr && !isMissing) {
            first = &arg;
        }
        if (firstNd == nullptr && !arg.is2D()) {
            firstNd = &arg;
        }
    }
    if (first == nullptr) {
        return;
    }
    if (hasMissing && commonType != NLS_STRING_ARRAY) {
        switch (first->dataClass()) {
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
        case NLS_SINGLE:
        case NLS_SCOMPLEX:
        case NLS_STRING_ARRAY:
        case NLS_HANDLE:
        case NLS_GO_HANDLE:
        case NLS_CLASS_ARRAY:
        case NLS_CLASSDEF_ARRAY: {
        } break;
        default: {
            RaiseError(formatI18nMessage(_("Conversion to %s from missing is not possible."),
                           ClassToString(first->dataClass())),
                "Nelson:invalidConversion");
        } break;
        }
    }
    if (firstNd == nullptr
        || (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_SINGLE
            && commonType != NLS_SCOMPLEX && commonType != NLS_LOGICAL
            && commonType != NLS_MISSING_ARRAY)) {
        return;
    }
    if (first->isSparse() && firstNd->dataClass() == NLS_MISSING_ARRAY) {
        RaiseError(
            _("N-D sparse output arrays are not supported."), "Nelson:repmat:NdSparseOutput");
    }
    RaiseError(_("Concatenation involves conversion to sparse of an array with more than 2 "
                 "dimensions."),
        "Nelson:catenate:dimensionMismatch");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
