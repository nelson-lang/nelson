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
#include <cwctype>
#include <limits>
#include <string>
#include "ArrayOf.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// class asked by a constant function (eps, realmax, intmax...): none, a name or 'like', p
struct ConstantClassRequest
{
    std::wstring className;
    bool isComplex = false;
    bool isSparse = false;
};
//=============================================================================
inline bool
isConstantIntegerClass(const std::wstring& name)
{
    return name == L"int8" || name == L"uint8" || name == L"int16" || name == L"uint16"
        || name == L"int32" || name == L"uint32" || name == L"int64" || name == L"uint64";
}
//=============================================================================
inline ConstantClassRequest
parseConstantClassArguments(const ArrayOfVector& argIn, bool integerClasses)
{
    ConstantClassRequest request;
    request.className = integerClasses ? L"int32" : L"double";
    if (argIn.empty()) {
        return request;
    }
    auto isText = [](const ArrayOf& a) {
        return a.isRowVectorCharacterArray() || a.isScalarStringArray();
    };
    auto isSupported = [integerClasses](const std::wstring& name) {
        return integerClasses ? isConstantIntegerClass(name)
                              : (name == L"double" || name == L"single");
    };
    if (argIn.size() == 1) {
        std::wstring name = isText(argIn[0]) ? argIn[0].asWideString() : std::wstring();
        if (!isSupported(name)) {
            if (integerClasses) {
                Error(L"Nelson:constants:integerClassExpected",
                    L"Class name must be an integer class such as 'int8' or 'uint64'.");
            }
            Error(L"Nelson:constants:floatClassExpected",
                L"Class name must be 'double' or 'single'.");
        }
        request.className = name;
        return request;
    }
    std::wstring option = isText(argIn[0]) ? argIn[0].asWideString() : std::wstring();
    for (auto& c : option) {
        c = towlower(c);
    }
    if (option != L"like") {
        Error(L"Nelson:constants:firstArgNotLike", L"With two inputs, the first one must be 'like'.");
    }
    const ArrayOf& prototype = argIn[1];
    std::wstring name;
    switch (prototype.dataClass()) {
    case NLS_INT8: name = L"int8"; break;
    case NLS_UINT8: name = L"uint8"; break;
    case NLS_INT16: name = L"int16"; break;
    case NLS_UINT16: name = L"uint16"; break;
    case NLS_INT32: name = L"int32"; break;
    case NLS_UINT32: name = L"uint32"; break;
    case NLS_INT64: name = L"int64"; break;
    case NLS_UINT64: name = L"uint64"; break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: name = L"single"; break;
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: name = L"double"; break;
    default: break;
    }
    if (!isSupported(name)) {
        if (integerClasses) {
            Error(L"Nelson:constants:integerPrototypeExpected",
                L"The value after 'like' must be an integer array.");
        }
        Error(L"Nelson:constants:floatPrototypeExpected",
            L"The value after 'like' must be a double or single array.");
    }
    request.className = name;
    request.isComplex = prototype.isComplex();
    request.isSparse = prototype.isSparse();
    return request;
}
//=============================================================================
// largest or smallest value of an integer class
inline ArrayOf
integerLimitScalar(const std::wstring& name, bool largest)
{
#define NLS_INTEGER_LIMIT(NAME, TYPE)                                                              \
    if (name == L## #TYPE) {                                                                       \
        return ArrayOf::NAME##Scalar(                                                              \
            largest ? std::numeric_limits<TYPE>::max() : std::numeric_limits<TYPE>::min());       \
    }
    NLS_INTEGER_LIMIT(int8, int8)
    NLS_INTEGER_LIMIT(uint8, uint8)
    NLS_INTEGER_LIMIT(int16, int16)
    NLS_INTEGER_LIMIT(uint16, uint16)
    NLS_INTEGER_LIMIT(uint32, uint32)
    NLS_INTEGER_LIMIT(int64, int64)
    NLS_INTEGER_LIMIT(uint64, uint64)
#undef NLS_INTEGER_LIMIT
    return ArrayOf::int32Scalar(
        largest ? std::numeric_limits<int32>::max() : std::numeric_limits<int32>::min());
}
//=============================================================================
// gives a constant the complexity and sparsity of the 'like' prototype
inline ArrayOf
applyConstantClassRequest(ArrayOf value, const ConstantClassRequest& request)
{
    if (request.isComplex && (value.isDoubleType() || value.isSingleType())) {
        value.promoteType(value.isSingleType() ? NLS_SCOMPLEX : NLS_DCOMPLEX);
    }
    if (request.isSparse && value.isDoubleType()) {
        value.makeSparse();
    }
    return value;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
