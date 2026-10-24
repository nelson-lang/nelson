//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include <algorithm>
#include <cmath>
#include <cwchar>
#include <cstdio>
#include <limits>
#include <memory>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isConvertibleToStringScalar(const ArrayOf& value);
static void
raiseMustBeConvertibleCellArray(size_t elementIndex);
//=============================================================================
static ArrayOf
stringArrayScalarFromValue(const ArrayOf& value)
{
    Dimensions dims(1, 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, 1, stringVector(), false));
    elements[0] = value;
    return ArrayOf(NLS_STRING_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOf
stringArrayFromOwnedElements(const Dimensions& dims, std::unique_ptr<ArrayOf[]>& elements)
{
    ArrayOf* rawElements = elements.get();
    ArrayOf res(NLS_STRING_ARRAY, dims, rawElements);
    elements.release();
    return res;
}
//=============================================================================
bool
ArrayOf::isStringArray() const
{
    if (dp) {
        return (dp->dataClass == NLS_STRING_ARRAY);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isScalarStringArray(bool withMissing) const
{
    if (isScalar() && isStringArray()) {
        if (!withMissing) {
            ArrayOf* ptr = (ArrayOf*)dataPointer();
            if (ptr && ptr->isDoubleClass()) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isNdArrayString() const
{
    if (dp) {
        return (dp->dataClass == NLS_STRING_ARRAY) && !is2D();
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::stringArray(const std::string& value)
{
    return stringArrayScalarFromValue(ArrayOf::characterArray(value));
}
//=============================================================================
ArrayOf
ArrayOf::stringArray(const std::wstring& value)
{
    return stringArrayScalarFromValue(ArrayOf::characterArray(value));
}
//=============================================================================
ArrayOf
ArrayOf::missingStringArray(Dimensions& dims)
{
    size_t nbElements = dims.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements.reset(new ArrayOf[nbElements]);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    for (size_t k = 0; k < nbElements; k++) {
        elements[k] = ArrayOf::doubleScalar(std::nan(""));
    }
    return stringArrayFromOwnedElements(dims, elements);
}
//=============================================================================
ArrayOf
ArrayOf::stringArray(const stringVector& values, const Dimensions& dims)
{
    size_t nbElements = dims.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    if (nbElements != values.size()) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    if (nbElements > 0) {
        try {
            elements.reset(new ArrayOf[nbElements]);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArray(values[k]);
        }
    }
    return stringArrayFromOwnedElements(dims, elements);
}
//=============================================================================
ArrayOf
ArrayOf::stringArray(const wstringVector& values, const Dimensions& dims)
{
    size_t nbElements = dims.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    if (nbElements != values.size()) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    if (nbElements > 0) {
        try {
            elements.reset(new ArrayOf[nbElements]);
        } catch (std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArray(values[k]);
        }
    }
    return stringArrayFromOwnedElements(dims, elements);
}
//=============================================================================
static ArrayOf
character2dArrayTotoStringArray(const ArrayOf& m)
{
    ArrayOf res;
    auto* ptr = static_cast<wchar_t*>(const_cast<void*>(static_cast<const void*>(m.dataPointer())));
    indexType rows = m.rows();
    indexType columns = m.columns();
    wstringVector v;
    for (indexType i = 0; i < rows; i++) {
        std::wstring s;
        for (indexType j = 0; j < columns; j++) {
            s.push_back(ptr[i + j * rows]);
        }
        v.push_back(s);
    }
    Dimensions dims(v.size(), 1);
    return ArrayOf::stringArray(v, dims);
}
//=============================================================================
static ArrayOf
characterNdArrayTotoStringArray(const ArrayOf& m)
{
    wstringVector v;
    Dimensions dimsM = m.dimensions();
    indexType rows(dimsM.rows());
    indexType columns(dimsM.columns());
    indexType nbElements = dimsM.elementCount() / (rows * columns);
    auto* ptr = static_cast<wchar_t*>(const_cast<void*>(static_cast<const void*>(m.dataPointer())));
    indexType offset = 0;
    for (indexType k = 0; k < nbElements; k++) {
        for (indexType i = 0; i < rows; i++) {
            std::wstring s;
            for (indexType j = 0; j < columns; j++) {
                s.push_back(ptr[i + j * rows + offset]);
            }
            v.push_back(s);
        }
        offset += rows * columns;
    }
    indexType p = 0;
    Dimensions outputDims;
    for (indexType u = 0; u < dimsM.length(); u++) {
        if (u != 1) {
            outputDims[p] = dimsM[u];
            p++;
        }
    }
    return ArrayOf::stringArray(v, outputDims);
}
//=============================================================================
static ArrayOf
logicalToStringArray(const ArrayOf& m)
{
    Dimensions dimsM = m.dimensions();
    size_t nbElements = dimsM.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements.reset(new ArrayOf[nbElements]);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    auto* ptr = (logical*)m.dataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        logical val = ptr[k];
        std::wstring str = L"false";
        if (val != logical(0)) {
            str = L"true";
        }
        elements[k] = ArrayOf::characterArray(str);
    }
    return stringArrayFromOwnedElements(dimsM, elements);
}
//=============================================================================
template <class T>
static ArrayOf
integerToStringArray(const ArrayOf& m)
{
    Dimensions dimsM = m.dimensions();
    size_t nbElements = dimsM.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements.reset(new ArrayOf[nbElements]);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    T* ptr = (T*)m.dataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        T val = ptr[k];
        std::wstring str = std::to_wstring(val);
        elements[k] = ArrayOf::characterArray(str);
    }
    return stringArrayFromOwnedElements(dimsM, elements);
}
//=============================================================================
static std::string
trimTrailingZeros(std::string str)
{
    size_t dot = str.find('.');
    if (dot == std::string::npos) {
        return str;
    }
    while (!str.empty() && str.back() == '0') {
        str.pop_back();
    }
    if (!str.empty() && str.back() == '.') {
        str.pop_back();
    }
    return str;
}
//=============================================================================
// Text of a finite real value: integers below 1e16 print every digit, larger
// magnitudes use 16 significant digits, values below 1e-4 use 5 significant
// digits, anything else keeps 4 decimals (5 significant digits for values
// below one) capped to 16 significant digits, trailing zeros removed.
static std::string
formatRealForString(double val)
{
    double magnitude = std::fabs(val);
    if (magnitude == 0) {
        return "0"; // also for a negative zero
    }
    if (val == std::floor(val) && magnitude < 1e16) {
        return fmt::format("{:.0f}", val);
    }
    if (magnitude >= 1e16) {
        return fmt::format("{:.16g}", val);
    }
    if (magnitude < 1e-4) {
        return fmt::format("{:.5g}", val);
    }
    int exponent = static_cast<int>(std::floor(std::log10(magnitude)));
    int integerDigits = exponent >= 0 ? exponent + 1 : 1;
    int decimals = exponent >= 0 ? 4 : 4 - exponent;
    decimals = std::max(0, std::min(decimals, 16 - integerDigits));
    return trimTrailingZeros(fmt::format("{:.{}f}", val, decimals));
}
//=============================================================================
template <class T>
static ArrayOf
complexToStringArray(const ArrayOf& m)
{
    Dimensions dimsM = m.dimensions();
    size_t nbElements = dimsM.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements.reset(new ArrayOf[nbElements]);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    T* ptr = (T*)m.dataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        std::string str;
        T realPart = ptr[2 * k];
        T imagPart = ptr[2 * k + 1];
        if (std::isnan(realPart) || std::isnan(imagPart)) {
            elements[k] = ArrayOf::empty();
        } else {
            std::string realStr;
            std::string imagStr;
            if (std::isinf(realPart)) {
                if (realPart > 0) {
                    realStr = "Inf";
                } else {
                    realStr = "-Inf";
                }
            } else {
                realStr = formatRealForString(static_cast<double>(realPart));
            }
            T absImagPart = std::abs(imagPart);
            if (std::isinf(absImagPart)) {
                imagStr = "Inf";
            } else {
                imagStr = formatRealForString(static_cast<double>(absImagPart));
            }
            if (imagPart >= 0) {
                str = realStr + std::string("+") + imagStr + std::string("i");
            } else {
                str = realStr + std::string("-") + imagStr + std::string("i");
            }
            elements[k] = ArrayOf::characterArray(str);
        }
    }
    return stringArrayFromOwnedElements(dimsM, elements);
}
//=============================================================================
template <class T>
static ArrayOf
realToStringArray(const ArrayOf& m)
{
    Dimensions dimsM = m.dimensions();
    size_t nbElements = dimsM.elementCount();
    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements.reset(new ArrayOf[nbElements]);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    T* ptr = (T*)m.dataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        T val = ptr[k];
        if (std::isnan(val)) {
            elements[k] = ArrayOf::empty();
        } else {
            std::string str;
            if (std::isinf(val)) {
                if (val > 0) {
                    str = "Inf";
                } else {
                    str = "-Inf";
                }
            } else {
                str = formatRealForString(static_cast<double>(val));
            }
            elements[k] = ArrayOf::characterArray(str);
        }
    }
    return stringArrayFromOwnedElements(dimsM, elements);
}
//=============================================================================
ArrayOf
ArrayOf::toStringArray(const ArrayOf& m, bool& needToOverload)
{
    needToOverload = false;
    if (m.isStringArray()) {
        return m;
    }
    if (m.isSparse()) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsM = m.dimensions();
    if (m.isEmpty()) {
        if (m.isCharacterArray()) {
            if (m.is2D() && m.rows() == m.columns()) {
                return ArrayOf::stringArray("");
            }
            Dimensions newDims;
            indexType i = 0;
            for (indexType k = 0; k < dimsM.length(); ++k) {
                if (k != 1) {
                    newDims[i] = dimsM.at(k);
                    i++;
                }
            }
            if (newDims.length() < 2) {
                newDims[i] = 1;
            }
            size_t nbElements = newDims.elementCount();
            std::unique_ptr<ArrayOf[]> elements;
            try {
                elements.reset(new ArrayOf[nbElements]);
                for (indexType k = 0; k < nbElements; ++k) {
                    elements[k] = ArrayOf::characterArray("");
                }
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            return stringArrayFromOwnedElements(newDims, elements);
        }
        size_t nbElements = dimsM.elementCount();
        std::unique_ptr<ArrayOf[]> elements;
        try {
            elements.reset(new ArrayOf[nbElements]);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        return stringArrayFromOwnedElements(dimsM, elements);
    }
    switch (m.dataClass()) {
    case NLS_CELL_ARRAY: {
        auto* elementsCell = (ArrayOf*)m.dataPointer();
        size_t nbElements = dimsM.elementCount();
        std::unique_ptr<ArrayOf[]> elementsOutput;
        try {
            elementsOutput.reset(new ArrayOf[nbElements]);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        for (size_t k = 0; k < nbElements; k++) {
            if (!isConvertibleToStringScalar(elementsCell[k])) {
                raiseMustBeConvertibleCellArray(k + 1);
            }
            bool over;
            ArrayOf el = ArrayOf::toStringArray(elementsCell[k], over);
            if (over) {
                needToOverload = true;
                return {};
            }
            if (el.isEmpty()) {
                elementsOutput[k] = ArrayOf::characterArray("");
            } else if (el.isScalar()) {
                if (el.isStringArray()) {
                    ArrayOf* p = (ArrayOf*)el.dataPointer();
                    ArrayOf p0 = p[0];
                    if (p0.isEmpty() && !p0.isCharacterArray()) {
                        elementsOutput[k] = ArrayOf::doubleScalar(std::nan(""));
                    } else {
                        elementsOutput[k] = ArrayOf::characterArray(p0.asWideString());
                    }
                } else {
                    elementsOutput[k] = ArrayOf::characterArray(el.asWideString());
                }
            } else {
                raiseMustBeConvertibleCellArray(k + 1);
            }
        }
        return stringArrayFromOwnedElements(dimsM, elementsOutput);
    } break;
    case NLS_LOGICAL: {
        return logicalToStringArray(m);
    } break;
    case NLS_UINT8: {
        return integerToStringArray<uint8>(m);
    } break;
    case NLS_INT8: {
        return integerToStringArray<int8>(m);
    } break;
    case NLS_UINT16: {
        return integerToStringArray<uint16>(m);
    } break;
    case NLS_INT16: {
        return integerToStringArray<int16>(m);
    } break;
    case NLS_UINT32: {
        return integerToStringArray<uint32>(m);
    } break;
    case NLS_INT32: {
        return integerToStringArray<int32>(m);
    } break;
    case NLS_UINT64: {
        return integerToStringArray<uint64>(m);
    } break;
    case NLS_INT64: {
        return integerToStringArray<int64>(m);
    } break;
    case NLS_SINGLE: {
        return realToStringArray<single>(m);
    } break;
    case NLS_DOUBLE: {
        return realToStringArray<double>(m);
    } break;
    case NLS_SCOMPLEX: {
        return complexToStringArray<single>(m);
    } break;
    case NLS_DCOMPLEX: {
        return complexToStringArray<double>(m);
    } break;
    case NLS_CHAR: {
        if (m.isRowVectorCharacterArray()) {
            return stringArrayScalarFromValue(m);
        }
        if (m.nDims() == 2) {
            return character2dArrayTotoStringArray(m);
        }
        if (m.nDims() > 2) {
            return characterNdArrayTotoStringArray(m);
        }
    } break;
    case NLS_MISSING_ARRAY: {
        // every element, whatever the size, becomes <missing> through NaN
        ArrayOf asDouble(m);
        asDouble.promoteType(NLS_DOUBLE);
        return realToStringArray<double>(asDouble);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
// A cell element becomes one string: a scalar, a character row ('' too) or a
// 1x1 cell holding such a value. A function handle, a struct or a sparse
// array never does; an object is left to its own conversion.
bool
isConvertibleToStringScalar(const ArrayOf& value)
{
    switch (value.dataClass()) {
    case NLS_CELL_ARRAY: {
        return value.isScalar()
            && isConvertibleToStringScalar(static_cast<const ArrayOf*>(value.dataPointer())[0]);
    }
    case NLS_CHAR: {
        return value.is2D() && (value.rows() == 1 || (value.rows() == 0 && value.columns() == 0));
    }
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        return false;
    }
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        return true;
    }
    default: {
        return !value.isSparse() && value.isScalar();
    }
    }
}
//=============================================================================
void
raiseMustBeConvertibleCellArray(size_t elementIndex)
{
    Error("Nelson:string:MustBeConvertibleCellArray",
        "Conversion from cell failed. Element %d must be convertible to a string scalar.",
        static_cast<int>(elementIndex));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
