//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <cwchar>
#include <cstdio>
#include <limits>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
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
            ArrayOf* ptr = (ArrayOf*)getDataPointer();
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
ArrayOf::stringArrayConstructor(const std::string& value)
{
    stringVector strVector;
    strVector.push_back(value);
    Dimensions dims(1, 1);
    return stringArrayConstructor(strVector, dims);
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const std::wstring& value)
{
    wstringVector strVector;
    strVector.push_back(value);
    Dimensions dims(1, 1);
    return stringArrayConstructor(strVector, dims);
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructorAllMissing(Dimensions& dims)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = dims.getElementCount();
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    for (size_t k = 0; k < nbElements; k++) {
        elements[k] = ArrayOf::doubleConstructor(std::nan("NaN"));
    }
    return ArrayOf(NLS_STRING_ARRAY, dims, elements);
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const stringVector& values, const Dimensions& dims)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = dims.getElementCount();
    if (nbElements != values.size()) {
        Error(_W("Invalid dimensions."));
    }
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(values[k]);
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, dims, elements);
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const wstringVector& values, const Dimensions& dims)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = dims.getElementCount();
    if (nbElements != values.size()) {
        Error(_W("Invalid dimensions."));
    }
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(values[k]);
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOf
character2dArrayTotoStringArray(const ArrayOf& m)
{
    ArrayOf res;
    auto* ptr
        = static_cast<wchar_t*>(const_cast<void*>(static_cast<const void*>(m.getDataPointer())));
    indexType rows = m.getRows();
    indexType columns = m.getColumns();
    wstringVector v;
    for (indexType i = 0; i < rows; i++) {
        std::wstring s;
        for (indexType j = 0; j < columns; j++) {
            s.push_back(ptr[i + j * rows]);
        }
        v.push_back(s);
    }
    Dimensions dims(v.size(), 1);
    return ArrayOf::stringArrayConstructor(v, dims);
}
//=============================================================================
static ArrayOf
characterNdArrayTotoStringArray(const ArrayOf& m)
{
    wstringVector v;
    Dimensions dimsM = m.getDimensions();
    indexType rows(dimsM.getRows());
    indexType columns(dimsM.getColumns());
    indexType nbElements = dimsM.getElementCount() / (rows * columns);
    auto* ptr
        = static_cast<wchar_t*>(const_cast<void*>(static_cast<const void*>(m.getDataPointer())));
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
    for (indexType u = 0; u < dimsM.getLength(); u++) {
        if (u != 1) {
            outputDims[p] = dimsM[u];
            p++;
        }
    }
    return ArrayOf::stringArrayConstructor(v, outputDims);
}
//=============================================================================
static ArrayOf
logicalToStringArray(const ArrayOf& m)
{
    ArrayOf* elements = nullptr;
    Dimensions dimsM = m.getDimensions();
    size_t nbElements = dimsM.getElementCount();
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    auto* ptr = (logical*)m.getDataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        logical val = ptr[k];
        std::wstring str = L"false";
        if (val != logical(0)) {
            str = L"true";
        }
        elements[k] = ArrayOf::characterArrayConstructor(str);
    }
    return ArrayOf(NLS_STRING_ARRAY, dimsM, elements);
}
//=============================================================================
template <class T>
static ArrayOf
integerToStringArray(const ArrayOf& m)
{
    ArrayOf* elements = nullptr;
    Dimensions dimsM = m.getDimensions();
    size_t nbElements = dimsM.getElementCount();
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    T* ptr = (T*)m.getDataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        T val = ptr[k];
        std::wstring str = std::to_wstring(val);
        elements[k] = ArrayOf::characterArrayConstructor(str);
    }
    return ArrayOf(NLS_STRING_ARRAY, dimsM, elements);
}
//=============================================================================
template <class T>
static ArrayOf
complexToStringArray(const ArrayOf& m)
{
    ArrayOf* elements = nullptr;
    Dimensions dimsM = m.getDimensions();
    size_t nbElements = dimsM.getElementCount();
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    T* ptr = (T*)m.getDataPointer();
    for (size_t k = 0; k < nbElements * 2; k = k + 2) {
        std::string str;
        T realPart = ptr[k];
        T imagPart = ptr[k + 1];
        if (std::isnan(realPart) || std::isnan(imagPart)) {
            elements[k] = ArrayOf::emptyConstructor();
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
                char buffer[1024];
                if (std::abs(trunc(realPart) - realPart) < std::numeric_limits<double>::epsilon()) {
                    sprintf(buffer, "%d", (int)realPart);
                } else {
                    sprintf(buffer, "%.4f", realPart);
                }
                realStr = std::string(buffer);
            }
            T absImagPart = std::abs(imagPart);
            if (std::isinf(absImagPart)) {
                imagStr = "Inf";
            } else {
                char buffer[1024];
                if (std::abs(trunc(absImagPart) - absImagPart)
                    < std::numeric_limits<double>::epsilon()) {
                    sprintf(buffer, "%d", (int)absImagPart);
                } else {
                    sprintf(buffer, "%.4f", absImagPart);
                }
                imagStr = std::string(buffer);
            }
            if (imagPart > 0) {
                str = realStr + std::string("+") + imagStr + std::string("i");
            } else {
                str = realStr + std::string("-") + imagStr + std::string("i");
            }
            elements[k] = ArrayOf::characterArrayConstructor(str);
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, dimsM, elements);
}
//=============================================================================
template <class T>
static ArrayOf
realToStringArray(const ArrayOf& m)
{
    ArrayOf* elements = nullptr;
    Dimensions dimsM = m.getDimensions();
    size_t nbElements = dimsM.getElementCount();
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    T* ptr = (T*)m.getDataPointer();
    for (size_t k = 0; k < nbElements; k++) {
        T val = ptr[k];
        if (std::isnan(val)) {
            elements[k] = ArrayOf::emptyConstructor();
        } else {
            std::string str;
            if (std::isinf(val)) {
                if (val > 0) {
                    str = "Inf";
                } else {
                    str = "-Inf";
                }
            } else {
                char buffer[1024];
                if (std::abs(trunc(val) - val) < std::numeric_limits<double>::epsilon()) {
                    sprintf(buffer, "%d", (int)val);
                } else {
                    sprintf(buffer, "%.4f", val);
                }
                str = std::string(buffer);
            }
            elements[k] = ArrayOf::characterArrayConstructor(str);
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, dimsM, elements);
}
//=============================================================================
ArrayOf
ArrayOf::toStringArray(ArrayOf m, bool& needToOverload)
{
    needToOverload = false;
    if (m.isStringArray()) {
        return m;
    }
    if (m.isSparse()) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsM = m.getDimensions();
    if (m.isEmpty()) {
        if (m.isCharacterArray()) {
            if (m.is2D() && m.getRows() == m.getColumns()) {
                return ArrayOf::stringArrayConstructor("");
            }
            Dimensions newDims;
            indexType i = 0;
            for (indexType k = 0; k < dimsM.getLength(); ++k) {
                if (k != 1) {
                    newDims[i] = dimsM.getAt(k);
                    i++;
                }
            }
            if (newDims.getLength() < 2) {
                newDims[i] = 1;
            }
            ArrayOf* elements = nullptr;
            size_t nbElements = newDims.getElementCount();
            try {
                elements = new ArrayOf[nbElements];
                for (indexType k = 0; k < nbElements; ++k) {
                    elements[k] = ArrayOf::characterArrayConstructor("");
                }
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            return ArrayOf(NLS_STRING_ARRAY, newDims, elements);
        }
        ArrayOf* elements = nullptr;
        size_t nbElements = dimsM.getElementCount();
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        return ArrayOf(NLS_STRING_ARRAY, dimsM, elements);
    }
    switch (m.getDataClass()) {
    case NLS_CELL_ARRAY: {
        ArrayOf* elementsOutput = nullptr;
        auto* elementsCell = (ArrayOf*)m.getDataPointer();
        size_t nbElements = dimsM.getElementCount();
        try {
            elementsOutput = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            bool over;
            ArrayOf el = ArrayOf::toStringArray(elementsCell[k], over);
            if (over) {
                delete[] elementsOutput;
                needToOverload = true;
                return {};
            }
            if (el.isEmpty()) {
                elementsOutput[k] = ArrayOf::characterArrayConstructor("");
            } else if (el.isScalar()) {
                if (el.isStringArray()) {
                    ArrayOf* p = (ArrayOf*)el.getDataPointer();
                    ArrayOf p0 = p[0];
                    if (p0.isEmpty() && !p0.isCharacterArray()) {
                        elementsOutput[k] = ArrayOf::doubleConstructor(std::nan("NaN"));
                    } else {
                        elementsOutput[k]
                            = ArrayOf::characterArrayConstructor(p0.getContentAsWideString());
                    }
                } else {
                    elementsOutput[k]
                        = ArrayOf::characterArrayConstructor(el.getContentAsWideString());
                }
            } else {
                Error(_W("Unable to convert supplied object to a string."));
            }
        }
        return ArrayOf(NLS_STRING_ARRAY, dimsM, elementsOutput);
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
            return ArrayOf::stringArrayConstructor(m.getContentAsWideString());
        }
        if (m.nDims() == 2) {
            return character2dArrayTotoStringArray(m);
        }
        if (m.nDims() > 2) {
            return characterNdArrayTotoStringArray(m);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
