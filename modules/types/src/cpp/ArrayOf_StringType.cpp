//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <wchar.h>
#include <stdio.h>
#include <limits>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isStringArray() const
{
    return (dp->dataClass == NLS_STRING_ARRAY);
}
//=============================================================================
bool
ArrayOf::isNdArrayString() const
{
    return (dp->dataClass == NLS_STRING_ARRAY) && !is2D();
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
ArrayOf::stringArrayConstructor(const stringVector values, Dimensions& dims)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = dims.getElementCount();
    if (nbElements != values.size()) {
        Error(_W("Invalid dimensions."));
    }
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc& e) {
            e.what();
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
ArrayOf::stringArrayConstructor(const wstringVector values, Dimensions& dims)
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
character2dArrayTotoStringArray(ArrayOf m)
{
    ArrayOf res;
    wchar_t* ptr = (wchar_t*)m.getDataPointer();
    indexType rows = m.getDimensions().getRows();
    indexType columns = m.getDimensions().getColumns();
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
characterNdArrayTotoStringArray(ArrayOf m)
{
    wstringVector v;
    Dimensions dimsM = m.getDimensions();
    indexType rows(dimsM.getRows());
    indexType columns(dimsM.getColumns());
    indexType nbElements = dimsM.getElementCount() / (rows * columns);
    wchar_t* ptr = (wchar_t*)m.getDataPointer();
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
    } catch (const std::bad_alloc& e) {
        e.what();
        Error(ERROR_MEMORY_ALLOCATION);
    }
    logical* ptr = (logical*)m.getDataPointer();
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
    } catch (const std::bad_alloc& e) {
        e.what();
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
    } catch (const std::bad_alloc& e) {
        e.what();
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
    } catch (const std::bad_alloc& e) {
        e.what();
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
        return ArrayOf();
    }
    Dimensions dimsM = m.getDimensions();
    if (m.isEmpty()) {
        ArrayOf* elements = nullptr;
        size_t nbElements = dimsM.getElementCount();
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc& e) {
            e.what();
            Error(ERROR_MEMORY_ALLOCATION);
        }
        return ArrayOf(NLS_STRING_ARRAY, dimsM, elements);
    }
    switch (m.getDataClass()) {
    case NLS_CELL_ARRAY: {
        ArrayOf* elementsOutput = nullptr;
        ArrayOf* elementsCell = (ArrayOf*)m.getDataPointer();
        size_t nbElements = dimsM.getElementCount();
        try {
            elementsOutput = new ArrayOf[nbElements];
        } catch (const std::bad_alloc& e) {
            e.what();
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            bool over;
            ArrayOf el = ArrayOf::toStringArray(elementsCell[k], over);
            if (over) {
                delete[] elementsOutput;
                needToOverload = true;
                return ArrayOf();
            }
            if (el.isScalar() || el.isEmpty()) {
                elementsOutput[k] = ArrayOf::characterArrayConstructor(el.getContentAsWideString());
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
        } else if (m.getDimensions().getLength() == 2) {
            return character2dArrayTotoStringArray(m);
        } else if (m.getDimensions().getLength() > 2) {
            return characterNdArrayTotoStringArray(m);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
