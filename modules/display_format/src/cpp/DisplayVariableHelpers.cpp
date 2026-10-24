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
#include <cstring>
#include <algorithm>
#include <limits>
#include "StringHelpers.hpp"
#include "DisplayVariableHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "IEEEFP.hpp"
#include "characters_encoding.hpp"
#include "DisplayIntegerHelpers.hpp"
#include "DisplayCellHelpers.hpp"
#include "FormatEngineeringNotation.hpp"
#include "FormatBank.hpp"
#include "FormatPlus.hpp"
#include "FormatHex.hpp"
#include "FormatHelpers.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::function<bool(const ArrayOf&, std::wstring&)> objectCompactDisplayHook;
static std::function<bool(const ArrayOf&, Dimensions&)> objectDimensionsHook;
static std::function<bool(const ArrayOf&, wstringVector&)> objectElementTextsHook;
//=============================================================================
static std::wstring
summarizeNestedCellElement(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat);
//=============================================================================
static bool
isStructOrObject(const ArrayOf& A);
//=============================================================================
void
setObjectCompactDisplayHook(std::function<bool(const ArrayOf&, std::wstring&)> hook)
{
    objectCompactDisplayHook = std::move(hook);
}
//=============================================================================
bool
callObjectCompactDisplayHook(const ArrayOf& A, std::wstring& representation)
{
    if (!objectCompactDisplayHook) {
        return false;
    }
    return objectCompactDisplayHook(A, representation);
}
//=============================================================================
void
setObjectDimensionsHook(std::function<bool(const ArrayOf&, Dimensions&)> hook)
{
    objectDimensionsHook = std::move(hook);
}
//=============================================================================
void
setObjectElementTextsHook(std::function<bool(const ArrayOf&, wstringVector&)> hook)
{
    objectElementTextsHook = std::move(hook);
}
//=============================================================================
bool
callObjectElementTextsHook(const ArrayOf& A, wstringVector& texts)
{
    if (!objectElementTextsHook) {
        return false;
    }
    return objectElementTextsHook(A, texts);
}
//=============================================================================
Dimensions
displayDimensions(const ArrayOf& A)
{
    if (objectDimensionsHook && A.isClassType()) {
        Dimensions dims;
        if (objectDimensionsHook(A, dims)) {
            return dims;
        }
    }
    return A.dimensions();
}
//=============================================================================
template <class T>
void
getMinMax(T* val, indexType nbElements, T* min, T* max)
{
    T minValue = val[0];
    T maxValue = val[0];
    T shared_max = maxValue;
    T shared_min = minValue;
#if WITH_OPENMP
#pragma omp parallel
#endif
    {
#if WITH_OPENMP
#pragma omp for nowait
#endif
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
            minValue = std::max(val[idx], minValue);
        }
#if WITH_OPENMP
#pragma omp critical
#endif
        {
            shared_max = std::max(shared_max, maxValue);
            shared_min = std::max(shared_min, minValue);
        }
    }
    *min = shared_min;
    *max = shared_max;
}
//=============================================================================
template <class T>
T
max(T* val, indexType nbElements)
{
    T maxValue = val[0];
    T shared_max = maxValue;
#if WITH_OPENMP
#pragma omp parallel
#endif
    {
#if WITH_OPENMP
#pragma omp for nowait
#endif
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
        }
#if WITH_OPENMP
#pragma omp critical
#endif
        {
            shared_max = std::max(shared_max, maxValue);
        }
    }
    return shared_max;
}
//=============================================================================
template <class T>
T
getMin(T* val, indexType nbElements)
{
    T minValue = val[0];
    T shared_min = minValue;
#if WITH_OPENMP
#pragma omp parallel
#endif
    {
#if WITH_OPENMP
#pragma omp for nowait
#endif
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            minValue = std::min(val[idx], minValue);
        }
#if WITH_OPENMP
#pragma omp critical
#endif
        {
            shared_min = std::max(shared_min, minValue);
        }
    }
    return shared_min;
}
//=============================================================================
std::wstring
completeWithBlanksAtTheEnd(const std::wstring& msg, size_t width)
{
    size_t len = msg.length();
    std::wstring blanks;
    if (int(width) - int(len) > 0) {
        blanks.append(width - len, L' ');
        return msg + blanks;
    }
    return msg;
}
//=============================================================================
std::wstring
completeWithBlanksAtBeginning(const std::wstring& msg, size_t width)
{
    size_t len = msg.length();
    std::wstring blanks;
    if (int(width) - int(len) > 0) {
        blanks.append(width - len, L' ');
        return blanks + msg;
    }
    return msg;
}
//=============================================================================
static std::wstring
getClassAsWideString(const ArrayOf& A, bool isInAcell)
{
    std::wstring typeAsText;
    switch (A.dataClass()) {
    case NLS_GO_HANDLE:
        typeAsText = L"graphics_object";
        break;
    case NLS_HANDLE:
        typeAsText = utf8_to_wstring(isInAcell ? A.handleClassName() : "handle");
        break;
    case NLS_UINT8:
        typeAsText = L"uint8";
        break;
    case NLS_INT8:
        typeAsText = L"int8";
        break;
    case NLS_UINT16:
        typeAsText = L"uint16";
        break;
    case NLS_INT16:
        typeAsText = L"int16";
        break;
    case NLS_UINT32:
        typeAsText = L"uint32";
        break;
    case NLS_INT32:
        typeAsText = L"int32";
        break;
    case NLS_UINT64:
        typeAsText = L"uint64";
        break;
    case NLS_INT64:
        typeAsText = L"int64";
        break;
    case NLS_SINGLE:
        typeAsText = (A.isSparse() && !isInAcell) ? L"sparse single" : L"single";
        break;
    case NLS_DOUBLE: {
        if (isInAcell) {
            typeAsText = L"double";
        } else {
            if (A.isSparse()) {
                typeAsText = L"sparse double";
            } else {
                typeAsText = L"double";
            }
        }
    } break;
    case NLS_LOGICAL: {
        if (isInAcell) {
            typeAsText = L"logical";
        } else {
            if (A.isSparse()) {
                typeAsText = L"sparse logical";
            } else {
                typeAsText = L"logical";
            }
        }
    } break;
    case NLS_CHAR:
        typeAsText = L"char";
        break;
    case NLS_SCOMPLEX:
        if (isInAcell) {
            typeAsText = L"single";
        } else if (A.isSparse()) {
            typeAsText = L"sparse complex single";
        } else if (A.isEmpty()) {
            typeAsText = L"complex single";
        } else {
            typeAsText = L"single";
        }
        break;
    case NLS_DCOMPLEX: {
        if (isInAcell) {
            typeAsText = L"double";
        } else {
            if (A.isEmpty()) {
                if (A.isSparse()) {
                    typeAsText = L"sparse complex double";
                } else {
                    typeAsText = L"complex double";
                }
            } else {
                if (A.isSparse()) {
                    typeAsText = L"sparse complex double";
                } else {
                    typeAsText = L"double";
                }
            }
        }
    } break;
    case NLS_CELL_ARRAY:
        typeAsText = L"cell";
        break;
    case NLS_FUNCTION_HANDLE: {
        typeAsText = utf8_to_wstring(NLS_FUNCTION_HANDLE_STR);
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        typeAsText = utf8_to_wstring(A.classType());
    } break;
    case NLS_STRUCT_ARRAY: {
        typeAsText = utf8_to_wstring(NLS_STRUCT_ARRAY_STR);
    } break;
    case NLS_STRING_ARRAY:
        typeAsText = L"string";
        break;
    case NLS_MISSING_ARRAY: {
        typeAsText = L"missing";
        break;
    } break;
    default: {
    } break;
    }
    return typeAsText;
}
//=============================================================================
static std::wstring
buildHeader(const ArrayOf& A)
{
    std::wstring msg;
    std::wstring typeAsText = getClassAsWideString(A, false);
    if (A.isScalar()
        && !(A.isCell() || A.isStruct() || A.isClassType() || A.isHandle() || A.isFunctionHandle()
            || A.isGraphicsObject())) {
        msg = L"  " + typeAsText + L"\n";
    } else {
        std::wstring dimensions = A.dimensions().toWideString();
        std::wstring dimensionsForHuman = L"";
        switch (A.dataClass()) {
        case NLS_GO_HANDLE: {
            typeAsText = L"[" + typeAsText + L"]";
        } break;
        case NLS_HANDLE: {
            if (A.dataPointer() != nullptr) {
                typeAsText = typeAsText + L" [" + utf8_to_wstring(A.handleClassName()) + L"]";
            }
        } break;
        case NLS_FUNCTION_HANDLE: {
            stringVector fieldnames = A.fieldNames();
            bool haveFields = !fieldnames.empty();
            bool isEmpty = A.isEmpty();
            bool isScalar = A.isScalar();
            std::wstring withPart;

            if (!haveFields) {
                withPart = L"with no value.";
            } else {
                withPart = L"with value:";
            }
            dimensionsForHuman = isEmpty || !isScalar ? L"array" : L"";
            if (isEmpty) {
                msg = fmt::format(
                    L"  {} {} {} {}", dimensions, L"empty", typeAsText, dimensionsForHuman);
            } else if (isScalar) {
                msg = fmt::format(L"  {} {}", typeAsText, withPart);
            } else {
                msg = fmt::format(
                    L"  {} {} {} {}", dimensions, typeAsText, dimensionsForHuman, withPart);
            }
            msg.append(L"\n");
            return msg;
        } break;
        case NLS_CLASS_ARRAY:
        case NLS_CLASSDEF_ARRAY: {
            stringVector fieldnames = A.fieldNames();
            bool haveFields = !fieldnames.empty();
            bool isEmpty = A.isEmpty();
            bool isScalar = A.isScalar();
            std::wstring withPart;

            if (!haveFields) {
                withPart = L"with no properties.";
            } else {
                withPart = L"with properties:";
            }
            dimensionsForHuman = isEmpty || !isScalar ? L"array" : L"";
            if (isEmpty) {
                msg = fmt::format(L"  {} {} {} {} {}", dimensions, L"empty", typeAsText,
                    dimensionsForHuman, withPart);
            } else if (isScalar) {
                msg = fmt::format(L"  {} {}", typeAsText, withPart);
            } else {
                msg = fmt::format(
                    L"  {} {} {} {}", dimensions, typeAsText, dimensionsForHuman, withPart);
            }
            msg.append(L"\n");
            return msg;
        } break;
        case NLS_STRUCT_ARRAY: {
            stringVector fieldnames = A.fieldNames();
            bool haveFields = !fieldnames.empty();
            bool isEmpty = A.isEmpty();
            bool isScalar = A.isScalar();
            std::wstring withPart;
            if (!haveFields) {
                withPart = L"with no fields.";
            } else {
                withPart = L"with fields:";
            }
            dimensionsForHuman = isEmpty || !isScalar ? L"array" : L"";
            if (isEmpty) {
                msg = fmt::format(L"  {} {} {} {} {}", dimensions, L"empty", typeAsText,
                    dimensionsForHuman, withPart);
            } else if (isScalar) {
                msg = fmt::format(L"  {} {}", typeAsText, withPart);
            } else {
                msg = fmt::format(
                    L"  {} {} {} {}", dimensions, typeAsText, dimensionsForHuman, withPart);
            }
            msg.append(L"\n");
            return msg;
        } break;
        case NLS_LOGICAL:
        case NLS_CELL_ARRAY:
        case NLS_STRING_ARRAY:
        case NLS_CHAR: {
            dimensionsForHuman = _W("array");
        } break;
        case NLS_MISSING_ARRAY: {
            dimensionsForHuman = _W("array");
        } break;
        default: {
            if (A.dimensions().length() > 2) {
                dimensionsForHuman = _W("array");
            } else if (A.isRowVector()) {
                dimensionsForHuman = _W("row vector");
            } else if (A.isColumnVector()) {
                dimensionsForHuman = _W("column vector");
            } else {
                dimensionsForHuman = _W("matrix");
            }
        } break;
        }
        if (!A.isEmpty()) {
            msg = fmt::format(L"  {} {} {}", dimensions, typeAsText, dimensionsForHuman);
            if (A.isSparse() && A.nonzeros() > 0) {
                indexType nonzeros = A.nonzeros();
                msg.append(fmt::format(
                    L" ({} {})", nonzeros, nonzeros == 1 ? _W("nonzero") : _W("nonzeros")));
            }
        } else {
            std::wstring empty = _W("empty");
            msg = fmt::format(L"  {} {} {} {}", dimensions, empty, typeAsText, dimensionsForHuman);
        }
        msg.append(L"\n");
    }
    return msg;
}
//=============================================================================
std::wstring
displayHeaderEqualSign(const ArrayOf& A, bool isPage)
{
    switch (A.dataClass()) {
    case NLS_CELL_ARRAY:
        return isPage ? L" = " : L" =";
    case NLS_STRING_ARRAY:
    case NLS_MISSING_ARRAY:
        return L" = ";
    default:
        return isStructOrObject(A) ? L" = " : L" =";
    }
}
//=============================================================================
void
DisplayVariableHeader(Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    if (!asDisp) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE
            && !name.empty()) {
            io->outputMessage(L"\n");
        }
        // an N-D struct array or object is summarized as a whole, other N-D arrays name each page
        bool namesEachPage = (A.dimensions().length() > 2) && !A.isEmpty() && !isStructOrObject(A);

        if (!name.empty() && !namesEachPage) {
            io->outputMessage(name + displayHeaderEqualSign(A) + L"\n");
            if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
        }
    }
    switch (A.dataClass()) {
    case NLS_CHAR: {
        if (!asDisp) {
            bool withType = (A.isEmpty() || !A.isRowVector());
            if (withType) {
                std::wstring msg = buildHeader(A);
                io->outputMessage(msg);
            }
        }
    } break;
    case NLS_DOUBLE: {
        if (!asDisp) {

            if (A.isEmpty()) {
                bool allEmpty = A.isEmpty(true);
                if (!allEmpty || A.isSparse()) {
                    std::wstring msg = buildHeader(A);
                    io->outputMessage(msg);
                }
            } else if (A.isSparse()) {
                io->outputMessage(buildHeader(A));
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        if (!asDisp) {
            if (A.isEmpty() || A.isSparse()) {
                std::wstring msg = buildHeader(A);
                io->outputMessage(msg);
            }
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (!asDisp) {

            if (!A.isScalar()) {
                std::wstring msg = buildHeader(A);
                io->outputMessage(msg);
            }
        }
    } break;

    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
    default: {
        if (!asDisp) {
            std::wstring msg = buildHeader(A);
            io->outputMessage(msg);
        }
    } break;
    }
}
//=============================================================================
void
DisplayVariableFooter(Interface* io, bool asDisp)
{
    if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE
        && !asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
std::wstring
summarizeStringArray(const ArrayOf& A, size_t beginningLineLength, size_t termWidth)
{
    std::wstring str;
    if (A.isEmpty()) {
        if (A.isCharacterArray()) {
            str = L"\"\"";
        } else {
            str = L"<missing>";
        }
    } else {
        if (A.dataClass() == NLS_DOUBLE) {
            str = L"<missing>";
        } else if (A.dataClass() == NLS_CHAR) {
            Dimensions dims = A.dimensions();
            if (dims.isRowVector()) {
                if (dims.columns() < static_cast<indexType>(termWidth - 3)) {
                    std::wstring str = A.asWideString();
                    str = L"\"" + str + L"\"";
                    return str;
                }
            }
            str = lightDescription(A, L"[", L"]");
        }
    }
    return str;
}
//=============================================================================
std::wstring
summarizeCellEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool recursive)
{
    std::wstring msg;
    if (A.isEmpty()) {
        if (A.isEmpty(true)) {
            if (A.dataClass() == NLS_CHAR) {
                msg = L"''";
            } else if (A.isCell()) {
                msg = L"{}";
            } else {
                msg = L"[]";
            }
            return msg;
        }
        if (A.isCell()) {
            return lightDescription(A, L"{", L"}");
        }
        return lightDescription(A, L"[", L"]");
    }
    if (A.isSparse() && A.rows() == 1 && A.columns() < termWidth) {
        // a sparse row reads like its full counterpart: [1 0 2]
        ArrayOf dense(A);
        dense.makeDense();
        return summarizeCellEntry(
            dense, beginningLineLength, termWidth, currentNumericFormat, recursive);
    }
    switch (A.dataClass()) {
    case NLS_MISSING_ARRAY: {
        msg = summarizeCellMissingEntry(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_CELL_ARRAY: {
        if (A.isScalar()) {
            ArrayOf* elements = (ArrayOf*)A.dataPointer();
            msg = L"{"
                + summarizeNestedCellElement(
                    elements[0], beginningLineLength + 1, termWidth, currentNumericFormat)
                + L"}";
        } else if (A.isRowVector()) {
            ArrayOf* elements = (ArrayOf*)A.dataPointer();
            indexType currentLength = beginningLineLength + 2;
            msg = L"{";
            for (indexType k = 0; k < A.elementCount(); ++k) {
                if (currentLength + 3 > termWidth) {
                    msg = lightDescription(A, L"{", L"}");
                    break;
                }
                msg.append(summarizeNestedCellElement(
                    elements[k], currentLength, termWidth, currentNumericFormat));
                if (k < A.elementCount() - 1) {
                    msg.append(L"  ");
                }
                currentLength = msg.size() + 1;
            }
            msg.append(L"}");
        } else {
            msg = lightDescription(A, L"{", L"}");
        }
    } break;
    case NLS_STRING_ARRAY: {
        msg = summarizeCellStringEntry(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_CHAR: {
        if (A.isRowVector()) {
            if (A.columns() < termWidth - beginningLineLength - 3) {
                msg = L"\'" + A.asWideString() + L"\'";
            } else {
                msg = lightDescription(A, L"[", L"]");
            }
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        msg = summarizeCellObjectEntry(A, beginningLineLength, termWidth, true);
    } break;
    case NLS_FUNCTION_HANDLE: {
        if (!callObjectCompactDisplayHook(A, msg)) {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_STRUCT_ARRAY:
    case NLS_GO_HANDLE:
    case NLS_HANDLE: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_LOGICAL: {
        msg = summarizeCellLogicalEntry(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT8: {
        msg = summarizeCellIntegerEntry<int8>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT8: {
        msg = summarizeCellIntegerEntry<uint8>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT16: {
        msg = summarizeCellIntegerEntry<int16>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT16: {
        msg = summarizeCellIntegerEntry<uint16>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT32: {
        msg = summarizeCellIntegerEntry<int32>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT32: {
        msg = summarizeCellIntegerEntry<uint32>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT64: {
        msg = summarizeCellIntegerEntry<int64>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT64: {
        msg = summarizeCellIntegerEntry<uint64>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_DOUBLE: {
        msg = summarizeCellRealEntry<double>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_DCOMPLEX: {
        msg = summarizeCellComplexEntry<double>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_SINGLE: {
        msg = summarizeCellRealEntry<single>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_SCOMPLEX: {
        msg = summarizeCellComplexEntry<single>(
            A, beginningLineLength, termWidth, currentNumericFormat, true);
    } break;
    default: {
    } break;
    }
    return msg;
}
//=============================================================================
std::wstring
lightDescription(const ArrayOf& A, const std::wstring& firstChar, const std::wstring& lastChar)
{
    std::wstring format = L"{}{} {}{}";
    return fmt::format(format, firstChar, displayDimensions(A).toWideString(),
        getClassAsWideString(A, true), lastChar);
}
//=============================================================================
std::wstring
columnsHeader(indexType startCol, indexType endCol)
{
    std::wstring msg;
    if (startCol == endCol) {
        msg = L"  "
            + formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:display_format:column", L"Column %d"),
                startCol);
    } else {
        msg = L"  "
            + formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:display_format:columnsThrough", L"Columns %d through %d"),
                startCol, endCol);
    }
    return msg;
}
//=============================================================================
/**
 * Element of a cell displayed inside a struct field or an object property:
 * a nested cell is summarized by its size, scalars keep their brackets.
 * {[1 2]  [3]  []}, {"abc"}, {{1x2 cell}}
 */
static std::wstring
summarizeNestedCellElement(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat)
{
    if (A.isCell() && !A.isEmpty()) {
        return lightDescription(A, L"{", L"}");
    }
    std::wstring msg
        = summarizeCellEntry(A, beginningLineLength, termWidth, currentNumericFormat, true);
    bool bracketed = A.isNumeric() || A.isLogical() || A.isStringArray() || A.isFunctionHandle();
    if (bracketed && A.isScalar() && !StringHelpers::starts_with(msg, L"[")) {
        return L"[" + msg + L"]";
    }
    return msg;
}
//=============================================================================
static bool
isStructOrObject(const ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
        return true;
    default:
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
