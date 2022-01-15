//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cstring>
#include <algorithm>
#include <limits>
#include <boost/algorithm/string.hpp>
#include "DisplayVariableHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "IEEEFP.hpp"
#include "characters_encoding.hpp"
#include "DisplayIntegerHelpers.hpp"
#include "DisplayCellHelpers.hpp"
#include "FormatShort.hpp"
#include "FormatShortE.hpp"
#include "FormatShortG.hpp"
#include "FormatShortEng.hpp"
#include "FormatLongE.hpp"
#include "FormatLongG.hpp"
#include "FormatLongEng.hpp"
#include "FormatBank.hpp"
#include "FormatPlus.hpp"
#include "FormatHex.hpp"
#include "FormatRational.hpp"
#include "FormatHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
getMinMax(T* val, indexType nbElements, T* min, T* max)
{
    T minValue = val[0];
    T maxValue = val[0];
    T shared_max = maxValue;
    T shared_min = minValue;

#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
            minValue = std::max(val[idx], minValue);
        }
#pragma omp critical
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
getMax(T* val, indexType nbElements)
{
    T maxValue = val[0];
    T shared_max = maxValue;
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
        }
#pragma omp critical
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
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            minValue = std::min(val[idx], minValue);
        }
#pragma omp critical
        {
            shared_min = std::max(shared_min, minValue);
        }
    }
    return shared_min;
}
//=============================================================================
std::wstring
completeWithBlanksAtBeginning(const std::wstring& msg, NumericFormatDisplay currentNumericFormat)
{
    size_t width = 10;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
        width = 13;
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        width = 0;
    } break;
    default: { } break; }
    return completeWithBlanksAtBeginning(msg, width);
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
    switch (A.getDataClass()) {
    case NLS_GO_HANDLE:
        typeAsText = L"graphic_object";
        break;
    case NLS_HANDLE:
        typeAsText = L"handle";
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
        typeAsText = L"single";
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
        } else {
            if (A.isEmpty()) {
                typeAsText = L"complex single";
            } else {
                typeAsText = L"single";
            }
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
                    typeAsText = L"sparse double";
                } else {
                    typeAsText = L"double";
                }
            }
        }
    } break;
    case NLS_CELL_ARRAY:
        typeAsText = L"cell";
        break;
    case NLS_STRUCT_ARRAY: {
        if (A.isClassStruct()) {
            typeAsText = utf8_to_wstring(A.getStructType());
        } else {
            typeAsText = utf8_to_wstring(NLS_STRUCT_ARRAY_STR);
        }
    } break;
    case NLS_STRING_ARRAY:
        typeAsText = L"string";
        break;
    default: { } break; }
    return typeAsText;
}
//=============================================================================
static std::wstring
buildHeader(const ArrayOf& A)
{
    std::wstring msg;
    std::wstring typeAsText = getClassAsWideString(A, false);
    if (A.isScalar() && !(A.isCell() || A.isStruct() || A.isHandle() || A.isGraphicObject())) {
        msg = L"  " + typeAsText + L"\n";
    } else {
        std::wstring dimensions = A.getDimensions().toWideString();
        std::wstring dimensionsForHuman = L"";
        switch (A.getDataClass()) {
        case NLS_GO_HANDLE: {
            return msg;
        } break;
        case NLS_HANDLE: {
            if (A.isScalar()) {
                typeAsText = typeAsText + L" [" + A.getHandleCategory() + L"]";
            }
        } break;
        case NLS_STRUCT_ARRAY: {
            stringVector fieldnames = A.getFieldNames();
            bool haveFields = !fieldnames.empty();
            bool isEmpty = A.isEmpty();
            bool isScalar = A.isScalar();
            std::wstring withPart;

            if (A.isClassStruct()) {
                if (!haveFields) {
                    withPart = L"with no properties.";
                } else {
                    withPart = L"with properties:";
                }
            } else {
                if (!haveFields) {
                    withPart = L"with no fields.";
                } else {
                    withPart = L"with fields:";
                }
            }
            dimensionsForHuman = isEmpty || !isScalar ? L"array" : L"";
            if (isEmpty) {
                msg = fmt::sprintf(L"  %s %s %s %s %s", dimensions, L"empty", typeAsText,
                    dimensionsForHuman, withPart);
            } else if (isScalar) {
                msg = fmt::sprintf(L"  %s %s", typeAsText, withPart);
            } else {
                msg = fmt::sprintf(
                    L"  %s %s %s %s", dimensions, typeAsText, dimensionsForHuman, withPart);
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
        default: {
            if (A.getDimensions().getLength() > 2) {
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
            msg = fmt::sprintf(L"  %s %s %s", dimensions, typeAsText, dimensionsForHuman);
        } else {
            std::wstring empty = _W("empty");
            msg = fmt::sprintf(L"  %s %s %s %s", dimensions, empty, typeAsText, dimensionsForHuman);
        }
        msg.append(L"\n");
    }
    return msg;
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
        bool isNdArrayAndNotEmpty = (A.getDimensions().getLength() > 2) && !A.isEmpty();

        if (!name.empty() && !isNdArrayAndNotEmpty) {
            io->outputMessage(name + L" =\n");
            if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
        }
    }
    switch (A.getDataClass()) {
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
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        if (!asDisp) {
            if (A.isEmpty()) {
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
    /*
      if (!name.empty()) {
          bool isNdArrayAndNotEmpty = (A.getDimensions().getLength() > 2) && !A.isEmpty();
          if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
              == NLS_LINE_SPACING_COMPACT) {
              if (!isNdArrayAndNotEmpty) {
                  io->outputMessage(name + L" =\n");
              }
          } else {
              io->outputMessage(L"\n");
              if (!isNdArrayAndNotEmpty) {
                  io->outputMessage(name + L" =\n\n");
              }
          }
          switch (A.getDataClass()) {
          case NLS_CHAR: {
              bool withType = A.isEmpty() || !A.isRowVector();
              if (withType) {
                  std::wstring msg = buildHeader(A);
                  io->outputMessage(msg);
              }
          } break;
          case NLS_DOUBLE:
          case NLS_DCOMPLEX: {
              if (A.isEmpty()) {
                  bool allEmpty = A.isEmpty(true);
                  if (!allEmpty || A.isSparse()) {
                      std::wstring msg = buildHeader(A);
                      io->outputMessage(msg);
                  }
              }
          } break;
          case NLS_STRING_ARRAY: {
              if (!A.isScalar()) {
                  std::wstring msg = buildHeader(A);
                  io->outputMessage(msg);
              }
          } break;

          case NLS_GO_HANDLE:
          case NLS_HANDLE:
          case NLS_CELL_ARRAY:
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
              std::wstring msg = buildHeader(A);
              io->outputMessage(msg);
          } break;
          }
      }
      */
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
summarizeStringArray(const ArrayOf& A, size_t beginingLineLength, size_t termWidth)
{
    std::wstring str;
    if (A.isEmpty()) {
        if (A.isCharacterArray()) {
            str = L"\"\"";
        } else {
            str = L"<missing>";
        }
    } else {
        if (A.getDataClass() == NLS_DOUBLE) {
            str = L"<missing>";
        } else if (A.getDataClass() == NLS_CHAR) {
            Dimensions dims = A.getDimensions();
            if (dims.isRowVector()) {
                if (dims.getColumns() < static_cast<indexType>(termWidth - 3)) {
                    std::wstring str = A.getContentAsWideString();
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

/**
 * Print this object when it is an element of a cell array.  This is
 * generally a shorthand summary of the description of the object.
 */
std::wstring
summarizeCellEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool recursive)
{
    std::wstring msg;
    if (A.isEmpty()) {
        if (A.isEmpty(true)) {
            if (A.getDataClass() == NLS_CHAR) {
                msg = L"''";
            } else if (A.isCell()) {
                msg = L"{}";
            } else {
                msg = L"[]";
            }
            return msg;
        } else {
            return lightDescription(A, L"{", L"}");
        }
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY: {
        if (A.isScalar()) {
            ArrayOf* elements = (ArrayOf*)A.getDataPointer();
            msg = L"{"
                + summarizeCellEntry(
                      elements[0], beginingLineLength + 1, termWidth, currentNumericFormat, true)
                + L"}";
        } else if (A.isRowVector()) {
            ArrayOf* elements = (ArrayOf*)A.getDataPointer();
            indexType currentLength = beginingLineLength + 2;
            msg = L"{";
            for (indexType k = 0; k < A.getElementCount(); ++k) {
                if (currentLength + 3 > termWidth) {
                    msg = lightDescription(A, L"{", L"}");
                    break;
                }
                if (elements[k].isCell()) {
                    msg.append(lightDescription(elements[k], L"{", L"}"));
                } else {
                    msg.append(summarizeCellEntry(
                        elements[k], currentLength, termWidth, currentNumericFormat, true));
                }
                if (k < A.getElementCount() - 1) {
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
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_STRUCT_ARRAY: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_CHAR: {
        if (A.isRowVector()) {
            if (A.getColumns() < termWidth - beginingLineLength - 3) {
                msg = L"\'" + A.getContentAsWideString() + L"\'";
            } else {
                msg = lightDescription(A, L"[", L"]");
            }
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_GO_HANDLE: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_HANDLE: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_LOGICAL: {
        msg = summarizeCellLogicalEntry(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT8: {
        msg = summarizeCellRealEntry<int8>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT8: {
        msg = summarizeCellRealEntry<uint8>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT16: {
        msg = summarizeCellRealEntry<int16>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT16: {
        msg = summarizeCellRealEntry<uint16>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT32: {
        msg = summarizeCellRealEntry<int32>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT32: {
        msg = summarizeCellRealEntry<uint32>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_INT64: {
        msg = summarizeCellRealEntry<int64>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_UINT64: {
        msg = summarizeCellRealEntry<uint64>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_DOUBLE: {
        msg = summarizeCellRealEntry<double>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_DCOMPLEX: {
        msg = summarizeCellComplexEntry<double>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_SINGLE: {
        msg = summarizeCellRealEntry<single>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    case NLS_SCOMPLEX: {
        msg = summarizeCellComplexEntry<single>(
            A, beginingLineLength, termWidth, currentNumericFormat, true);
    } break;
    default: { } break; }
    return msg;
}
//=============================================================================
std::wstring
lightDescription(const ArrayOf& A, const std::wstring& firstChar, const std::wstring& lastChar)
{
    std::wstring format = L"%s%s %s%s";
    return fmt::sprintf(format, firstChar, A.getDimensions().toWideString(),
        getClassAsWideString(A, true), lastChar);
}
//=============================================================================
std::wstring
columnsHeader(indexType startCol, indexType endCol)
{
    std::wstring msg;
    if (startCol == endCol) {
        msg = fmt::sprintf(_W("  Columns %d"), startCol);
    } else {
        msg = fmt::sprintf(_W("  Columns %d through %d"), startCol, endCol);
    }
    return msg;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
