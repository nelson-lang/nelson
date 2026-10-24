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
#include "StringHelpers.hpp"
#include "DisplayCell.hpp"
#include "DisplayCellHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FormatHelpers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
getAsFormattedString(ArrayOf* elements, indexType idx, NumericFormatDisplay currentNumericFormat,
    indexType termWidth, bool asColumnsVector);
//=============================================================================
static indexType
getColsPerPage(const std::vector<size_t>& vSize, indexType termWidth);
//=============================================================================
static std::wstring
alignCellEntry(const ArrayOf& element, const std::wstring& entry, size_t width);
//=============================================================================
static void
DisplayEmptyCell(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dCell(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdCell(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayCell(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptyCell(io, A, name, currentNumericFormat, currentLineSpacing);
    } else if (A.isScalar() || A.is2D()) {
        Display2dCell(evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdCell(evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptyCell(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    // nothing to display
}
//=============================================================================
void
Display2dCell(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    indexType rows = A.rows();
    indexType columns = A.columns();
    indexType nbElements = A.elementCount();

    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    std::vector<size_t> vSize(columns, (size_t)0);
    wstringVector cellSummarize(nbElements, std::wstring(L""));
    sizeType termWidth = io->getTerminalWidth();

    bool isColumnsVector = A.isColumnVector();
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
        indexType v = (k / rows) % columns;
        cellSummarize[k]
            = getAsFormattedString(elements, k, currentNumericFormat, termWidth, isColumnsVector);
#if WITH_OPENMP
#pragma omp critical
#endif
        {
            vSize[v] = std::max(vSize[v], cellSummarize[k].length());
        }
    }

    indexType colsPerPage = getColsPerPage(vSize, termWidth);

    indexType pageCount
        = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
    bool withColumnsHeader = (rows * columns > 1) && pageCount > 1;

    bool continueDisplay = true;
    indexType block_page = 0;
    std::wstring buffer;

    for (indexType k = 0; k < pageCount && continueDisplay; k++) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            continueDisplay = false;
            break;
        }
        indexType colsInThisPage = columns - colsPerPage * k;
        colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;

        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            if (!name.empty() || k != 0) {
                buffer.append(L"\n");
            }
        }
        if (withColumnsHeader) {
            buffer.append(columnsHeader(k * colsPerPage + 1, k * colsPerPage + colsInThisPage));
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                buffer.append(L"\n\n");
            } else {
                buffer.append(L"\n");
            }
        }
        for (indexType i = 0; i < rows && continueDisplay; i++) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
                continueDisplay = false;
                break;
            }
            for (indexType j = 0; j < colsInThisPage; j++) {
                indexType colsPos = k * colsPerPage + j;
                indexType idx = i + (k * colsPerPage + j) * rows;
                indexType len = vSize[colsPos];
                buffer.append(BLANKS_AT_BOL);
                buffer.append(alignCellEntry(elements[idx], cellSummarize[idx], len));
            }
            buffer.append(L"\n");
            if (block_page >= io->getTerminalHeight()) {
                io->outputMessage(buffer);
                buffer.clear();
                block_page = 0;
            } else {
                block_page++;
            }
        }
        if (!buffer.empty()) {
            io->outputMessage(buffer);
            buffer.clear();
            block_page = 0;
        }
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
DisplayNdCell(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    sizeType termWidth = io->getTerminalWidth();
    Dimensions dims = A.dimensions();

    Dimensions wdims(dims.length());
    indexType rows(A.rows());
    indexType columns(A.columns());
    indexType offset = 0;

    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }
    bool continueDisplay = true;
    while (wdims.inside(dims) && continueDisplay) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            continueDisplay = false;
            break;
        }
        if (offset != 0) {
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
        }
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }

        io->outputMessage(name + L"(:,:");
        for (indexType m = 2; m < dims.length(); m++) {
            io->outputMessage(fmt::format(",{0}", static_cast<int>(wdims[m]) + 1));
        }
        io->outputMessage(L")" + displayHeaderEqualSign(A, true) + L"\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }

        std::vector<size_t> vSize(columns, (size_t)0);
        indexType nbElements = rows * columns;
        wstringVector cellSummarize(nbElements, L"");

        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
            cellSummarize[k] = getAsFormattedString(
                elements, k + offset, currentNumericFormat, termWidth, false);
            vSize[k / rows] = std::max(vSize[k / rows], cellSummarize[k].length());
        }
        indexType colsPerPage = getColsPerPage(vSize, termWidth);
        int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumnsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0;
            k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
            k++) {
            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (withColumnsHeader) {
                std::wstring msg
                    = columnsHeader(k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    if (k == 0) {
                        msg = msg + L"\n\n";
                    } else {
                        msg = L"\n" + msg + L"\n\n";
                    }
                } else {
                    msg = msg + L"\n";
                }
                io->outputMessage(msg);
            }
            for (indexType i = 0;
                i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                i++) {
                for (indexType j = 0; j < colsInThisPage
                    && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                    j++) {
                    indexType colsPos = k * colsPerPage + j;
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    indexType len = vSize[colsPos];
                    std::wstring msg
                        = alignCellEntry(elements[idx + offset], cellSummarize[idx], len);
                    io->outputMessage(BLANKS_AT_BOL + msg);
                }
                io->outputMessage(L"\n");
            }
        }
        offset += rows * columns;
        wdims.incrementModulo(dims, 2);
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
std::wstring
getAsFormattedString(ArrayOf* elements, indexType idx, NumericFormatDisplay currentNumericFormat,
    indexType termWidth, bool asColumnsVector)
{
    std::wstring msg;
    if (elements[idx].isEmpty()) {
        return lightDescription(elements[idx], L"{", L"}");
    }
    if (elements[idx].isSparse()) {
        if (elements[idx].rows() != 1 || elements[idx].columns() >= termWidth) {
            return lightDescription(elements[idx], L"{", L"}");
        }
        // a sparse row reads like its full counterpart: {[1 0 2]}
        ArrayOf dense(elements[idx]);
        dense.makeDense();
        return getAsFormattedString(&dense, 0, currentNumericFormat, termWidth, asColumnsVector);
    }
    switch (elements[idx].dataClass()) {
    case NLS_CHAR: {
        if (elements[idx].isRowVector()) {
            indexType nbCharsMax;
            if (asColumnsVector) {
                nbCharsMax = termWidth - 8;
            } else {
                nbCharsMax = 16;
            }
            msg = elements[idx].asWideString(nbCharsMax);

            if (msg.length() > nbCharsMax - 1) {
                msg.pop_back();
                msg = msg.substr(0, nbCharsMax - 1);
                msg = msg + HORIZONTAL_ELLIPSIS;
            }
            msg = L"{'" + msg + L"'}";
        } else {
            msg = lightDescription(elements[idx], L"{", L"}");
        }
    } break;
    case NLS_MISSING_ARRAY: {
        msg = summarizeCellMissingEntry(elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_STRING_ARRAY: {
        msg = summarizeCellStringEntry(elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_CELL_ARRAY: {
        msg = lightDescription(elements[idx], L"{", L"}");
    } break;
    case NLS_LOGICAL: {
        msg = summarizeCellLogicalEntry(elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT8: {
        msg = summarizeCellIntegerEntry<uint8>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT8: {
        msg = summarizeCellIntegerEntry<int8>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT16: {
        msg = summarizeCellIntegerEntry<uint16>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT16: {
        msg = summarizeCellIntegerEntry<int16>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT32: {
        msg = summarizeCellIntegerEntry<uint32>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT32: {
        msg = summarizeCellIntegerEntry<int32>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT64: {
        msg = summarizeCellIntegerEntry<uint64>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT64: {
        msg = summarizeCellIntegerEntry<int64>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_DOUBLE: {
        msg = summarizeCellRealEntry<double>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_SINGLE: {
        msg = summarizeCellRealEntry<single>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_DCOMPLEX: {
        msg = summarizeCellComplexEntry<double>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_SCOMPLEX: {
        msg = summarizeCellComplexEntry<single>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;

    case NLS_STRUCT_ARRAY: {
        msg = lightDescription(elements[idx], L"{", L"}");
    } break;
    case NLS_FUNCTION_HANDLE: {
        if (callObjectCompactDisplayHook(elements[idx], msg)) {
            msg = L"{" + msg + L"}";
        } else {
            msg = lightDescription(elements[idx], L"{", L"}");
        }
    } break;
    case NLS_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        msg = L"{" + summarizeCellObjectEntry(elements[idx], 0, termWidth, false) + L"}";
    } break;
    default: {
        if (elements[idx].isScalar()) {
            msg = summarizeCellEntry(elements[idx], 0, termWidth, currentNumericFormat, true);
            msg = L"{" + msg + L"}";
        } else {
            msg = lightDescription(elements[idx], L"{", L"}");
        }
    } break;
    }
    return msg;
}
//=============================================================================
indexType
getColsPerPage(const std::vector<size_t>& vSize, indexType termWidth)
{
    indexType colsPerPage = vSize.size();
    indexType len = 0;
    for (indexType k = 0; k < vSize.size(); ++k) {
        len = len + LENGTH_BLANKS_AT_BOL + vSize[k];
        if (len > floor((termWidth - 1))) {
            colsPerPage = k;
            break;
        }
    }
    if (colsPerPage == 0) {
        colsPerPage = 1;
    }
    return colsPerPage;
}
//=============================================================================
/**
 * Pads an entry to the column width inside its braces: numbers are right
 * aligned inside the brackets {[  1 2]}, strings and objects left aligned {["a" ]},
 * function handles right aligned {  @sin}, other entries padded before the
 * closing brace {'abc'  }.
 */
std::wstring
alignCellEntry(const ArrayOf& element, const std::wstring& entry, size_t width)
{
    if (entry.length() >= width) {
        return entry;
    }
    std::wstring blanks(width - entry.length(), L' ');
    if (entry.empty() || entry.back() != L'}') {
        return entry + blanks;
    }
    if (element.isFunctionHandle() && entry.compare(0, 2, L"{@") == 0) {
        return L"{" + blanks + entry.substr(1);
    }
    bool bracketed = entry.length() > 4 && entry.compare(0, 2, L"{[") == 0
        && entry.compare(entry.length() - 2, 2, L"]}") == 0;
    bool leftAligned = element.isStringArray() || element.dataClass() == NLS_MISSING_ARRAY
        || element.isClassType() || element.isHandle();
    if (!bracketed || leftAligned) {
        size_t closing = bracketed ? entry.length() - 2 : entry.length() - 1;
        return entry.substr(0, closing) + blanks + entry.substr(closing);
    }
    return L"{[" + blanks + entry.substr(2);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
