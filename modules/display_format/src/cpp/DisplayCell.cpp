//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "StringHelpers.hpp"
#include "DisplayCell.hpp"
#include "DisplayCellHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FormatHelpers.hpp"
#include "nlsBuildConfig.h"
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
    indexType rows = A.getRows();
    indexType columns = A.getColumns();
    indexType nbElements = A.getElementCount();

    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
    std::vector<size_t> vSize(columns, (size_t)0);
    wstringVector cellSummarize(nbElements, std::wstring(L""));
    sizeType termWidth = io->getTerminalWidth();

    bool isColumnsVector = A.isColumnVector();
#if WITH_OPENMP
#pragma omp parallel for
#endif
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
    bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

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
        if (withColumsHeader) {
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
                buffer.append(completeWithBlanksAtTheEnd(cellSummarize[idx], len));
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
    Dimensions dims = A.getDimensions();

    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;

    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
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
        for (indexType m = 2; m < dims.getLength(); m++) {
            io->outputMessage(fmt::sprintf(",%d", static_cast<int>(wdims[m]) + 1));
        }
        io->outputMessage(L") =\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }

        std::vector<size_t> vSize(columns, (size_t)0);
        indexType nbElements = rows * columns;
        wstringVector cellSummarize(nbElements, L"");

#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
            cellSummarize[k] = getAsFormattedString(
                elements, k + offset, currentNumericFormat, termWidth, false);
            vSize[k / rows] = std::max(vSize[k / rows], cellSummarize[k].length());
        }
        indexType colsPerPage = getColsPerPage(vSize, termWidth);
        int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0;
             k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
             k++) {
            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (withColumsHeader) {
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
                    std::wstring msg = completeWithBlanksAtBeginning(cellSummarize[idx], len);
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
    switch (elements[idx].getDataClass()) {
    case NLS_CHAR: {
        if (elements[idx].isRowVector()) {
            indexType nbCharsMax;
            if (asColumnsVector) {
                nbCharsMax = termWidth - 8;
            } else {
                nbCharsMax = 16;
            }
            msg = elements[idx].getContentAsWideString(nbCharsMax);

            if (msg.length() > nbCharsMax - 1) {
                msg.pop_back();
                msg = msg.substr(0, nbCharsMax - 1);
                msg = msg + HORIZONTAL_ELLIPSIS;
            }
            msg = L"{'" + msg + L"'}";
        } else {
            msg = summarizeCellEntry(elements[idx], 0, termWidth, currentNumericFormat, true);
        }
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
        msg = summarizeCellRealEntry<uint8>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT8: {
        msg = summarizeCellRealEntry<int8>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT16: {
        msg = summarizeCellRealEntry<uint16>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT16: {
        msg = summarizeCellRealEntry<int16>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT32: {
        msg = summarizeCellRealEntry<uint32>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT32: {
        msg = summarizeCellRealEntry<int32>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_UINT64: {
        msg = summarizeCellRealEntry<uint64>(
            elements[idx], 0, termWidth, currentNumericFormat, false);
        msg = L"{" + msg + L"}";
    } break;
    case NLS_INT64: {
        msg = summarizeCellRealEntry<int64>(
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
} // namespace Nelson
//=============================================================================
