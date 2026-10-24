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
#include "DisplayString.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
getAsFormattedString(ArrayOf* elements, indexType idx, size_t maxLength);
//=============================================================================
static size_t
getMaxElementLength(indexType columns, indexType termWidth);
//=============================================================================
static indexType
getColsPerPage(const std::vector<size_t>& vSize, indexType termWidth);
//=============================================================================
static void
DisplayEmptyString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayScalarString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
Display2dString(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdString(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayString(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptyString(io, A, name, currentNumericFormat, currentLineSpacing);
    } else if (A.isScalar()) {
        DisplayScalarString(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.is2D()) {
        Display2dString(evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdString(evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptyString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
}
//=============================================================================
void
DisplayScalarString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    std::wstring msg;
    if (elements[0].isCharacterArray()) {
        if (asDisp) {
            if (elements[0].isEmpty()) {
                // disp("") prints nothing, like disp('')
                return;
            }
            msg.append(elements[0].asWideString());
            msg.append(L"\n");
        } else {
            msg.append(BLANKS_AT_BOL);
            msg.append(L"\"");
            msg.append(elements[0].asWideString());
            msg.append(L"\"");
            msg.append(L"\n");
        }
    } else {
        msg.append(L"    <missing>\n");
    }
    io->outputMessage(msg);
}
//=============================================================================
void
Display2dString(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    if (A.isColumnVector()) {
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            if (!name.empty()) {
                io->outputMessage(L"\n");
            }
        }
        for (indexType k = 0; k < A.elementCount()
            && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
            ++k) {
            std::wstring msg;
            if (elements[k].isCharacterArray()) {
                msg = L"\"" + elements[k].asWideString() + L"\"";
            } else {
                msg = L"<missing>";
            }
            io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        }
    } else {
        indexType rows = A.rows();
        indexType columns = A.columns();
        indexType nbElements = A.elementCount();

        std::vector<size_t> vSize(columns, (size_t)0);
        wstringVector cellSummarize(nbElements, std::wstring(L""));
        sizeType termWidth = io->getTerminalWidth();
        size_t maxLength = getMaxElementLength(columns, termWidth);

        for (indexType k = 0; k < nbElements; ++k) {
            cellSummarize[k] = getAsFormattedString(elements, k, maxLength);
            vSize[k / rows] = std::max(vSize[k / rows], cellSummarize[k].length());
        }

        indexType colsPerPage = getColsPerPage(vSize, termWidth);

        indexType pageCount
            = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumnsHeader = (rows * columns > 1) && pageCount > 1;

        for (indexType k = 0;
            k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
            k++) {

            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;

            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                if (!name.empty() || k != 0) {
                    io->outputMessage(L"\n");
                }
            }
            if (withColumnsHeader) {
                std::wstring msg
                    = columnsHeader(k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    msg = msg + L"\n\n";
                } else {
                    msg = msg + L"\n";
                }
                io->outputMessage(msg);
            }
            for (indexType i = 0;
                i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    indexType colsPos = k * colsPerPage + j;
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    indexType len = vSize[colsPos];
                    std::wstring msg = completeWithBlanksAtTheEnd(cellSummarize[idx], len);
                    io->outputMessage(BLANKS_AT_BOL + msg);
                }
                io->outputMessage(L"\n");
            }
        }
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
DisplayNdString(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    sizeType termWidth = io->getTerminalWidth();
    Dimensions dims = A.dimensions();

    Dimensions wdims(dims.length());
    indexType rows(A.rows());
    indexType columns(A.columns());
    indexType offset = 0;
    size_t maxLength = getMaxElementLength(columns, termWidth);

    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    while (wdims.inside(dims)) {
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
            io->outputMessage(fmt::format(L",{}", static_cast<int>(wdims[m]) + 1));
        }
        io->outputMessage(L")" + displayHeaderEqualSign(A, true) + L"\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }

        std::vector<size_t> vSize(columns, (size_t)0);
        indexType nbElements = rows * columns;
        wstringVector cellSummarize(nbElements, L"");
        for (indexType k = 0; k < nbElements; ++k) {
            cellSummarize[k] = getAsFormattedString(elements, k + offset, maxLength);
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
            for (indexType i = 0; i < rows; i++) {
                for (indexType j = 0; j < colsInThisPage
                    && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                    j++) {
                    indexType colsPos = k * colsPerPage + j;
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    // a single column is not padded, like a column vector
                    indexType len = (columns == 1) ? 0 : vSize[colsPos];
                    std::wstring msg = completeWithBlanksAtTheEnd(cellSummarize[idx], len);
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
size_t
getMaxElementLength(indexType columns, indexType termWidth)
{
    // A single column is never truncated. Otherwise each element may use its
    // share of the terminal width, minus the leading blanks, the quotes and a
    // separator, but never less than MIN_ELEMENT_LENGTH characters.
    constexpr size_t MIN_ELEMENT_LENGTH = 19;
    const size_t ELEMENT_DECORATION_LENGTH = LENGTH_BLANKS_AT_BOL + 3;
    if (columns <= 1) {
        return 0;
    }
    size_t share = static_cast<size_t>(termWidth / columns);
    if (share < MIN_ELEMENT_LENGTH + ELEMENT_DECORATION_LENGTH) {
        return MIN_ELEMENT_LENGTH;
    }
    return share - ELEMENT_DECORATION_LENGTH;
}
//=============================================================================
std::wstring
getAsFormattedString(ArrayOf* elements, indexType idx, size_t maxLength)
{
    std::wstring msg;
    switch (elements[idx].dataClass()) {
    case NLS_CHAR: {
        if (elements[idx].isRowVector()) {
            msg = elements[idx].asWideString();
            if (maxLength > 0 && msg.length() > maxLength) {
                msg = msg.substr(0, maxLength - 1) + HORIZONTAL_ELLIPSIS;
            }
            msg = L"\"" + msg + L"\"";
        } else {
            if (elements[idx].isEmpty()) {
                msg = L"\"" + msg + L"\"";
            } else {
                msg = lightDescription(elements[idx], L"", L"");
            }
        }
    } break;
    default: {
        msg = L"<missing>";

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
