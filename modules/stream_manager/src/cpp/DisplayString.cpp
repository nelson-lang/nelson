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
#include "DisplayString.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
getAsFormattedString(ArrayOf* elements, indexType idx, NumericFormatDisplay currentNumericFormat,
    indexType termWidth);
//=============================================================================
static indexType
getColsPerPage(const std::vector<size_t>& vSize, indexType termWidth);
//=============================================================================
static void
DisplayEmptyString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplayString(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    bool withFooter = false;
    if (A.isEmpty()) {
        DisplayEmptyString(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.isScalar() || A.is2D()) {
        Display2dString(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = true;
    } else {
        DisplayNdString(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = true;
    }
    if (withFooter) {
        DisplayVariableFooter(io, A, name);
    }
}
//=============================================================================
void
DisplayEmptyString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    // nothing to display
}
//=============================================================================
void
Display2dString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
    if (A.isColumnVector()) {
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
        for (indexType k = 0;
             k < A.getElementCount() && !NelsonConfiguration::getInstance()->getInterruptPending();
             ++k) {
            std::wstring msg;
            if (elements[k].isCharacterArray()) {
                msg = L"\"" + elements[k].getContentAsWideString() + L"\"";
            } else {
                msg = L"<missing>";
            }
            io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        }
    } else {
        indexType rows = A.getRows();
        indexType columns = A.getColumns();
        indexType nbElements = A.getElementCount();

        std::vector<size_t> vSize(columns, (size_t)0);
        wstringVector cellSummarize(nbElements, std::wstring(L""));
        sizeType termWidth = io->getTerminalWidth();

        indexType v = 0;
        for (indexType k = 0; k < nbElements; ++k) {
            if (v >= columns) {
                v = 0;
            }
            cellSummarize[k] = getAsFormattedString(elements, k, currentNumericFormat, termWidth);
            vSize[k / rows] = std::max(vSize[k / rows], cellSummarize[k].length());
        }

        indexType colsPerPage = getColsPerPage(vSize, termWidth);

        indexType pageCount
            = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

        for (indexType k = 0;
             k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {

            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;

            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                if (!name.empty() || k != 0) {
                    io->outputMessage(L"\n");
                }
            }
            if (withColumsHeader) {
                std::wstring msg = fmt::sprintf(_W("  Columns %d through %d").c_str(),
                    k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    msg = msg + L"\n\n";
                } else {
                    msg = msg + L"\n";
                }
                io->outputMessage(msg);
            }
            for (indexType i = 0;
                 i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(); i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    indexType colsPos = k * colsPerPage + j;
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    indexType len = vSize[colsPos];
                    std::wstring msg = completeWithBlanksAtBeginning(cellSummarize[idx], len);
                    io->outputMessage(BLANKS_AT_BOL + msg);
                }
                io->outputMessage(L"\n");
            }
        }
    }
}
//=============================================================================
void
DisplayNdString(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    sizeType termWidth = io->getTerminalWidth();
    Dimensions dims = A.getDimensions();

    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;

    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
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
        for (indexType k = 0; k < nbElements; ++k) {
            cellSummarize[k]
                = getAsFormattedString(elements, k + offset, currentNumericFormat, termWidth);
            vSize[k / rows] = std::max(vSize[k / rows], cellSummarize[k].length());
        }
        indexType colsPerPage = getColsPerPage(vSize, termWidth);
        int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0; k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending();
             k++) {
            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (withColumsHeader) {
                std::wstring msg = fmt::sprintf(_W("  Columns %d through %d"), k * colsPerPage + 1,
                    k * colsPerPage + colsInThisPage);
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
                     && !NelsonConfiguration::getInstance()->getInterruptPending();
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
}
//=============================================================================
std::wstring
getAsFormattedString(ArrayOf* elements, indexType idx, NumericFormatDisplay currentNumericFormat,
    indexType termWidth)
{
    std::wstring msg;
    switch (elements[idx].getDataClass()) {
    case NLS_CHAR: {
        if (elements[idx].isRowVector()) {
            msg = elements[idx].getContentAsWideString(16);
            if (msg.length() > 15) {
                msg.pop_back();
                msg = msg.substr(0, 15);
                msg = msg + HORIZONTAL_ELLIPSIS;
            }
            msg = L"\"" + msg + L"\"";
        } else {
            msg = lightDescription(elements[idx], L"", L"");
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
    return colsPerPage;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
