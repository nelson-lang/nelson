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
static void
DisplayEmptyChar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dChar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdChar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplayChar(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    bool withFooter = false;
    if (A.isEmpty()) {
        DisplayEmptyChar(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.isScalar() || A.is2D() || A.isRowVector()) {
        Display2dChar(io, A, name, currentNumericFormat, currentLineSpacing);
        if (A.isScalar() || A.isRowVector()) {
            withFooter = !name.empty();
        } else {
            withFooter = true;
        }
    } else {
        DisplayNdChar(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = true;
    }
    if (withFooter) {
        DisplayVariableFooter(io, A, name);
    }
}
//=============================================================================
void
DisplayEmptyChar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    // nothing to display
}
//=============================================================================
void
Display2dChar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    if (A.isRowVectorCharacterArray()) {
        std::wstring msg = A.getContentAsWideString();
        if (msg.empty()) {
            if (name.empty()) {
                io->outputMessage(L"");
            } else {
                io->outputMessage(L"''\n");
            }
        } else {
            if (name.empty()) {
                io->outputMessage(msg + L"\n");
            } else {
                io->outputMessage(L"\'" + msg + L"\'\n");
            }
        }
    } else {
        indexType rows = A.getRows();
        indexType columns = A.getColumns();
        charType* raw = (charType*)A.getDataPointer();
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            if (!name.empty()) {
                io->outputMessage(L"\n");
            }
        }
        for (indexType i = 0;
             i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(); i++) {
            std::wstring buffer(columns, L' ');
            for (indexType j = 0; j < columns; j++) {
                indexType idx = i + (j * rows);
                buffer[j] = raw[idx];
            }
            if (name.empty()) {
                io->outputMessage(buffer + L"\n");
            } else {
                buffer = L"'" + buffer + L"'";
                io->outputMessage(BLANKS_AT_BOL + buffer + L"\n");
            }
        }
    }
}
//=============================================================================
void
DisplayNdChar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{ 
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
        charType* raw = (charType*)A.getDataPointer();
        indexType colsPerPage = columns;
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
                std::wstring buffer(colsInThisPage, L' ');
                for (indexType j = 0; j < colsInThisPage
                     && !NelsonConfiguration::getInstance()->getInterruptPending();
                     j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows + offset;
                    buffer[j] = raw[idx];
                }
                if (name.empty()) {
                    io->outputMessage(buffer + L"\n");
                } else {
                    buffer = L"'" + buffer + L"'";
                    io->outputMessage(BLANKS_AT_BOL + buffer + L"\n");
                }
            }
        }
        offset += rows * columns;
        wdims.incrementModulo(dims, 2);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
