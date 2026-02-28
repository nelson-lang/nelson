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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "StringHelpers.hpp"
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
Display2dChar(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdChar(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplayChar(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptyChar(io, A, name, currentNumericFormat, currentLineSpacing);
    } else if (A.isScalar() || A.is2D() || A.isRowVector()) {
        Display2dChar(evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing);
    } else {
        DisplayNdChar(evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing);
    }
    DisplayVariableFooter(io, asDisp);
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
Display2dChar(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    if (A.isRowVectorCharacterArray()) {
        std::wstring msg = A.getContentAsWideString();
        if (msg.empty()) {
            if (name.empty()) {
                io->outputMessage(L"");
            } else {
                io->outputMessage(std::wstring(BLANKS_AT_BOL) + L"''\n");
            }
        } else {
            if (name.empty()) {
                io->outputMessage(msg + L"\n");
            } else {
                io->outputMessage(std::wstring(BLANKS_AT_BOL) + L"\'" + msg + L"\'\n");
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
             i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
             i++) {
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
DisplayNdChar(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    Dimensions dims = A.getDimensions();

    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;

    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }
    while (wdims.inside(dims)) {
        if (offset != 0) {
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
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
            for (indexType i = 0; i < rows; i++) {
                std::wstring buffer(colsInThisPage, L' ');
                for (indexType j = 0; j < colsInThisPage
                     && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                     j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows + offset;
                    buffer[j] = raw[idx];
                }
                if (name.empty()) {
                    io->outputMessage(buffer + L"\n");
                } else {
                    buffer = L"'" + buffer + L"'";
                    io->outputMessage(L"    " + buffer + L"\n");
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
