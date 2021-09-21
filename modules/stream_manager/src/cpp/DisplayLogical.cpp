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
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "DisplayLogical.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
DisplayEmptyLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplayLogical(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    bool withFooter = false;
    if (A.isEmpty()) {
        DisplayEmptyLogical(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.isScalar() || A.is2D() || A.isRowVector()) {
        Display2dLogical(io, A, name, currentNumericFormat, currentLineSpacing);
        if (A.isScalar() || A.isRowVector()) {
            withFooter = !name.empty();
        } else {
            withFooter = true;
        }
    } else {
        DisplayNdLogical(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = true;
    }
    if (withFooter) {
        DisplayVariableFooter(io, A, name);
    }
}
//=============================================================================
void
DisplayEmptyLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    if (A.isSparse()) {
    } else {
        // nothing to display
    }
}
//=============================================================================
static void
Display2dSparseLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }
    if (A.getNonzeros() == 0) {
        std::wstring format = L"%sAll zero sparse: %s\n";
        std::wstring msg = fmt::sprintf(format, BLANKS_AT_BOL, A.getDimensions().toWideString());
        io->outputMessage(msg);
    } else {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.getSparseDataPointer();
        std::wstring format;
        if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
            format = L"%s(%lu,%lu)      +\n";
        } else {
            format = L"%s(%lu,%lu)      true\n";
        }
        for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                break;
            }
            for (Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
                 ++it) {
                if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                    break;
                }
                if (it.value()) {
                    std::wstring msg = fmt::sprintf(format, BLANKS_AT_BOL,
                        (long long)(it.row() + 1), (long long)(it.col() + 1));
                    io->outputMessage(msg);
                }
            }
        }
    }
}
//=============================================================================
void
Display2dLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    if (A.isSparse()) {
        Display2dSparseLogical(io, A, name, currentNumericFormat, currentLineSpacing);
    } else {
        indexType rows = A.getRows();
        indexType columns = A.getColumns();
        indexType nbElements = A.getElementCount();

        sizeType termWidth = io->getTerminalWidth();
        size_t lengthLogicalString;
        if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
            lengthLogicalString = std::wstring(L"+").length();
        } else {
            lengthLogicalString = std::wstring(L"false").length();
        }
        indexType colsPerPage = static_cast<indexType>(
            floor((termWidth - 1) / (static_cast<single>(lengthLogicalString))));

        indexType pageCount
            = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

        logical* data = (logical*)A.getDataPointer();
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
                 i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(); i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    std::wstring valueAsString;
                    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                        if (data[idx]) {
                            valueAsString = L"+";
                        } else {
                            valueAsString = L" ";
                        }
                        io->outputMessage(valueAsString);
                    } else {
                        if (data[idx]) {
                            valueAsString = L"true";
                        } else {
                            valueAsString = L"false";
                        }
                        std::wstring msg
                            = completeWithBlanksAtBeginning(valueAsString, lengthLogicalString);
                        io->outputMessage(BLANKS_AT_BOL + msg);
                    }
                }
                io->outputMessage(L"\n");
            }
        }
    }
}
//=============================================================================
void
DisplayNdLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    sizeType termWidth = io->getTerminalWidth();
    Dimensions dims = A.getDimensions();

    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;

    size_t lengthLogicalString;
    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
        lengthLogicalString = std::wstring(L"+").length();
    } else {
        lengthLogicalString = std::wstring(L"false").length();
    }

    logical* data = (logical*)A.getDataPointer();
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
        indexType colsPerPage = static_cast<indexType>(
            floor((termWidth - 1) / (static_cast<single>(lengthLogicalString))));
        int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0; k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending();
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
                for (indexType j = 0; j < colsInThisPage
                     && !NelsonConfiguration::getInstance()->getInterruptPending();
                     j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    std::wstring valueAsString;
                    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                        if (data[idx]) {
                            valueAsString = L"+";
                        } else {
                            valueAsString = L" ";
                        }
                        io->outputMessage(valueAsString);
                    } else {
                        if (data[idx]) {
                            valueAsString = L"true";
                        } else {
                            valueAsString = L"false";
                        }
                        std::wstring msg
                            = completeWithBlanksAtBeginning(valueAsString, lengthLogicalString);
                        io->outputMessage(BLANKS_AT_BOL + msg);
                    }
                }
                io->outputMessage(L"\n");
            }
        }
        offset += rows * columns;
        wdims.incrementModulo(dims, 2);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
