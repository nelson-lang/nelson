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
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "DisplayLogical.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FormatHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
DisplayEmptyLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayEmptySparseLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dSparseLogical(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayLogical(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        if (A.isSparse()) {
            DisplayEmptySparseLogical(io, A, name, currentNumericFormat, currentLineSpacing);
        } else {
            DisplayEmptyLogical(io, A, name, currentNumericFormat, currentLineSpacing);
        }
    } else if (A.isScalar() || A.is2D() || A.isRowVector()) {
        if (A.isSparse()) {
            Display2dSparseLogical(
                evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
        } else {
            Display2dLogical(
                evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
        }
    } else {
        DisplayNdLogical(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptyLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
}
//=============================================================================
void
Display2dLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    sizeType termWidth = io->getTerminalWidth();
    size_t lengthLogicalString;
    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
        lengthLogicalString = std::wstring(L"+").length();
    } else {
        lengthLogicalString = std::wstring(L"false").length();
    }
    indexType colsPerPage = static_cast<indexType>(
        floor((termWidth - 1) / (static_cast<single>(lengthLogicalString))));

    indexType rows = A.getRows();
    indexType columns = A.getColumns();

    indexType pageCount
        = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
    bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

    logical* data = (logical*)A.getDataPointer();
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
                    buffer.append(BLANKS_AT_BOL);
                    buffer.append(
                        completeWithBlanksAtBeginning(valueAsString, lengthLogicalString));
                }
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
DisplayNdLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
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
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }
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
                for (indexType j = 0; j < colsInThisPage
                     && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                     j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows + offset;
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
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
DisplayEmptySparseLogical(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
}
//=============================================================================
void
Display2dSparseLogical(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    indexType nbRows = A.getRows();
    indexType nbCols = A.getColumns();

    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }

    if (A.getNonzeros() == 0) {
        std::wstring format = _W("All zero sparse: %s");
        std::wstring msg = fmt::sprintf(format, A.getDimensions().toWideString());
        io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        return;
    }
    Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();

    std::wstring formatIndex = _W("(%lu,%lu)");
    std::wstring indexAsString = fmt::sprintf(formatIndex, (long long)nbRows, (long long)nbCols);
    size_t maxLenIndexString = indexAsString.length();

    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            break;
        }
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
             ++it) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
                break;
            }
            std::wstring asStr;
            if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                if (it.value()) {
                    asStr = L"+";
                } else {
                    asStr = L" ";
                }
            } else {
                if (it.value()) {
                    asStr = L"true";
                } else {
                    asStr = L"false";
                }
            }
            std::wstring indexAsString
                = fmt::sprintf(formatIndex, (long long)(it.row() + 1), (long long)(it.col() + 1));
            io->outputMessage(BLANKS_AT_BOL + centerText(indexAsString, maxLenIndexString)
                + BLANKS_BETWEEN + asStr + L"\n");
        }
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
