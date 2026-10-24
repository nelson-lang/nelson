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
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "DisplayLogical.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "DisplaySparseHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Nelson displays a logical value as the word true or false (not 1 and 0),
// right-aligned on the width of "false"; format + shows one character.
//=============================================================================
static size_t
logicalValueWidth(NumericFormatDisplay currentNumericFormat);
//=============================================================================
static std::wstring
logicalValueText(logical value, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static void
Display2dLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
Display2dSparseLogical(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
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
        // nothing after the header
    } else if (A.isSparse()) {
        Display2dSparseLogical(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.isScalar() || A.is2D() || A.isRowVector()) {
        Display2dLogical(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdLogical(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
size_t
logicalValueWidth(NumericFormatDisplay currentNumericFormat)
{
    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
        return std::wstring(L"+").length();
    }
    return LENGTH_BLANKS_AT_BOL + std::wstring(L"false").length();
}
//=============================================================================
std::wstring
logicalValueText(logical value, NumericFormatDisplay currentNumericFormat)
{
    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
        return value ? L"+" : L" ";
    }
    return BLANKS_AT_BOL
        + completeWithBlanksAtBeginning(
            value ? L"true" : L"false", std::wstring(L"false").length());
}
//=============================================================================
void
Display2dLogical(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    sizeType termWidth = io->getTerminalWidth();
    size_t valueWidth = logicalValueWidth(currentNumericFormat);
    indexType colsPerPage
        = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(valueWidth))));
    if (colsPerPage < 1) {
        colsPerPage = 1;
    }

    indexType rows = A.rows();
    indexType columns = A.columns();

    indexType pageCount
        = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
    bool withColumnsHeader = (rows * columns > 1) && pageCount > 1;

    const logical* data = (const logical*)A.dataPointer();
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
                indexType idx = i + (k * colsPerPage + j) * rows;
                buffer.append(logicalValueText(data[idx], currentNumericFormat));
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
    Dimensions dims = A.dimensions();

    Dimensions wdims(dims.length());
    indexType rows(A.rows());
    indexType columns(A.columns());
    indexType offset = 0;

    size_t valueWidth = logicalValueWidth(currentNumericFormat);
    indexType colsPerPage
        = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(valueWidth))));
    if (colsPerPage < 1) {
        colsPerPage = 1;
    }
    int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
    bool withColumnsHeader = (rows * columns > 1) && pageCount > 1;

    const logical* data = (const logical*)A.dataPointer();
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
        for (indexType m = 2; m < dims.length(); m++) {
            io->outputMessage(fmt::format(",{0:d}", static_cast<int>(wdims[m]) + 1));
        }
        io->outputMessage(L") =\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
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
                std::wstring line;
                for (indexType j = 0; j < colsInThisPage
                    && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                    j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows + offset;
                    line.append(logicalValueText(data[idx], currentNumericFormat));
                }
                io->outputMessage(line + L"\n");
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
Display2dSparseLogical(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    if (!DisplaySparseBeginning(io, A, currentLineSpacing, asDisp)) {
        return;
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.sparseDataPointer();
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            break;
        }
        for (Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
            ++it) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
                break;
            }
            std::wstring asStr;
            if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                asStr = it.value() ? L"+" : L" ";
            } else {
                asStr = it.value() ? L"true" : L"false";
            }
            io->outputMessage(sparseIndexText(A, it.row() + 1, it.col() + 1) + asStr + L"\n");
        }
    }
    DisplaySparseEnd(io, currentLineSpacing, asDisp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
