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
#include "DisplayDoubleComplex.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
#include "IEEEFP.hpp"
#include "FormatHelpers.hpp"
#include "ComputeFormatInfo.hpp"
#include "FormatTemplateHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BLANKS_INTEGER_AT_BOL L"   "
#define LENGTH_BLANKS_INTEGER_AT_BOL 3
//============================================================================
static void
DisplayEmptyDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayScalarDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
Display2dDoubleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdDoubleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayDoubleComplex(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptyDoubleComplex(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.isScalar()) {
        DisplayScalarDoubleComplex(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.is2D() || A.isRowVector()) {
        Display2dDoubleComplex(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdDoubleComplex(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptyDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
}
//=============================================================================
void
DisplayScalarDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    std::wstring msg;
    const double* ptrValue = (const double*)A.getDataPointer();
    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
    formatInfo.trim = false;
    msg.append(formatScalarComplexNumber(ptrValue[0], ptrValue[1], false, formatInfo));
    msg.append(L"\n");
    io->outputMessage(msg);
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
Display2dDoubleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    Dimensions dims = A.getDimensions();
    const double* pValues = (const double*)A.getDataPointer();

    indexType rows = dims.getRows();
    indexType columns = dims.getColumns();

    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);

    if (formatInfo.scaleFactor != 1) {
        std::wstring scaleFactorAsString = formatScaleFactor(formatInfo);
        io->outputMessage(L"   " + scaleFactorAsString + L"\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
    }
    indexType nominalWidth = getNominalWidth(formatInfo);
    sizeType termWidth = io->getTerminalWidth();
    indexType colsPerPage = (nominalWidth > 0)
        ? static_cast<indexType>(
              std::max<single>(1.0f, floor((termWidth - 1) / (static_cast<single>(nominalWidth)))))
        : 1;

    indexType pageCount = (colsPerPage > 0)
        ? static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))))
        : 1;
    bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
    bool continueDisplay = true;
    indexType block_page = 0;
    std::wstring buffer;
    buffer.reserve(static_cast<size_t>(rows)
            * static_cast<size_t>(std::max<indexType>(1, colsPerPage))
            * static_cast<size_t>(std::max<indexType>(1, nominalWidth))
        + 128);
    auto config = NelsonConfiguration::getInstance();
    sizeType termHeight = io->getTerminalHeight();
    bool isLoose = (currentLineSpacing == NLS_LINE_SPACING_LOOSE);
    for (indexType k = 0; k < pageCount && continueDisplay; k++) {
        if (config->getInterruptPending(evaluatorID)) {
            continueDisplay = false;
            break;
        }
        indexType colsInThisPage = columns - colsPerPage * k;
        colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;

        if (isLoose) {
            if (k > 0) {
                buffer.append(L"\n");
            }
        }
        if (withColumsHeader) {
            buffer.append(columnsHeader(k * colsPerPage + 1, k * colsPerPage + colsInThisPage));
            buffer.append(isLoose ? L"\n\n" : L"\n");
        }

        for (indexType i = 0; i < rows && continueDisplay; i++) {
            if (config->getInterruptPending(evaluatorID)) {
                continueDisplay = false;
                break;
            }
            for (indexType j = 0; j < colsInThisPage; j++) {
                indexType idx = i + (k * colsPerPage + j) * rows;
                double rvalue = pValues[2 * idx];
                double ivalue = pValues[2 * idx + 1];
                buffer.append(formatElementComplex(rvalue, ivalue, formatInfo));
            }
            buffer.append(L"\n");
            if (block_page >= termHeight) {
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
DisplayNdDoubleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    sizeType termWidth = io->getTerminalWidth();
    Dimensions dims = A.getDimensions();

    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;

    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);

    if (formatInfo.scaleFactor != 1) {
        std::wstring scaleFactorAsString = formatScaleFactor(formatInfo);
        io->outputMessage(L"   " + scaleFactorAsString + L"\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
    }

    bool continueDisplay = true;
    indexType block_page = 0;
    std::wstring buffer;

    indexType nominalWidth = getNominalWidth(formatInfo);
    const double* pValues = (const double*)A.getDataPointer();
    auto config = NelsonConfiguration::getInstance();
    sizeType termHeight = io->getTerminalHeight();
    bool isLoose = (currentLineSpacing == NLS_LINE_SPACING_LOOSE);
    buffer.reserve(static_cast<size_t>(rows) * static_cast<size_t>(std::max<indexType>(1, columns))
            * static_cast<size_t>(std::max<indexType>(1, nominalWidth)) / 4
        + 128);
    while (wdims.inside(dims)) {
        if (config->getInterruptPending(evaluatorID)) {
            break;
        }
        if (offset != 0) {
            if (isLoose) {
                buffer.append(L"\n\n");
            }
        }
        buffer.append(name + L"(:,:");
        for (indexType m = 2; m < dims.getLength(); m++) {
            buffer.append(fmt::format(L",{0:d}", static_cast<int>(wdims[m]) + 1));
        }
        buffer.append(L") =\n");
        if (isLoose) {
            buffer.append(L"\n");
        }
        indexType colsPerPage = (nominalWidth > 0)
            ? static_cast<indexType>(std::max<single>(
                  1.0f, floor((termWidth - 1) / (static_cast<single>(nominalWidth)))))
            : 1;
        int pageCount = (colsPerPage > 0)
            ? static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))))
            : 1;
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0; k < pageCount && continueDisplay; k++) {
            if (config->getInterruptPending(evaluatorID)) {
                continueDisplay = false;
                break;
            }
            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (withColumsHeader) {
                std::wstring msg
                    = columnsHeader(k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (isLoose) {
                    if (k == 0) {
                        msg = msg + L"\n\n";
                    } else {
                        msg = L"\n" + msg + L"\n\n";
                    }
                } else {
                    msg = msg + L"\n";
                }
                buffer.append(msg);
            }
            for (indexType i = 0; i < rows; i++) {
                for (indexType j = 0; j < colsInThisPage && continueDisplay; j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows + offset;
                    double rvalue = pValues[2 * idx];
                    double ivalue = pValues[2 * idx + 1];
                    buffer.append(formatElementComplex(rvalue, ivalue, formatInfo));
                }
                buffer.append(L"\n");
                if (block_page >= termHeight) {
                    io->outputMessage(buffer);
                    buffer.clear();
                    block_page = 0;
                } else {
                    block_page++;
                }
            }
        }
        offset += rows * columns;
        wdims.incrementModulo(dims, 2);
    }
    if (!buffer.empty()) {
        io->outputMessage(buffer);
        buffer.clear();
        block_page = 0;
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
