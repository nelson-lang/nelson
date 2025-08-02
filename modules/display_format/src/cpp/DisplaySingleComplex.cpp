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
#include "DisplaySingleComplex.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
#include "IEEEFP.hpp"
#include "FormatHelpers.hpp"
#include "FormatTemplateHelpers.hpp"
#include "ComputeFormatInfo.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BLANKS_INTEGER_AT_BOL L"   "
#define LENGTH_BLANKS_INTEGER_AT_BOL 3
//============================================================================
static void
DisplayEmptySingleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayScalarSingleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
Display2dSingleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdSingleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplaySingleComplex(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && !A.isEmpty()) {
        io->outputMessage(L"\n");
    }
    if (A.isEmpty()) {
        DisplayEmptySingleComplex(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.isScalar()) {
        DisplayScalarSingleComplex(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.is2D()) {
        Display2dSingleComplex(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdSingleComplex(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptySingleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
}
//=============================================================================
void
DisplayScalarSingleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    std::wstring msg;
    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
    formatInfo.trim = false;
    const single* ptrValue = (const single*)A.getDataPointer();
    if ((double)(ptrValue[1]) == 0.) {
        msg.append(formatScalarNumber((double)(ptrValue[0]), true, formatInfo));
    } else {
        msg.append(formatScalarComplexNumber(
            (double)(ptrValue[0]), (double)(ptrValue[1]), true, formatInfo));
    }
    msg.append(L"\n");
    io->outputMessage(msg);
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
Display2dSingleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    Dimensions dims = A.getDimensions();
    const single* pValues = (const single*)A.getDataPointer();

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
    indexType colsPerPage
        = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
    colsPerPage = (colsPerPage < 1) ? 1 : colsPerPage;

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
            if (k > 0) {
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
                indexType idx = i + (k * colsPerPage + j) * rows;
                single rvalue = pValues[2 * idx];
                single ivalue = pValues[2 * idx + 1];
                buffer.append(formatElementComplex(rvalue, ivalue, formatInfo));
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
DisplayNdSingleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
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
    const single* pValues = (const single*)A.getDataPointer();
    while (wdims.inside(dims)) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            break;
        }
        if (offset != 0) {
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                buffer.append(L"\n\n");
            }
        }
        buffer.append(name + L"(:,:");
        for (indexType m = 2; m < dims.getLength(); m++) {
            buffer.append(fmt::sprintf(L",%d", static_cast<int>(wdims[m]) + 1));
        }
        buffer.append(L") =\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            buffer.append(L"\n");
        }
        indexType colsPerPage
            = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
        int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0; k < pageCount && continueDisplay; k++) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
                continueDisplay = false;
                break;
            }
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
                buffer.append(msg);
            }
            for (indexType i = 0; i < rows; i++) {
                for (indexType j = 0; j < colsInThisPage && continueDisplay; j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows + offset;
                    single rvalue = pValues[2 * idx];
                    single ivalue = pValues[2 * idx + 1];
                    std::wstring valueAsString = formatElementComplex(rvalue, ivalue, formatInfo);
                    buffer.append(valueAsString);
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
