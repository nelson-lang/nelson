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
#include "i18n.hpp"
#include "DisplayDouble.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FormatHelpers.hpp"
#include "ArrayOfFormatInfo.hpp"
#include "ComputeFormatInfo.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
DisplayEmptySparseDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplaySparseDoubleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplaySparseDoubleComplexScalar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplaySparseDoubleComplex(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptySparseDoubleComplex(
            io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        if (A.getNonzeros() == 1) {
            DisplaySparseDoubleComplexScalar(
                io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
        } else {
            DisplaySparseDoubleComplex(
                evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
        }
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptySparseDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
}
//=============================================================================
void
DisplaySparseDoubleComplexScalar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)A.getSparseDataPointer();

    indexType r = 0;
    indexType c = 0;
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        for (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>::InnerIterator it(
                 *spMat, k);
             it; ++it) {
            r = it.row() + 1;
            c = it.col() + 1;
        }
    }
    std::wstring formatIndex = _W("(%lu,%lu)");
    std::wstring indexAsString = fmt::sprintf(formatIndex, (long long)r, (long long)c);
    size_t maxLenIndexString = indexAsString.length();

    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
    formatInfo.trim = false;
    const std::complex<double>* values = spMat->valuePtr();
    std::wstring asStr
        = formatScalarComplexNumber(values[0].real(), values[0].imag(), false, formatInfo);
    indexAsString = fmt::sprintf(formatIndex, (long long)r, (long long)c);
    std::wstring msg = BLANKS_AT_BOL + centerText(indexAsString, maxLenIndexString) + BLANKS_BETWEEN
        + asStr + L"\n";
    io->outputMessage(msg);
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
static void
DisplaySparseDoubleComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    const std::wstring& name, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    if (A.getNonzeros() == 0) {
        std::wstring format = _W("All zero sparse: %s");
        std::wstring msg = fmt::sprintf(format, A.getDimensions().toWideString());
        io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        return;
    }

    Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)A.getSparseDataPointer();

    indexType rMax = 0;
    indexType cMax = 0;
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        for (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>::InnerIterator it(
                 *spMat, k);
             it; ++it) {
            rMax = std::max((long long)rMax, (long long)it.row() + 1);
            cMax = std::max((long long)cMax, (long long)it.col() + 1);
        }
    }

    std::wstring formatIndex = _W("(%lu,%lu)");
    std::wstring indexAsString = fmt::sprintf(formatIndex, (long long)rMax, (long long)cMax);
    size_t maxLenIndexString = indexAsString.length();

    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);

    if (formatInfo.scaleFactor != 1) {
        std::wstring scaleFactorAsString = formatScaleFactor(formatInfo);
        io->outputMessage(BLANKS_AT_BOL + scaleFactorAsString + L"\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
    }

    bool continueDisplay = true;
    indexType block_page = 0;
    std::wstring buffer;

    for (indexType k = 0; k < (indexType)spMat->outerSize() && continueDisplay; ++k) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            continueDisplay = false;
            break;
        }
        for (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>::InnerIterator it(
                 *spMat, k);
             it && continueDisplay; ++it) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
                continueDisplay = false;
                break;
            }
            std::complex<double> value = it.value();
            std::wstring asStr = formatElementComplex(value.real(), value.imag(), formatInfo);

            std::wstring indexAsString
                = fmt::sprintf(formatIndex, (long long)(it.row() + 1), (long long)(it.col() + 1));
            buffer.append(BLANKS_AT_BOL);
            buffer.append(
                centerText(indexAsString, maxLenIndexString) + BLANKS_BETWEEN + asStr + L"\n");
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
} // namespace Nelson
//=============================================================================
