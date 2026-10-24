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
#include <type_traits>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "DisplayDouble.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "DisplaySparseHelpers.hpp"
#include "FormatHelpers.hpp"
#include "ArrayOfFormatInfo.hpp"
#include "ComputeFormatInfo.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static void
DisplaySparseComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
template <class T>
static void
DisplaySparseComplexScalar(Interface* io, const ArrayOf& A,
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
    if (!A.isEmpty() && DisplaySparseBeginning(io, A, currentLineSpacing, asDisp)) {
        bool asSingle = A.isSingleClass();
        if (A.nonzeros() == 1) {
            if (asSingle) {
                DisplaySparseComplexScalar<single>(
                    io, A, currentNumericFormat, currentLineSpacing, asDisp);
            } else {
                DisplaySparseComplexScalar<double>(
                    io, A, currentNumericFormat, currentLineSpacing, asDisp);
            }
        } else {
            if (asSingle) {
                DisplaySparseComplex<single>(
                    evaluatorID, io, A, currentNumericFormat, currentLineSpacing, asDisp);
            } else {
                DisplaySparseComplex<double>(
                    evaluatorID, io, A, currentNumericFormat, currentLineSpacing, asDisp);
            }
        }
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
template <class T>
void
DisplaySparseComplexScalar(Interface* io, const ArrayOf& A,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)A.sparseDataPointer();

    indexType r = 0;
    indexType c = 0;
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>::InnerIterator it(
                 *spMat, k);
            it; ++it) {
            r = it.row() + 1;
            c = it.col() + 1;
        }
    }
    FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
    formatInfo.trim = false;
    const std::complex<T>* values = spMat->valuePtr();
    std::wstring asStr = formatScalarComplexNumber((double)values[0].real(),
        (double)values[0].imag(), std::is_same<T, single>::value, formatInfo);
    io->outputMessage(sparseIndexText(A, r, c) + asStr + L"\n");
    DisplaySparseEnd(io, currentLineSpacing, asDisp);
}
//=============================================================================
template <class T>
void
DisplaySparseComplex(size_t evaluatorID, Interface* io, const ArrayOf& A,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)A.sparseDataPointer();

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

    for (indexType k = 0; k < (indexType)spMat->outerSize() && continueDisplay; ++k) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            continueDisplay = false;
            break;
        }
        for (typename Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>::InnerIterator it(
                 *spMat, k);
            it && continueDisplay; ++it) {
            if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
                continueDisplay = false;
                break;
            }
            std::complex<T> value = it.value();
            std::wstring asStr = formatElementComplex(value.real(), value.imag(), formatInfo);
            buffer.append(sparseIndexText(A, it.row() + 1, it.col() + 1) + asStr + L"\n");
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
    DisplaySparseEnd(io, currentLineSpacing, asDisp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
