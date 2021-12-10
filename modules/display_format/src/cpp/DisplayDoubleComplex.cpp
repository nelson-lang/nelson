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
#include "DisplayDouble.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
#include "IEEEFP.hpp"
#include "FormatHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BLANKS_INTEGER_AT_BOL L"   "
#define LENGTH_BLANKS_INTEGER_AT_BOL 3
//============================================================================
static void
DisplayEmptyDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayScalarDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplayDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    bool withFooter = false;
    if (A.isEmpty()) {
        DisplayEmptyDoubleComplex(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.isScalar()) {
        DisplayScalarDoubleComplex(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.is2D() || A.isRowVector()) {
        Display2dDoubleComplex(io, A, name, currentNumericFormat, currentLineSpacing);
        if (A.isRowVector()) {
            withFooter = !name.empty();
        } else {
            withFooter = true;
        }
    } else {
        DisplayNdDoubleComplex(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = true;
    }
    if (withFooter) {
        DisplayVariableFooter(io, A, name);
    }
}
//=============================================================================
void
DisplayEmptyDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{}
//=============================================================================
void
DisplayScalarDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    std::wstring msg;
    const double* pValue = (const double*)A.getDataPointer();
    msg.append(formatNumberComplex(pValue[0], pValue[1], currentNumericFormat, false));
    msg.append(L"\n");
    io->outputMessage(msg);
}
//=============================================================================
template <class T>
bool
getFiniteMinMax(const T* val, indexType nbElements, T& min, T& max)
{
    T minValue = std::nan("NaN");
    T maxValue = std::nan("NaN");
    T shared_max = std::nan("NaN");
    T shared_min = std::nan("NaN");

    for (indexType k = 0; k < nbElements; ++k) {
        if (std::isfinite(val[k])) {
            minValue = val[k];
            maxValue = val[k];
            shared_max = maxValue;
            shared_min = minValue;
            break;
        }
    }
    if (std::isnan(shared_min) && std::isnan(shared_max)) {
        min = shared_min;
        max = shared_max;
        return false;
    }
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            if (std::isfinite(val[idx])) {
                maxValue = std::max(val[idx], maxValue);
                minValue = std::min(val[idx], minValue);
            }
        }
#pragma omp critical
        {
            shared_max = std::max(shared_max, maxValue);
            shared_min = std::min(shared_min, minValue);
        }
    }
    min = shared_min;
    max = shared_max;
    return true;
}
//=============================================================================
void
Display2dDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
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

    indexType nominalWidth = formatInfo.widthReal;
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
        if (NelsonConfiguration::getInstance()->getInterruptPending()) {
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
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                continueDisplay = false;
                break;
            }
            for (indexType j = 0; j < colsInThisPage; j++) {
                indexType idx = i + (k * colsPerPage + j) * rows;
                double rvalue = pValues[2 * idx];
                double ivalue = pValues[2 * idx + 1];
                buffer.append(
                    formatElementComplex(rvalue, ivalue, currentNumericFormat, formatInfo));
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
}
//=============================================================================
void
DisplayNdDoubleComplex(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{}
//=============================================================================
} // namespace Nelson
//=============================================================================
