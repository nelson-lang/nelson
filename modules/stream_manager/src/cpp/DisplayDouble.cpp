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
//=============================================================================
namespace Nelson {
//=============================================================================
#define BLANKS_INTEGER_AT_BOL L"   "
#define LENGTH_BLANKS_INTEGER_AT_BOL 3
//============================================================================
static void
DisplayEmptyDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayScalarDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
Display2dDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplayNdDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static indexType
getDoubleNominalWidth(const ArrayOf& A, NumericFormatDisplay currentNumericFormat);
//=============================================================================
void
DisplayDouble(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    bool withFooter = false;
    if (A.isEmpty()) {
        DisplayEmptyDouble(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.isScalar()) {
        DisplayScalarDouble(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else if (A.is2D() || A.isRowVector()) {
        Display2dDouble(io, A, name, currentNumericFormat, currentLineSpacing);
        if (A.isRowVector()) {
            withFooter = !name.empty();
        } else {
            withFooter = true;
        }
    } else {
        DisplayNdDouble(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = true;
    }
    if (withFooter) {
        DisplayVariableFooter(io, A, name);
    }
}
//=============================================================================
void
DisplayEmptyDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{ }
//=============================================================================
void
DisplayScalarDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    const double* pValue = (const double*)A.getDataPointer();
    std::wstring msg;
    if (IsIntegerForm(pValue[0])) {
        msg = outputDoublePrecisionAsIntegerForm(pValue[0], currentNumericFormat, false);
    } else {
        msg = outputDoublePrecisionFloat(pValue[0], currentNumericFormat, false, false);
    }
    io->outputMessage(msg + L"\n");
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
static int
getOptionalCommonLogarithm(
    double minValue, double maxValue, NumericFormatDisplay currentNumericFormat)
{
    if (minValue == maxValue) {
        return 0;
    }
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_LONG: {
        int commonLogarithm = log10(std::max(minValue, maxValue));
        if (commonLogarithm == 1) {
            return 0;
        }
        if (commonLogarithm < -2 || commonLogarithm >= 2) {
            return commonLogarithm;
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORT: {
        int commonLogarithm = log10(std::max(minValue, maxValue));
        if (commonLogarithm == 1) {
            return 0;
        }
        if (commonLogarithm < -2 || commonLogarithm > 2) {
            return commonLogarithm;
        }
    } break;
    default: {
    }
    }
    return 0;
}
//=============================================================================
void
Display2dDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    Dimensions dims = A.getDimensions();
    const double* pValues = (const double*)A.getDataPointer();
    indexType rows = dims.getRows();
    indexType columns = dims.getColumns();
    indexType nominalWidth = getDoubleNominalWidth(A, currentNumericFormat);

    bool allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount());
    double minValue = 0;
    double maxValue = 0;
    getFiniteMinMax<double>(pValues, A.getElementCount(), minValue, maxValue);

    std::wstring minStr;
    std::wstring maxStr;
    int commonLogarithm = 0;
    if (allInteger) {
        minStr = outputDoublePrecisionAsIntegerForm(minValue, currentNumericFormat, false);
        maxStr = outputDoublePrecisionAsIntegerForm(maxValue, currentNumericFormat, false);
    } else {
        commonLogarithm = getOptionalCommonLogarithm(minValue, maxValue, currentNumericFormat);
        if (commonLogarithm != 0) {
            minValue = minValue / pow(10, commonLogarithm);
            maxValue = maxValue / pow(10, commonLogarithm);
        }
        minStr = outputDoublePrecisionFloat(minValue, currentNumericFormat, false, false);
        maxStr = outputDoublePrecisionFloat(maxValue, currentNumericFormat, false, false);
    }
    if (commonLogarithm != 0) {
        int absCommonLogarithm = abs(commonLogarithm);
        std::wstring sign;
        if (commonLogarithm > 0) {
            sign = L"+";
        } else {
            sign = L"-";
        }
        std::wstring fmt;
        std::wstring str;
        if (absCommonLogarithm < 10) {
            fmt = L"1.0e%s0%d *\n";
            str = fmt::sprintf(fmt, sign, absCommonLogarithm);
        } else {
            fmt = L"1.0e%s%d *\n";
            str = fmt::sprintf(fmt, sign, absCommonLogarithm);
        }
        io->outputMessage(L"   " + str);
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
    }

    nominalWidth = std::max(minStr.length(), maxStr.length());

    bool forceFormat = false;

    sizeType termWidth = io->getTerminalWidth();
    indexType colsPerPage
        = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
    indexType pageCount
        = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
    bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
    std::wstring lineBuffer;
    for (indexType k = 0;
         k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {

        indexType colsInThisPage = columns - colsPerPage * k;
        colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;

        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            if (k > 0) {
                io->outputMessage(L"\n");
            }
        }
        if (withColumsHeader) {
            std::wstring msg = columnsHeader(k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
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
                std::wstring msg;
                msg.reserve(nominalWidth);
                if (allInteger) {
                    msg = outputDoublePrecisionAsIntegerForm(
                        pValues[idx], currentNumericFormat, false);
                } else {
                    double value;
                    if (commonLogarithm != 0) {
                        value = pValues[idx] / pow(10, commonLogarithm);
                    } else {
                        value = pValues[idx];
                    }
                    msg = outputDoublePrecisionFloat(
                        value, currentNumericFormat, forceFormat, false);
                }
                lineBuffer.append(msg);
            }
            lineBuffer.append(L"\n");
            io->outputMessage(lineBuffer);
            lineBuffer.clear();
            lineBuffer.reserve(colsInThisPage * nominalWidth);
        }
    }
}
//=============================================================================
void
DisplayNdDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{ }
//=============================================================================
indexType
getDoubleNominalWidth(const ArrayOf& A, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 10;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTENG:
    case NLS_NUMERIC_FORMAT_LONGENG:
    case NLS_NUMERIC_FORMAT_PLUS:
    case NLS_NUMERIC_FORMAT_BANK:
    case NLS_NUMERIC_FORMAT_HEX:
    case NLS_NUMERIC_FORMAT_RATIONAL: {
    } break;
    default: {
    } break;
    }
    return nominalWidth;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
