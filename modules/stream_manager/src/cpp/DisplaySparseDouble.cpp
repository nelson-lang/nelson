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
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MIDDLE_MULTIPLY "\U000000D7"
#define BLANKS_INTEGER_AT_BOL L"   "
#define LENGTH_BLANKS_INTEGER_AT_BOL 3
#define LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_BANK 3
#define LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_HEX 6
#define LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_PLUS 3
#define LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_RATIONAL 6
#define x_wchar std::wstring(1, (wchar_t)215)

//============================================================================
static void
DisplayEmptySparseDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplaySparseDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
static void
DisplaySparseDoubleScalar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing);
//=============================================================================
void
DisplaySparseDouble(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name);
    bool withFooter = false;
    if (A.isEmpty()) {
        DisplayEmptySparseDouble(io, A, name, currentNumericFormat, currentLineSpacing);
        withFooter = !name.empty();
    } else {
        if (A.getNonzeros() == 1) {
            DisplaySparseDoubleScalar(io, A, name, currentNumericFormat, currentLineSpacing);
        } else {
            DisplaySparseDouble(io, A, name, currentNumericFormat, currentLineSpacing);
        }
        if (A.isScalar() || A.isRowVector()) {
            withFooter = !name.empty();
        } else {
            withFooter = true;
        }
    }
    if (withFooter) {
        DisplayVariableFooter(io, A, name);
    }
}
//=============================================================================
void
DisplayEmptySparseDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{ }
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
static bool
findFiniteMinMax(const ArrayOf& A, double& minValue, double& maxValue)
{
    Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
    const double* data = spMat->valuePtr();
    return getFiniteMinMax<double>(data, (indexType)spMat->nonZeros(), minValue, maxValue);
}
//=============================================================================
static int
getOptionalCommonLogarithm(
    double minValue, double maxValue, NumericFormatDisplay currentNumericFormat)
{
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
DisplaySparseDoubleScalar(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    indexType nbRows = A.getRows();
    indexType nbCols = A.getColumns();

    Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();

    indexType r = 0;
    indexType c = 0;
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
             ++it) {
            r = it.row() + 1;
            c = it.col() + 1;
        }
    }
    std::wstring formatIndex = _W("(%lu,%lu)");
    std::wstring indexAsString = fmt::sprintf(formatIndex, (long long)r, (long long)c);
    size_t maxLenIndexString = indexAsString.length();

    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
        maxLenIndexString = maxLenIndexString + 3;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_HEX;
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_PLUS;
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_RATIONAL;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_BANK;
    } break;

    default: {
    } break;
    }

    const double* values = spMat->valuePtr();
    double value = values[0];

    std::wstring asStr;
    if (IsIntegerFormOrNotFinite(values, 1)) {
        asStr = outputDoublePrecisionAsIntegerForm(value, currentNumericFormat, false);
    } else {
        asStr = outputDoublePrecisionFloat(value, currentNumericFormat, true, false);
    }
    indexAsString = fmt::sprintf(formatIndex, (long long)r, (long long)c);
    std::wstring blanks(maxLenIndexString - indexAsString.length(), L' ');
    std::wstring format = L"%s%s%s";
    std::wstring msg = fmt::sprintf(format, indexAsString, blanks, asStr);
    io->outputMessage(L"    " + msg + L"\n");
}
//=============================================================================
static void
DisplaySparseDouble(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing)
{
    indexType nbRows = A.getRows();
    indexType nbCols = A.getColumns();

    if (A.getNonzeros() == 0) {
        std::wstring format = _W("All zero sparse: %lu\U000000D7%lu");
        std::wstring msg = fmt::sprintf(format, (long long)nbRows, (long long)nbCols);
        io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        return;
    }

    Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();

    indexType rMax = 0;
    indexType cMax = 0;
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
             ++it) {
            rMax = std::max((long long)rMax, (long long)it.row() + 1);
            cMax = std::max((long long)cMax, (long long)it.col() + 1);
        }
    }

    std::wstring formatIndex = _W("(%lu,%lu)");
    std::wstring indexAsString = fmt::sprintf(formatIndex, (long long)rMax, (long long)cMax);
    size_t maxLenIndexString = indexAsString.length();

    double minValue;
    double maxValue;
    bool allInteger = false;
    int commonLogarithm = 0;
    if (findFiniteMinMax(A, minValue, maxValue)) {
        const double* data = spMat->valuePtr();
        allInteger = IsIntegerFormOrNotFinite(data, (indexType)spMat->nonZeros());
        if (allInteger) {
            std::wstring minAsStr
                = outputDoublePrecisionAsIntegerForm(minValue, currentNumericFormat, true);
            std::wstring maxAsStr
                = outputDoublePrecisionAsIntegerForm(maxValue, currentNumericFormat, true);
        } else {
            commonLogarithm = getOptionalCommonLogarithm(minValue, maxValue, currentNumericFormat);
        }
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
        io->outputMessage(L"    " + str);
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
    }

    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
        maxLenIndexString = maxLenIndexString + 3;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_HEX;
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_PLUS;
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_RATIONAL;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        maxLenIndexString = maxLenIndexString + LENGTH_BLANKS_BETWEEN_INDEX_AND_NUMBER_BANK;
    } break;

    default: {
    } break;
    }

    std::wstring format = L"    %s%s%s\n";

    bool continueDisplay = true;
    indexType block_page = 0;
    std::wstring buffer;

    for (indexType k = 0; k < (indexType)spMat->outerSize() && continueDisplay; ++k) {
        if (NelsonConfiguration::getInstance()->getInterruptPending()) {
            continueDisplay = false;
            break;
        }
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spMat, k);
             it && continueDisplay;
             ++it) {
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                continueDisplay = false;
                break;
            }
            double value = it.value();
            if (commonLogarithm != 0) {
                value = value / pow(10, commonLogarithm);
            }
            std::wstring asStr;
            asStr.reserve(20);
            if (allInteger) {
                asStr = outputDoublePrecisionAsIntegerForm(value, currentNumericFormat, false);
            } else {
                asStr = outputDoublePrecisionFloat(value, currentNumericFormat, false, false);
            }
            std::wstring indexAsString
                = fmt::sprintf(formatIndex, (long long)(it.row() + 1), (long long)(it.col() + 1));
            std::wstring blanks(maxLenIndexString - indexAsString.length(), L' ');
            std::wstring msg = fmt::sprintf(format, indexAsString, blanks, asStr);
            buffer.append(msg);
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
} // namespace Nelson
//=============================================================================
