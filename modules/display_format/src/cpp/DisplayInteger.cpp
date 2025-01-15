//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "DisplayInteger.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "Hexify.hpp"
#include "DisplayIntegerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BLANKS_INTEGER_AT_BOL L"   "
#define LENGTH_BLANKS_INTEGER_AT_BOL 3
//=============================================================================
template <class T>
void
getMinMax(T* val, indexType nbElements, T* min, T* max)
{
    T minValue = val[0];
    T maxValue = val[0];
    T shared_max = maxValue;
    T shared_min = minValue;

#if WITH_OPENMP
#pragma omp parallel
#endif
    {
#if WITH_OPENMP
#pragma omp for nowait
#endif
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
            minValue = std::max(val[idx], minValue);
        }
#if WITH_OPENMP
#pragma omp critical
#endif
        {
            shared_max = std::max(shared_max, maxValue);
            shared_min = std::max(shared_min, minValue);
        }
    }
    *min = shared_min;
    *max = shared_max;
}
//=============================================================================
template <class T>
T
getMax(T* val, indexType nbElements)
{
    T maxValue = val[0];
    T shared_max = maxValue;
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
        }
#pragma omp critical
        {
            shared_max = std::max(shared_max, maxValue);
        }
    }
    return shared_max;
}
//=============================================================================
template <typename T>
indexType
getSignedIntegerNominalWidth(T* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 0;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_PLUS: {
        nominalWidth = 1;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        nominalWidth = (sizeof(T) << 1);
    } break;
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTENG:
    case NLS_NUMERIC_FORMAT_LONGENG:
    case NLS_NUMERIC_FORMAT_BANK:
    case NLS_NUMERIC_FORMAT_RATIONAL:
    default: {
        T maxValue;
        T minValue;
        getMinMax<T>(dp, nbElements, &minValue, &maxValue);
        std::wstring maxStr = fmt::to_wstring(maxValue);
        std::wstring minStr = fmt::to_wstring(minValue);
        nominalWidth = std::max(minStr.length(), maxStr.length());
    } break;
    }
    return nominalWidth;
}
//=============================================================================
template <typename T>
indexType
getUnsignedIntegerNominalWidth(
    T* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 0;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_PLUS: {
        nominalWidth = 1;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        nominalWidth = (sizeof(T) << 1);
    } break;
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTENG:
    case NLS_NUMERIC_FORMAT_LONGENG:
    case NLS_NUMERIC_FORMAT_BANK:
    case NLS_NUMERIC_FORMAT_RATIONAL:
    default: {
        T maxValue = getMax<T>(dp, nbElements);
        std::wstring maxStr = fmt::to_wstring(maxValue);
        nominalWidth = maxStr.length();
    } break;
    }
    return nominalWidth;
}
//=============================================================================
static indexType
getIntegerNominalWidth(const ArrayOf& A, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static void
DisplayEmptyInteger(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
Display2dInteger(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
static void
DisplayNdInteger(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplayInteger(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp)
{
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    DisplayVariableHeader(io, A, name, asDisp);
    if (A.isEmpty()) {
        DisplayEmptyInteger(io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else if (A.isScalar() || A.is2D() || A.isRowVector()) {
        Display2dInteger(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    } else {
        DisplayNdInteger(
            evaluatorID, io, A, name, currentNumericFormat, currentLineSpacing, asDisp);
    }
    DisplayVariableFooter(io, asDisp);
}
//=============================================================================
void
DisplayEmptyInteger(Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
}
//=============================================================================
void
Display2dInteger(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    Dimensions dims = A.getDimensions();
    const void* ap = A.getDataPointer();
    indexType rows = dims.getRows();
    indexType columns = dims.getColumns();
    indexType nominalWidth = getIntegerNominalWidth(A, currentNumericFormat);
    NelsonType classA = A.getDataClass();
    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage("\n");
        }
        for (indexType i = 0; i < rows; i++) {
            for (indexType j = 0; j < columns; j++) {
                std::wstring msg = formatInteger(ap, classA, i + (rows * j), currentNumericFormat);
                io->outputMessage(msg);
            }
            io->outputMessage("\n");
        }
    } else {
        sizeType termWidth = io->getTerminalWidth();
        indexType colsPerPage = static_cast<indexType>(floor(
            (termWidth - 1) / (static_cast<single>(LENGTH_BLANKS_INTEGER_AT_BOL + nominalWidth))));
        indexType pageCount
            = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (indexType k = 0;
             k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
             k++) {

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
                 i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID);
                 i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    std::wstring valueAsString
                        = formatInteger(ap, classA, idx, currentNumericFormat);
                    io->outputMessage(BLANKS_INTEGER_AT_BOL);
                    io->outputMessage(valueAsString);
                }
                io->outputMessage(L"\n");
            }
        }
    }
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
void
DisplayNdInteger(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    sizeType termWidth = io->getTerminalWidth();
    Dimensions dims = A.getDimensions();
    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;
    NelsonType classA = A.getDataClass();
    indexType nominalWidth = getIntegerNominalWidth(A, currentNumericFormat);
    const void* ap = A.getDataPointer();
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
        indexType colsPerPage = static_cast<indexType>(floor(
            (termWidth - 1) / (static_cast<single>(LENGTH_BLANKS_INTEGER_AT_BOL + nominalWidth))));
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
                    std::wstring valueAsString
                        = formatInteger(ap, classA, idx, currentNumericFormat);
                    io->outputMessage(BLANKS_INTEGER_AT_BOL);
                    io->outputMessage(valueAsString);
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
indexType
getIntegerNominalWidth(const ArrayOf& A, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = DEFAULT_NOMINAL_WIDTH;
    switch (A.getDataClass()) {
    case NLS_UINT8: {
        uint8* val = (uint8*)A.getDataPointer();
        nominalWidth
            = getUnsignedIntegerNominalWidth<uint8>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT8: {
        int8* val = (int8*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int8>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_UINT16: {
        uint16* val = (uint16*)A.getDataPointer();
        nominalWidth = getUnsignedIntegerNominalWidth<uint16>(
            val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT16: {
        int16* val = (int16*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int16>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_UINT32: {
        uint32* val = (uint32*)A.getDataPointer();
        nominalWidth = getUnsignedIntegerNominalWidth<uint32>(
            val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT32: {
        int32* val = (int32*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int32>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_UINT64: {
        uint64* val = (uint64*)A.getDataPointer();
        nominalWidth = getUnsignedIntegerNominalWidth<uint64>(
            val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT64: {
        int64* val = (int64*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int64>(val, A.getElementCount(), currentNumericFormat);
    } break;
    default: {
        nominalWidth = DEFAULT_NOMINAL_WIDTH;
    } break;
    }
    return nominalWidth;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
