//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <rapidcsv.h>
#include <complex>
#include <regex>
#include <fast_float/fast_float.h>
#include "ReadCell.hpp"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
#include "CSVTypeConverters.hpp"
#include "ReadLinesFromFile.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
ConvertToArrayOfCharacter(const std::string& pStr, ArrayOf& pVal)
{
    double value;
    if (ConvertToDouble(pStr, value)) {
        pVal = ArrayOf::doubleConstructor(value);
        return;
    }
    std::complex<double> cvalue;
    if (ConvertToDoubleComplex(pStr, cvalue)) {
        if (cvalue.imag() == 0) {
            pVal = ArrayOf::doubleConstructor(cvalue.real());
        } else {
            pVal = ArrayOf::dcomplexConstructor(cvalue.real(), cvalue.imag());
        }
        return;
    }
    if (pStr == "<Missing>") {
        Dimensions dims(1, 1);
        pVal = ArrayOf::stringArrayConstructorAllMissing(dims);
        return;
    }
    pVal = ArrayOf::characterArrayConstructor(pStr);
}
//=============================================================================
static void
ConvertToArrayOfString(const std::string& pStr, ArrayOf& pVal)
{
    double value;
    if (ConvertToDouble(pStr, value)) {
        pVal = ArrayOf::doubleConstructor(value);
        return;
    }
    std::complex<double> cvalue;
    if (ConvertToDoubleComplex(pStr, cvalue)) {
        if (cvalue.imag() == 0) {
            pVal = ArrayOf::doubleConstructor(cvalue.real());
        } else {
            pVal = ArrayOf::dcomplexConstructor(cvalue.real(), cvalue.imag());
        }
        return;
    }
    pVal = ArrayOf::stringArrayConstructor(pStr);
}
//=============================================================================
ArrayOf
ReadCell(
    const std::wstring& filename, const detectImportOptions& options, std::string& errorMessage)
{
    char separator = options.Delimiter[0][0];
    bool pHasCR = false;
    rapidcsv::SeparatorParams separatorParams
        = rapidcsv::SeparatorParams(separator, true, pHasCR, false, false);

    rapidcsv::ConverterParams converterParams;
    converterParams.mHasDefaultConverter = false;
    converterParams.mNumericLocale = false;

    rapidcsv::LineReaderParams lineReaderParams;
    lineReaderParams.mSkipCommentLines = !options.CommentStyle.empty();
    if (options.CommentStyle.empty()) {
        lineReaderParams.mCommentPrefix = '\0';
        lineReaderParams.mSkipCommentLines = false;
    } else {
        lineReaderParams.mCommentPrefix = options.CommentStyle[0][0];
        lineReaderParams.mSkipCommentLines = true;
    }
    lineReaderParams.mSkipEmptyLines = options.EmptyLineRule == "skip";

    rapidcsv::LabelParams labelParams(-1, -1);
    try {
        std::stringstream stream = readLinesFromFile(filename, options);
        rapidcsv::Document doc(
            stream, labelParams, separatorParams, converterParams, lineReaderParams);
        stringVector columnNames = doc.GetColumnNames();
        stringVector rowNames = doc.GetRowNames();
        size_t nbRows = doc.GetRowCount();
        size_t nbColumns = doc.GetColumnCount();
        size_t nbElements = nbRows
            * (options.VariableNames.size() > nbColumns ? options.VariableNames.size() : nbColumns);

        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        Dimensions dims(nbRows,
            options.VariableNames.size() > nbColumns ? options.VariableNames.size() : nbColumns);
        ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, elements);

        ompIndexType nbAvailableElements = (ompIndexType)(nbColumns * nbRows);

        if (options.TextType == "char") {
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                elements[index] = doc.GetCell<ArrayOf>(i, j, ConvertToArrayOfCharacter);
            }
        } else {
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType index = 0; index < nbAvailableElements; ++index) {
                size_t i = index / nbRows;
                size_t j = index % nbRows;
                elements[index] = doc.GetCell<ArrayOf>(i, j, ConvertToArrayOfString);
            }
        }

        return result;
    } catch (const std::exception& e) {
        errorMessage = e.what();
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
