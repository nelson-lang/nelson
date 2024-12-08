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
#include "ReadTable.hpp"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
#if WITH_OPENMP
#include <omp.h>
#endif
#include "CSVTypeConverters.hpp"
#include "ReadLinesFromFile.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct DoubleDoubleComplexString
{
    double asDouble;
    std::complex<double> asDoubleComplex;
    std::string asString;
    NelsonType nelsonType;
};
//=============================================================================
static void
ConvertToArrayOfCharacter(const std::string& pStr, struct DoubleDoubleComplexString& structValue)
{
    double value;
    structValue.asString = pStr;
    if (ConvertToDouble(pStr, value)) {
        structValue.asDouble = value;
        structValue.nelsonType = NLS_DOUBLE;
        structValue.asDoubleComplex = std::complex<double>(value, 0);
        return;
    }
    std::complex<double> cvalue;
    if (ConvertToDoubleComplex(pStr, cvalue)) {
        structValue.asDouble = cvalue.real();
        structValue.nelsonType = NLS_DCOMPLEX;
        structValue.asDoubleComplex = cvalue;
        return;
    }
    structValue.asDouble = std::nan("NaN");
    structValue.asDoubleComplex = std::complex<double>(std::nan("NaN"), std::nan("NaN"));
    structValue.nelsonType = NLS_CHAR;
}
//=============================================================================
ArrayOf
ReadTable(
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
        stringVector columnNames = options.VariableNames;
        stringVector rowNames = doc.GetRowNames();
        size_t nbRows = doc.GetRowCount();
        size_t nbColumns = doc.GetColumnCount();

        ArrayOfVector columnValues;
        columnValues.resize(nbColumns);
        for (ompIndexType c = 0; c < (ompIndexType)columnValues.size(); c++) {
            std::vector<DoubleDoubleComplexString> structValues;
            structValues.resize(nbRows);

#if WITH_OPENMP
            int nbThreads = omp_get_max_threads();
#else
            int nbThreads = 1;

#endif
            std::unordered_map<NelsonType, int> countMap;
            std::vector<std::unordered_map<NelsonType, int>> localCountMaps(nbThreads);

#if WITH_OPENMP
#pragma omp parallel
#endif
            {
#if WITH_OPENMP
                int threadId = omp_get_thread_num();
#else
                int threadId = 1;
#endif
                std::unordered_map<NelsonType, int>& localMap = localCountMaps[threadId];
#if WITH_OPENMP
#pragma omp for
#endif
                for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                    structValues[r]
                        = doc.GetCell<DoubleDoubleComplexString>(c, r, ConvertToArrayOfCharacter);
                    localMap[structValues[r].nelsonType]++;
                }
            }
            // Merge results from all threads
            for (const auto& localMap : localCountMaps) {
                for (const auto& entry : localMap) {
                    countMap[entry.first] += entry.second;
                }
            }

            int maxCount = 0;
            NelsonType mostFrequentType = NLS_CELL_ARRAY;
            for (const auto& pair : countMap) {
                if (pair.second > maxCount) {
                    maxCount = pair.second;
                    mostFrequentType = pair.first;
                }
            }

            if (mostFrequentType == NLS_DOUBLE && countMap[NLS_DCOMPLEX] > 0) {
                mostFrequentType = NLS_DCOMPLEX;
            }

            Dimensions dims(nbRows, 1);
            switch (mostFrequentType) {
            case NLS_DOUBLE: {
                double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nbRows);
#if WITH_OPENMP
#pragma omp for
#endif
                for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                    ptr[r] = structValues[r].asDouble;
                }
                columnValues[c] = ArrayOf(NLS_DOUBLE, dims, ptr);
            } break;
            case NLS_DCOMPLEX: {
                std::complex<double>* ptr = reinterpret_cast<std::complex<double>*>(
                    ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbRows));
#if WITH_OPENMP
#pragma omp for
#endif
                for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                    ptr[r] = structValues[r].asDoubleComplex;
                }
                columnValues[c] = ArrayOf(NLS_DCOMPLEX, dims, ptr);
            } break;
            case NLS_CELL_ARRAY:
            default: {
                ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
#if WITH_OPENMP
#pragma omp for
#endif
                for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                    elements[r] = ArrayOf::characterArrayConstructor(structValues[r].asString);
                }
                columnValues[c] = ArrayOf(NLS_CELL_ARRAY, dims, elements);
            } break;
            }
        }
        return ArrayOf::tableConstructor(columnValues, columnNames, rowNames);
    } catch (const std::exception& e) {
        errorMessage = e.what();
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
