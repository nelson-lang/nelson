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
#include <algorithm>
#include <cctype>
#include <cwctype>
#include <limits>
#include <unordered_map>
#include <fast_float/fast_float.h>
#include "ReadTable.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#if WITH_OPENMP
#include <omp.h>
#endif
#include "CSVTypeConverters.hpp"
#include "ReadLinesFromFile.hpp"
#include "ErrorID.hpp"
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
    structValue.asDouble = std::nan("");
    structValue.asDoubleComplex = std::complex<double>(std::nan(""), std::nan(""));
    structValue.nelsonType = NLS_CHAR;
}
//=============================================================================
static ArrayOf
buildColumnFromCells(const std::vector<DoubleDoubleComplexString>& structValues,
    NelsonType mostFrequentType, const std::string& textType);
//=============================================================================
static bool
parseTableRangeSpec(const std::wstring& spec, long long& rowStart, long long& rowEnd,
    long long& colStart, long long& colEnd);
//=============================================================================
static bool
isAutoDetectedType(const std::string& declaredType)
{
    // An empty, "char" or "auto" type keeps automatic column-type detection.
    // Any other type bypasses it so the caller can coerce the raw text.
    if (declaredType.empty()) {
        return true;
    }
    std::string lowered = declaredType;
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
        [](unsigned char c) { return (char)std::tolower(c); });
    return lowered == "char" || lowered == "auto";
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
            for (const auto& localMap : localCountMaps) {
                for (const auto& entry : localMap) {
                    countMap[entry.first] += entry.second;
                }
            }

            Dimensions dims(nbRows, 1);

            // An explicitly typed column stays raw text; the builtin layer coerces it.
            std::string declaredType
                = ((size_t)c < options.VariableTypes.size()) ? options.VariableTypes[c] : "";
            if (!isAutoDetectedType(declaredType)) {
                stringVector values;
                values.reserve(nbRows);
                for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                    values.push_back(structValues[r].asString);
                }
                columnValues[c] = ArrayOf::stringArray(values, dims);
                continue;
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
                if (options.TextType == "string") {
                    stringVector values;
                    values.reserve(nbRows);
                    for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                        values.push_back(structValues[r].asString);
                    }
                    columnValues[c] = ArrayOf::stringArray(values, dims);
                } else {
                    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
#if WITH_OPENMP
#pragma omp for
#endif
                    for (ompIndexType r = 0; r < (ompIndexType)nbRows; r++) {
                        elements[r] = ArrayOf::characterArray(structValues[r].asString);
                    }
                    columnValues[c] = ArrayOf(NLS_CELL_ARRAY, dims, elements);
                }
            } break;
            }
        }
        return ArrayOf::table(columnValues, columnNames, rowNames);
    } catch (const std::exception& e) {
        errorMessage = e.what();
    }
    return {};
}
//=============================================================================
ArrayOf
ReadTableRange(const std::wstring& filename, const detectImportOptions& options,
    const std::wstring& rangeSpec, int readVariableNames, std::string& errorMessage)
{
    long long rowStart = 1, rowEnd = -1, colStart = 1, colEnd = -1;
    if (!parseTableRangeSpec(rangeSpec, rowStart, rowEnd, colStart, colEnd)) {
        errorMessage = Nelson::resolveMessageTemplate(
            "Nelson:spreadsheet:invalidRange", "Invalid Range value.");
        return {};
    }

    char separator = options.Delimiter.empty() ? ',' : options.Delimiter[0][0];
    bool pHasCR = false;
    rapidcsv::SeparatorParams separatorParams(separator, true, pHasCR, false, false);
    rapidcsv::ConverterParams converterParams;
    converterParams.mHasDefaultConverter = false;
    converterParams.mNumericLocale = false;
    rapidcsv::LineReaderParams lineReaderParams;
    if (options.CommentStyle.empty()) {
        lineReaderParams.mCommentPrefix = '\0';
        lineReaderParams.mSkipCommentLines = false;
    } else {
        lineReaderParams.mCommentPrefix = options.CommentStyle[0][0];
        lineReaderParams.mSkipCommentLines = true;
    }
    lineReaderParams.mSkipEmptyLines = options.EmptyLineRule == "skip";
    rapidcsv::LabelParams labelParams(-1, -1);

    // Read every physical line so that Range row numbers map directly to document rows.
    detectImportOptions allLines = options;
    allLines.DataLines.clear();
    allLines.DataLines.push_back(1);
    allLines.DataLines.push_back(std::numeric_limits<double>::infinity());

    try {
        std::stringstream stream = readLinesFromFile(filename, allLines);
        rapidcsv::Document doc(
            stream, labelParams, separatorParams, converterParams, lineReaderParams);
        long long totalRows = (long long)doc.GetRowCount();
        long long totalCols = (long long)doc.GetColumnCount();

        long long colEndReq = colEnd < 0 ? totalCols : colEnd;
        long long rowEndReq = rowEnd < 0 ? totalRows : rowEnd;

        long long colFirst = colStart; // 1-based
        long long colLast = std::min(colEndReq, totalCols);
        long long columnCount = (colLast >= colFirst) ? (colLast - colFirst + 1) : 0;

        long long headerRow0 = rowStart - 1;
        std::vector<std::string> headerCells;
        bool looksLikeHeader = false;
        if (headerRow0 >= 0 && headerRow0 < totalRows && columnCount > 0) {
            looksLikeHeader = true;
            bool anyLetter = false;
            headerCells.reserve((size_t)columnCount);
            for (long long c = 0; c < columnCount; ++c) {
                DoubleDoubleComplexString sv = doc.GetCell<DoubleDoubleComplexString>(
                    (size_t)(colFirst - 1 + c), (size_t)headerRow0, ConvertToArrayOfCharacter);
                headerCells.push_back(sv.asString);
                if (sv.nelsonType != NLS_CHAR) {
                    looksLikeHeader = false;
                }
                for (char ch : sv.asString) {
                    if (std::isalpha((unsigned char)ch)) {
                        anyLetter = true;
                    }
                }
            }
            looksLikeHeader = looksLikeHeader && anyLetter;
        }

        bool consumeHeader = (readVariableNames == 1) || looksLikeHeader;
        bool useHeaderNames = consumeHeader && (readVariableNames != 0);

        long long dataFirst = headerRow0 + (consumeHeader ? 1 : 0); // 0-based inclusive
        long long dataLast = std::min(rowEndReq, totalRows) - 1; // 0-based inclusive
        if (dataFirst < 0) {
            dataFirst = 0;
        }
        long long rowCount = (dataLast >= dataFirst) ? (dataLast - dataFirst + 1) : 0;

        ArrayOfVector columnValues;
        columnValues.resize((size_t)columnCount);
        stringVector columnNames;
        columnNames.reserve((size_t)columnCount);

        for (long long cc = 0; cc < columnCount; ++cc) {
            long long c0 = colFirst - 1 + cc;
            std::vector<DoubleDoubleComplexString> structValues((size_t)rowCount);
            std::unordered_map<NelsonType, int> countMap;
            for (long long rr = 0; rr < rowCount; ++rr) {
                structValues[(size_t)rr] = doc.GetCell<DoubleDoubleComplexString>(
                    (size_t)c0, (size_t)(dataFirst + rr), ConvertToArrayOfCharacter);
                countMap[structValues[(size_t)rr].nelsonType]++;
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
            columnValues[(size_t)cc]
                = buildColumnFromCells(structValues, mostFrequentType, options.TextType);

            if (useHeaderNames && (size_t)cc < headerCells.size()) {
                columnNames.push_back(headerCells[(size_t)cc]);
            } else {
                columnNames.push_back("Var" + std::to_string((int)(cc + 1)));
            }
        }
        return ArrayOf::table(columnValues, columnNames, stringVector {});
    } catch (const std::exception& e) {
        errorMessage = e.what();
    }
    return {};
}
//=============================================================================
static ArrayOf
buildColumnFromCells(const std::vector<DoubleDoubleComplexString>& structValues,
    NelsonType mostFrequentType, const std::string& textType)
{
    size_t nbRows = structValues.size();
    Dimensions dims(nbRows, 1);
    switch (mostFrequentType) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nbRows);
        for (size_t r = 0; r < nbRows; r++) {
            ptr[r] = structValues[r].asDouble;
        }
        return ArrayOf(NLS_DOUBLE, dims, ptr);
    }
    case NLS_DCOMPLEX: {
        std::complex<double>* ptr = reinterpret_cast<std::complex<double>*>(
            ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbRows));
        for (size_t r = 0; r < nbRows; r++) {
            ptr[r] = structValues[r].asDoubleComplex;
        }
        return ArrayOf(NLS_DCOMPLEX, dims, ptr);
    }
    case NLS_CELL_ARRAY:
    default: {
        if (textType == "string") {
            stringVector values;
            values.reserve(nbRows);
            for (size_t r = 0; r < nbRows; r++) {
                values.push_back(structValues[r].asString);
            }
            return ArrayOf::stringArray(values, dims);
        }
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbRows);
        for (size_t r = 0; r < nbRows; r++) {
            elements[r] = ArrayOf::characterArray(structValues[r].asString);
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    }
}
//=============================================================================
static long long
lettersToColumnIndex(const std::wstring& letters)
{
    long long value = 0;
    for (wchar_t c : letters) {
        wchar_t upper = (wchar_t)std::towupper(c);
        value = value * 26 + (long long)(upper - L'A' + 1);
    }
    return value;
}
//=============================================================================
static bool
parseRangeCorner(const std::wstring& token, long long& column, long long& row)
{
    std::wstring letters;
    std::wstring digits;
    for (wchar_t c : token) {
        if (c == L'$' || c == L' ') {
            continue;
        }
        if ((c >= L'A' && c <= L'Z') || (c >= L'a' && c <= L'z')) {
            if (!digits.empty()) {
                return false;
            }
            letters.push_back(c);
        } else if (c >= L'0' && c <= L'9') {
            digits.push_back(c);
        } else {
            return false;
        }
    }
    if (letters.empty() && digits.empty()) {
        return false;
    }
    column = letters.empty() ? 0 : lettersToColumnIndex(letters);
    row = digits.empty() ? 0 : std::stoll(digits);
    return true;
}
//=============================================================================
static bool
parseTableRangeSpec(const std::wstring& spec, long long& rowStart, long long& rowEnd,
    long long& colStart, long long& colEnd)
{
    size_t colon = spec.find(L':');
    long long c1 = 0, r1 = 0, c2 = 0, r2 = 0;
    if (colon == std::wstring::npos) {
        if (!parseRangeCorner(spec, c1, r1)) {
            return false;
        }
        colStart = c1 > 0 ? c1 : 1;
        rowStart = r1 > 0 ? r1 : 1;
        colEnd = -1;
        rowEnd = -1;
        return true;
    }
    if (!parseRangeCorner(spec.substr(0, colon), c1, r1)
        || !parseRangeCorner(spec.substr(colon + 1), c2, r2)) {
        return false;
    }
    colStart = c1 > 0 ? c1 : 1;
    rowStart = r1 > 0 ? r1 : 1;
    colEnd = c2 > 0 ? c2 : -1;
    rowEnd = r2 > 0 ? r2 : -1;
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
