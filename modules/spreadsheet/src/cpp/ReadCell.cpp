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
//=============================================================================
namespace Nelson {
//=============================================================================
struct ComplexPatterns
{
    // Regex for special values (Inf, NaN)
    static inline const std::string special_re = R"((?:[Nn][Aa][Nn]|[Ii][Nn][Ff]))";

    // Full regex patterns combining numbers and special values
    static inline const std::regex full_complex { R"(([+-]?(?:\d*\.?\d+|)" + special_re
            + R"())([+-](?:\d*\.?\d+|)" + special_re + R"())[ij])",
        std::regex::optimize };
    static inline const std::regex real_only {
        R"(([+-]?(?:\d*\.?\d+|)" + special_re + R"())(?![ij]))", std::regex::optimize
    };
    static inline const std::regex imag_only { R"(([+-]?(?:\d*\.?\d+|)" + special_re + R"())[ij])",
        std::regex::optimize };
};
//=============================================================================
static bool
ConvertToDouble(const std::string& pStr, double& pVal)
{
    fast_float::parse_options options { fast_float::chars_format::fortran };

    const char* first = pStr.data();
    const char* last = pStr.data() + pStr.size();
    if (!pStr.empty() && pStr.front() == '+') {
        first += 1;
    }

    auto answer = fast_float::from_chars_advanced(first, last, pVal, options);

    if (answer.ec != std::errc() || answer.ptr != last) {
        return false;
    }
    return true;
}
//=============================================================================
static bool
ConvertToDoubleComplex(const std::string& str, std::complex<double>& pVal)
{
    char lastChar = '\0';
    if (!str.empty()) {
        lastChar = str.back();
    }
    if ((lastChar != '\0') && lastChar == 'I' || lastChar == 'J' || lastChar == 'i'
        || lastChar == 'j') {
        std::smatch matches;
        if (std::regex_match(str, matches, ComplexPatterns::full_complex)) {
            bool isNegativeReal = false;
            bool isNegativeImag = false;
            std::string realStr = matches[1].str();
            std::string imagStr = matches[2].str();
            if (imagStr.front() == L'+' || imagStr.front() == L'-') {
                if (imagStr.front() == L'-') {
                    isNegativeImag = true;
                }
                imagStr.erase(0, 1);
            }

            double realPart, imagPart;

            bool res = ConvertToDouble(realStr, realPart);
            if (!res) {
                return res;
            }
            res = ConvertToDouble(imagStr, imagPart);
            if (!res) {
                return res;
            }
            if (isNegativeReal) {
                realPart = -realPart;
            }
            if (isNegativeImag) {
                imagPart = -imagPart;
            }
            pVal = { realPart, imagPart };
            return true;
        } else if (std::regex_match(str, matches, ComplexPatterns::imag_only)) {
            bool isNegativeImag = false;
            std::string imagStr = matches[1].str();
            if (imagStr.front() == L'+' || imagStr.front() == L'-') {
                if (imagStr.front() == L'-') {
                    isNegativeImag = true;
                }
                imagStr.erase(0, 1);
            }

            double imagPart;
            bool res = ConvertToDouble(imagStr, imagPart);
            if (!res) {
                return false;
            }
            if (isNegativeImag) {
                imagPart = -imagPart;
            }
            pVal = { 0., imagPart };
            return true;
        } else {
            return false;
        }
    } else {
        double valueReal;
        bool res = ConvertToDouble(str, valueReal);
        if (res) {
            pVal = { valueReal, 0 };
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
ConvertToArrayOfCharacter(const std::string& pStr, ArrayOf& pVal)
{
    std::complex<double> value;
    if (ConvertToDoubleComplex(pStr, value)) {
        if (value.imag() == 0) {
            pVal = ArrayOf::doubleConstructor(value.real());
        } else {
            pVal = ArrayOf::dcomplexConstructor(value.real(), value.imag());
        }
    } else {
        if (pStr == "<Missing>") {
            Dimensions dims(1, 1);
            pVal = ArrayOf::stringArrayConstructorAllMissing(dims);
        } else {
            pVal = ArrayOf::characterArrayConstructor(pStr);
        }
    }
}
//=============================================================================
static void
ConvertToArrayOfString(const std::string& pStr, ArrayOf& pVal)
{
    std::complex<double> value;
    if (ConvertToDoubleComplex(pStr, value)) {
        if (value.imag() == 0) {
            pVal = ArrayOf::doubleConstructor(value.real());
        } else {
            pVal = ArrayOf::dcomplexConstructor(value.real(), value.imag());
        }
    } else {
        pVal = ArrayOf::stringArrayConstructor(pStr);
    }
}
//=============================================================================
static std::stringstream
readLinesFromFile(const std::wstring& filename, const detectImportOptions& options)
{
    std::ifstream file;
#ifdef _MSC_VER
    file.open(filename);
#else
    file.open(wstring_to_utf8(filename));
#endif

    std::string line;
    int currentLine = 1;
    std::stringstream normalizedStream;

    while (currentLine < (int)options.DataLines[0] && std::getline(file, line)) {
        currentLine++;
    }

    auto normalizeLineEnding = [](const std::string& inputLine) -> std::string {
        std::string normalized = inputLine;
        normalized.erase(std::remove(normalized.begin(), normalized.end(), '\r'), normalized.end());
        return normalized;
    };

    if (std::isinf(options.DataLines[1])) {
        while (std::getline(file, line)) {
            normalizedStream << normalizeLineEnding(line) << '\n';
            currentLine++;
        }
    } else {
        while (currentLine <= (int)options.DataLines[1] && std::getline(file, line)) {
            normalizedStream << normalizeLineEnding(line) << '\n';
            currentLine++;
        }
    }
    return normalizedStream;
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
