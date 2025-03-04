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
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <regex>
#include <fast_float/fast_float.h>
#include "DelimiterFileReader.hpp"
#include "characters_encoding.hpp"
#include "StringToDoubleComplex.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
#include "CSVSeparatorDetector.hpp"
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
static void
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
        throw std::invalid_argument("Invalid number format.");
    }
}
//=============================================================================
static void
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

            ConvertToDouble(realStr, realPart);
            ConvertToDouble(imagStr, imagPart);
            if (isNegativeReal) {
                realPart = -realPart;
            }
            if (isNegativeImag) {
                imagPart = -imagPart;
            }
            pVal = { realPart, imagPart };
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
            ConvertToDouble(imagStr, imagPart);
            if (isNegativeImag) {
                imagPart = -imagPart;
            }
            pVal = { 0., imagPart };
        } else {
            throw std::invalid_argument("Invalid number format.");
        }
    } else {
        double valueReal;
        ConvertToDouble(str, valueReal);
        pVal = { valueReal, 0 };
    }
}
//=============================================================================
static rapidcsv::Document
readDocument(
    const std::wstring& filename, const std::wstring& delimiter, std::wstring& errorMessage)
{
    char separator = ',';
    if (delimiter.empty()) {
        separator = CSVSeparatorDetector(filename, errorMessage);
        if (!errorMessage.empty()) {
            return rapidcsv::Document();
        }
    } else {
        separator = wstring_to_utf8(delimiter)[0];
    }

    rapidcsv::SeparatorParams separatorParams
        = rapidcsv::SeparatorParams(separator, true, true, false, false);

    rapidcsv::ConverterParams converterParams;
    converterParams.mHasDefaultConverter = false;
    converterParams.mNumericLocale = false;

    rapidcsv::LineReaderParams lineReaderParams;
    lineReaderParams.mSkipCommentLines = false;
    lineReaderParams.mSkipEmptyLines = true;

    rapidcsv::LabelParams labelParams(-1, -1);

#ifdef _MSC_VER
    std::ifstream file(filename);
#else
    std::ifstream file(wstring_to_utf8(filename));
#endif

    rapidcsv::Document doc(file, labelParams, separatorParams, converterParams, lineReaderParams);
    return doc;
}
//=============================================================================
ArrayOf
delimitedFileReader(const std::wstring& filename, const std::wstring& delimiter,
    std::vector<double> range, std::wstring& errorMessage)
{
    rapidcsv::Document doc = readDocument(filename, delimiter, errorMessage);
    if (!errorMessage.empty()) {
        return {};
    }

    size_t R1 = 0;
    size_t C1 = 0;
    size_t R2 = doc.GetRowCount();
    size_t C2 = doc.GetColumnCount();
    if (range.size() == 4) {
        R1 = (size_t)range[0];
        C1 = (size_t)range[1];
        R2 = (size_t)range[2] + 1;
        C2 = (size_t)range[3] + 1;
    } else if (range.size() == 2) {
        R1 = (size_t)range[0];
        C1 = (size_t)range[1];
    }

    size_t nbRowsRange = (R2 - R1);
    size_t nbColumnsRange = (C2 - C1);
    ompIndexType nbElements = nbRowsRange * nbColumnsRange;

    bool asComplex = doc.IsIJLastChar();
    bool hasFailed = false;

    ArrayOf result;
    Dimensions dims(nbRowsRange, nbColumnsRange);
    if (asComplex) {
        std::complex<double>* values = static_cast<std::complex<double>*>(
            ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbElements));
        result = ArrayOf(NLS_DCOMPLEX, dims, values);
        hasFailed = false;
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType index = 0; index < nbElements; ++index) {
            if (hasFailed) {
                continue;
            }
            size_t C = C1 + (index / nbRowsRange);
            size_t R = R1 + (index % nbRowsRange);
            try {
                values[index] = doc.GetCell<std::complex<double>>(C, R, ConvertToDoubleComplex);
            } catch (const std::exception&) {
#if WITH_OPENMP
#pragma omp critical
#endif
                {
                    hasFailed = true;
                    errorMessage = fmt::sprintf(
                        _W("Unable to parse numeric value at row %d, column %d."), R + 1, C + 1);
                }
            }
        }
        if (hasFailed) {
            return {};
        }
    } else {
        double* values = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements));
        result = ArrayOf(NLS_DOUBLE, dims, values);
        hasFailed = false;
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType index = 0; index < nbElements; ++index) {
            if (hasFailed) {
                continue;
            }
            size_t C = C1 + (index / nbRowsRange);
            size_t R = R1 + (index % nbRowsRange);
            try {
                values[index] = doc.GetCell<double>(C, R, ConvertToDouble);
            } catch (const std::exception&) {
#if WITH_OPENMP
#pragma omp critical
#endif
                {
                    errorMessage = fmt::sprintf(
                        _W("Unable to parse numeric value at row %d, column %d."), R + 1, C + 1);
                    hasFailed = true;
                }
            }
        }
        if (hasFailed) {
            return {};
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
