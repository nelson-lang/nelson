//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <fstream>
#include <iostream>
#include "DelimitedFileWriter.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
writeRealData(const ArrayOf& mat, FILE* file, const std::string& formatPrecision,
    const std::string& delimiter, const std::string& newline, int64 ymax, int64 colsOffset);
void
writeComplexData(const ArrayOf& mat, FILE* file, const std::string& formatPrecision,
    const std::string& delimiter, const std::string& newline, int64 ymax, int64 colsOffset);
//=============================================================================
void
delimitedFileWriter(ArrayOf mat, const std::wstring& filenameDestination, bool bAppend,
    const std::wstring& delimiter, int64 rowsOffset, int64 colsOffset,
    const std::wstring& formatPrecision, bool isNewLinePc)
{
    bool bTypeSupported = mat.isNumeric() || mat.isLogical();
    if (!bTypeSupported) {
        Error(L"Nelson:dlmwrite:numericMatrixExpected", L"An numeric matrix expected.");
    }
    if (mat.isSparse()) {
        Error(L"Nelson:dlmwrite:typeNotSupported", L"Type not supported.");
    }
#ifdef _MSC_VER
    FILE* file = _wfopen(filenameDestination.c_str(), bAppend ? L"ab" : L"wb");
#else
    FILE* file = fopen(wstring_to_utf8(filenameDestination).c_str(), bAppend ? "ab" : "wb");
#endif
    if (file == nullptr) {
        Error(L"Nelson:dlmwrite:cannotOpenFile", L"Impossible to open file.");
    }

    const std::string newline = isNewLinePc ? "\r\n" : "\n";

    mat.isComplex() ? mat.promoteType(NLS_DCOMPLEX) : mat.promoteType(NLS_DOUBLE);
    Dimensions dims = mat.dimensions();
    int64 ymax = mat.is2D() ? dims.columns() + colsOffset - 1
                            : dims.elementCount() / dims.rows() + colsOffset - 1;

    for (int64 x = 1; x <= rowsOffset; x++) {
        for (int64 y = 1; y <= ymax; y++) {
            fmt::fprintf(file, "%s", wstring_to_utf8(delimiter));
        }
        fmt::fprintf(file, "%s", newline);
    }

    ymax = mat.is2D() ? dims.columns() : dims.elementCount() / dims.rows();

    if (mat.isComplex()) {
        writeComplexData(mat, file, wstring_to_utf8(formatPrecision), wstring_to_utf8(delimiter),
            newline, ymax, colsOffset);
    } else {
        writeRealData(mat, file, wstring_to_utf8(formatPrecision), wstring_to_utf8(delimiter),
            newline, ymax, colsOffset);
    }
    fclose(file);
}
//=============================================================================
void
writeRealData(const ArrayOf& mat, FILE* file, const std::string& formatPrecision,
    const std::string& delimiter, const std::string& newline, int64 ymax, int64 colsOffset)
{
    Dimensions dims = mat.dimensions();
    std::string fmt_with_delimiter = formatPrecision + delimiter;
    auto* pValue = (double*)mat.dataPointer();
    for (int64 x = 0; x < static_cast<int64>(dims.rows()); x++) {
        for (int64 x = 0; x < colsOffset; x++) {
            fmt::fprintf(file, "%s", delimiter);
        }
        for (int64 y = 0; y < ymax; y++) {
            double val = pValue[x + y * dims.rows()];
            if (std::isnan(val)) {
                fmt::fprintf(file, "%s", "NaN");
                if (y < ymax - 1) {
                    fmt::fprintf(file, "%s", delimiter);
                }
            } else {
                if (std::isinf(val)) {
                    fmt::fprintf(file, val > 0 ? "Inf" : "-Inf");
                    if (y < ymax - 1) {
                        fmt::fprintf(file, "%s", delimiter);
                    }
                } else {
                    std::string format(y < ymax - 1 ? fmt_with_delimiter : formatPrecision);
                    fmt::fprintf(file, format, val);
                }
            }
        }
        fmt::fprintf(file, "%s", newline);
    }
}
//=============================================================================
void
writeComplexData(const ArrayOf& mat, FILE* file, const std::string& formatPrecision,
    const std::string& delimiter, const std::string& newline, int64 ymax, int64 colsOffset)
{
    Dimensions dims = mat.dimensions();
    auto* matCplx = reinterpret_cast<doublecomplex*>((double*)mat.dataPointer());
    for (int64 x = 0; x < static_cast<int64>(dims.rows()); x++) {
        for (int64 x = 0; x < colsOffset; x++) {
            fmt::fprintf(file, "%s", delimiter);
        }
        std::string realPartStr;
        std::string imagPartStr;
        for (int64 y = 0; y < ymax; y++) {
            doublecomplex val = matCplx[x + y * dims.rows()];
            if (std::isnan(val.real())) {
                realPartStr = "NaN";
            } else {
                if (std::isinf(val.real())) {
                    realPartStr = (val.real() > 0) ? "Inf" : "-Inf";
                } else {
                    realPartStr = fmt::sprintf(formatPrecision, val.real());
                }
            }
            if (std::isnan(val.imag())) {
                imagPartStr = "+NaN";
            } else {
                if (std::isinf(val.imag())) {
                    imagPartStr = val.imag() > 0 ? "+Inf" : "-Inf";
                } else {
                    imagPartStr = fmt::sprintf(formatPrecision, val.imag());
                    if (val.imag() >= 0) {
                        imagPartStr = "+" + imagPartStr;
                    }
                }
            }
            fmt::fprintf(file, "%s%si", realPartStr, imagPartStr);
            if (y < ymax - 1) {
                fmt::fprintf(file, "%s", delimiter);
            }
        }
        fmt::fprintf(file, "%s", newline);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
