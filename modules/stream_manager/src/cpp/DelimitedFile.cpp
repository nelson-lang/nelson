//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DelimitedFile.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include <fstream>
#include <iostream>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP void
delimitedWrite(ArrayOf mat, const std::wstring& filenameDestination, bool bAppend,
    const std::wstring& delimiter, int64 rowsOffset, int64 colsOffset,
    const std::wstring& formatPrecision, bool isNewLinePc)
{
    bool bTypeSupported = mat.isNumeric() || mat.isLogical();
    if (!bTypeSupported) {
        Error(_W("An numeric matrix expected."));
    }
    if (mat.isSparse()) {
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    if (mat.isComplex()) {
        mat.promoteType(NLS_DCOMPLEX);
    } else {
        mat.promoteType(NLS_DOUBLE);
    }
    std::ios::openmode wofstream_mode = std::ios::app | std::ios::binary;
    if (bAppend) {
        wofstream_mode = std::ios::app | std::ios::binary;
    } else {
        wofstream_mode = std::ios::trunc | std::ios::binary;
    }
#ifdef _MSC_VER
    std::wofstream outputStream(filenameDestination, wofstream_mode);
#else
    std::wofstream outputStream(wstring_to_utf8(filenameDestination), wofstream_mode);
#endif
    if (!outputStream.is_open()) {
        Error(_W("Impossible to open file."));
    } else {
        Dimensions dims = mat.getDimensions();
        std::wstring fmt_with_delimiter = formatPrecision + delimiter;
        int64 ymax;
        if (mat.is2D()) {
            ymax = dims.getColumns() + colsOffset - 1;
        } else {
            ymax = dims.getElementCount() / dims.getRows() + colsOffset - 1;
        }
        for (int64 x = 1; x <= rowsOffset; x++) {
            for (int64 y = 1; y <= ymax; y++) {
                outputStream << delimiter;
            }
            if (isNewLinePc) {
                outputStream << L"\r\n";
            } else {
                outputStream << L"\n";
            }
        }
        if (mat.is2D()) {
            ymax = dims.getColumns();
        } else {
            ymax = dims.getElementCount() / dims.getRows();
        }
        if (mat.isComplex()) {
            auto* matCplx = reinterpret_cast<doublecomplex*>((double*)mat.getDataPointer());
            for (int64 x = 0; x < static_cast<int64>(dims.getRows()); x++) {
                for (int64 x = 0; x < colsOffset; x++) {
                    outputStream << delimiter;
                }
                std::wstring realPartStr;
                std::wstring imagPartStr;
                for (int64 y = 0; y < ymax; y++) {
                    doublecomplex val = matCplx[x + y * dims.getRows()];
                    if (std::isnan(val.real())) {
                        realPartStr = L"NaN";
                    } else {
                        if (std::isinf(val.real())) {
                            if (val.real() > 0) {
                                realPartStr = L"Inf";
                            } else {
                                realPartStr = L"-Inf";
                            }
                        } else {
                            wchar_t buffer[1024];
                            swprintf(buffer, 1024, formatPrecision.c_str(), val.real());
                            realPartStr = buffer;
                        }
                    }
                    if (std::isnan(val.imag())) {
                        imagPartStr = L"+NaN";
                    } else {
                        if (std::isinf(val.imag())) {
                            if (val.imag() > 0) {
                                imagPartStr = L"+Inf";
                            } else {
                                imagPartStr = L"-Inf";
                            }
                        } else {
                            wchar_t buffer[1024];
                            swprintf(buffer, 1024, formatPrecision.c_str(), val.imag());
                            if (val.imag() >= 0) {
                                imagPartStr = buffer;
                                imagPartStr = L"+" + imagPartStr;
                            } else {
                                imagPartStr = buffer;
                            }
                        }
                    }
                    std::wstring numberAsString = realPartStr + imagPartStr + L"i";
                    outputStream << numberAsString;
                    if (y < ymax - 1) {
                        outputStream << delimiter;
                    }
                }
                if (isNewLinePc) {
                    outputStream << L"\r\n";
                } else {
                    outputStream << L"\n";
                }
            }
        } else {
            auto* pValue = (double*)mat.getDataPointer();
            for (int64 x = 0; x < static_cast<int64>(dims.getRows()); x++) {
                for (int64 x = 0; x < colsOffset; x++) {
                    outputStream << delimiter;
                }
                for (int64 y = 0; y < ymax; y++) {
                    double val = pValue[x + y * dims.getRows()];
                    if (std::isnan(val)) {
                        outputStream << L"NaN";
                        if (y < ymax - 1) {
                            outputStream << delimiter;
                        }
                    } else {
                        if (std::isinf(val)) {
                            if (val > 0) {
                                outputStream << L"Inf";
                            } else {
                                outputStream << L"-Inf";
                            }
                            if (y < ymax - 1) {
                                outputStream << delimiter;
                            }
                        } else {
                            std::wstring fmt(formatPrecision);
                            if (y < ymax - 1) {
                                fmt = fmt_with_delimiter;
                            }
                            wchar_t buffer[1024];
                            swprintf(buffer, 1024, fmt.c_str(), val);
                            outputStream << buffer;
                        }
                    }
                }
                if (isNewLinePc) {
                    outputStream << L"\r\n";
                } else {
                    outputStream << L"\n";
                }
            }
        }
        outputStream.close();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
