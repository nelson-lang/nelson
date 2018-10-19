//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
delimitedWrite(ArrayOf mat, std::wstring filenameDestination, bool bAppend, std::wstring delimiter,
    int64 rowsOffset, int64 colsOffset, std::wstring formatPrecision, bool isNewLinePc)
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
            doublecomplex* matCplx
                = reinterpret_cast<doublecomplex*>((double*)mat.getDataPointer());
            for (int64 x = 0; x < (int64)dims.getRows(); x++) {
                for (int64 x = 0; x < colsOffset; x++) {
                    outputStream << delimiter;
                }
                std::wstring realPartStr = L"";
                std::wstring imagPartStr = L"";
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
            double* pValue = (double*)mat.getDataPointer();
            for (int64 x = 0; x < (int64)dims.getRows(); x++) {
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
                            std::wstring fmt;
                            if (y < ymax - 1) {
                                fmt = fmt_with_delimiter;
                            } else {
                                fmt = formatPrecision;
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
}
//=============================================================================
