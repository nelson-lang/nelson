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
#if _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NON_CONFORMING_SWPRINTFS
#endif
#include <boost/algorithm/string.hpp>
#include <math.h>
#include <stdio.h>
#include <wchar.h>
#include "MatrixToString.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
MatrixToString(ArrayOf A, indexType precision, bool withClass)
{
    std::wstring res;
    if (!A.is2D()) {
        Error(_W("A 2D matrix expected."));
    }
    std::wstring formatNumber = L"%." + std::to_wstring(precision) + L"g";
    std::wstring class_name = L"";
    ClassName(A, class_name);
    if (withClass) {
        res = class_name + L"(";
    }
    Dimensions dims = A.getDimensions();
    if (A.isEmpty()) {
        if (A.isEmpty(true)) {
            res = res + L"[]";
        } else {
            res = res + L"zeros(" + std::to_wstring(dims.getRows()) + L","
                + std::to_wstring(dims.getColumns()) + L")";
        }
    } else {
        if (!A.isScalar()) {
            res = res + L"[";
        }
        wchar_t buffer[1024];
        switch (A.getDataClass()) {
        default: {
            Error(ERROR_TYPE_NOT_SUPPORTED);
        } break;
        case NLS_SCOMPLEX: {
            single* pValue = (single*)A.getDataPointer();
            singlecomplex* pComplexValue = reinterpret_cast<singlecomplex*>(pValue);
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    singlecomplex complexVal = pComplexValue[x + y * dims.getRows()];
                    std::wstring realPartStr;
                    std::wstring imagPartStr;
                    if (std::isnan(complexVal.real())) {
                        realPartStr = L"NaN";
                    } else {
                        if (std::isinf(complexVal.real())) {
                            if (complexVal.real() > 0) {
                                realPartStr = L"Inf";
                            } else {
                                realPartStr = L"-Inf";
                            }
                        } else {
                            swprintf(buffer, 1024, formatNumber.c_str(), complexVal.real());
                            realPartStr = buffer;
                        }
                    }
                    if (std::isnan(complexVal.imag())) {
                        imagPartStr = L"NaN";
                    } else {
                        if (std::isinf(complexVal.imag())) {
                            if (complexVal.imag() > 0) {
                                imagPartStr = L"Inf";
                            } else {
                                imagPartStr = L"-Inf";
                            }
                        } else {
                            swprintf(buffer, 1024, formatNumber.c_str(), complexVal.imag());
                            imagPartStr = buffer;
                        }
                    }
                    if (imagPartStr.compare(L"NaN") == 0 || imagPartStr.compare(L"Inf") == 0
                        || imagPartStr.compare(L"-Inf") == 0) {
                        if (imagPartStr.compare(L"-Inf") == 0) {
                            res = realPartStr + L"-1i*" + L"Inf";
                        } else {
                            res = realPartStr + L"+1i*" + imagPartStr;
                        }
                    } else {
                        if (complexVal.imag() > 0) {
                            res = realPartStr + L"+" + imagPartStr + L"i";
                        } else {
                            res = realPartStr + imagPartStr + L"i";
                        }
                    }
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_DCOMPLEX: {
            double* pValue = (double*)A.getDataPointer();
            doublecomplex* pComplexValue = reinterpret_cast<doublecomplex*>(pValue);
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    doublecomplex complexVal = pComplexValue[x + y * dims.getRows()];
                    std::wstring realPartStr;
                    std::wstring imagPartStr;
                    if (std::isnan(complexVal.real())) {
                        realPartStr = L"NaN";
                    } else {
                        if (std::isinf(complexVal.real())) {
                            if (complexVal.real() > 0) {
                                realPartStr = L"Inf";
                            } else {
                                realPartStr = L"-Inf";
                            }
                        } else {
                            swprintf(buffer, 1024, formatNumber.c_str(), complexVal.real());
                            realPartStr = buffer;
                        }
                    }
                    if (std::isnan(complexVal.imag())) {
                        imagPartStr = L"NaN";
                    } else {
                        if (std::isinf(complexVal.imag())) {
                            if (complexVal.imag() > 0) {
                                imagPartStr = L"Inf";
                            } else {
                                imagPartStr = L"-Inf";
                            }
                        } else {
                            swprintf(buffer, 1024, formatNumber.c_str(), complexVal.imag());
                            imagPartStr = buffer;
                        }
                    }
                    if (imagPartStr.compare(L"NaN") == 0 || imagPartStr.compare(L"Inf") == 0
                        || imagPartStr.compare(L"-Inf") == 0) {
                        if (imagPartStr.compare(L"-Inf") == 0) {
                            res = realPartStr + L"-1i*" + L"Inf";
                        } else {
                            res = realPartStr + L"+1i*" + imagPartStr;
                        }
                    } else {
                        if (complexVal.imag() > 0) {
                            res = realPartStr + L"+" + imagPartStr + L"i";
                        } else {
                            res = realPartStr + imagPartStr + L"i";
                        }
                    }
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_DOUBLE: {
            double* pValue = (double*)A.getDataPointer();
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    double val = pValue[x + y * dims.getRows()];
                    if (std::isnan(val)) {
                        res = res + L"NaN" + L" ";
                    } else {
                        if (std::isinf(val)) {
                            if (val > 0) {
                                res = res + L"Inf" + L" ";
                            } else {
                                res = res + L"-Inf" + L" ";
                            }
                        } else {
                            swprintf(buffer, 1024, formatNumber.c_str(), val);
                            res = res + buffer + L" ";
                        }
                    }
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_SINGLE: {
            single* pValue = (single*)A.getDataPointer();
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    single val = pValue[x + y * dims.getRows()];
                    if (std::isnan(val)) {
                        res = res + L"NaN" + L" ";
                    } else {
                        if (std::isinf(val)) {
                            if (val > 0) {
                                res = res + L"Inf" + L" ";
                            } else {
                                res = res + L"-Inf" + L" ";
                            }
                        } else {
                            swprintf(buffer, 1024, formatNumber.c_str(), val);
                            res = res + buffer + L" ";
                        }
                    }
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_LOGICAL: {
            logical* pValue = (logical*)A.getDataPointer();
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    if (pValue[x + y * dims.getRows()] == 1) {
                        res = res + L"true" + L" ";
                    } else {
                        res = res + L"false" + L" ";
                    }
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64: {
            A.promoteType(NLS_INT64);
            int64* pValue = (int64*)A.getDataPointer();
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    res = res + std::to_wstring(pValue[x + y * dims.getRows()]) + L" ";
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64: {
            A.promoteType(NLS_UINT64);
            uint64* pValue = (uint64*)A.getDataPointer();
            for (size_t x = 0; x < dims.getRows(); x++) {
                for (size_t y = 0; y < dims.getColumns(); y++) {
                    res = res + std::to_wstring(pValue[x + y * dims.getRows()]) + L" ";
                }
                if (boost::algorithm::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        }
        if (boost::algorithm::ends_with(res, L";")) {
            res.pop_back();
        }
        if (!A.isScalar()) {
            res = res + L"]";
        }
    }
    if (withClass) {
        res = res + L")";
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
