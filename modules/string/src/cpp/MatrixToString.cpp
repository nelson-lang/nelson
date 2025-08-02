//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cstdio>
#include <cwchar>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "MatrixToString.hpp"
#include "StringHelpers.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
static std::wstring
complexToString(T complexValue, const std::wstring& formatNumber)
{
    std::wstring res;
    std::wstring realPartStr;
    std::wstring imagPartStr;
    if (std::isnan(complexValue.real())) {
        realPartStr = L"NaN";
    } else {
        if (std::isinf(complexValue.real())) {
            if (complexValue.real() > 0) {
                realPartStr = L"Inf";
            } else {
                realPartStr = L"-Inf";
            }
        } else {
            realPartStr = fmt::sprintf(formatNumber, complexValue.real());
        }
    }
    if (std::isnan(complexValue.imag())) {
        imagPartStr = L"NaN";
    } else {
        if (std::isinf(complexValue.imag())) {
            if (complexValue.imag() > 0) {
                imagPartStr = L"Inf";
            } else {
                imagPartStr = L"-Inf";
            }
        } else {
            imagPartStr = fmt::sprintf(formatNumber, complexValue.imag());
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
        if (complexValue.imag() > 0) {
            res = realPartStr + L"+" + imagPartStr + L"i";
        } else {
            res = realPartStr + imagPartStr + L"i";
        }
    }
    return res;
}
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
        switch (A.getDataClass()) {
        default: {
            Error(ERROR_TYPE_NOT_SUPPORTED);
        } break;
        case NLS_SCOMPLEX: {
            auto* pValue = (single*)A.getDataPointer();
            auto* pComplexValue = reinterpret_cast<singlecomplex*>(pValue);
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
                    singlecomplex complexVal = pComplexValue[x + y * dims.getRows()];
                    res = res + complexToString<singlecomplex>(complexVal, formatNumber) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_DCOMPLEX: {
            auto* pValue = (double*)A.getDataPointer();
            auto* pComplexValue = reinterpret_cast<doublecomplex*>(pValue);
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
                    doublecomplex complexVal = pComplexValue[x + y * dims.getRows()];
                    res = res + complexToString<doublecomplex>(complexVal, formatNumber) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_DOUBLE: {
            auto* pValue = (double*)A.getDataPointer();
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
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
                            res = res + fmt::sprintf(formatNumber, val) + L" ";
                        }
                    }
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_SINGLE: {
            auto* pValue = (single*)A.getDataPointer();
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
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
                            res = res + fmt::sprintf(formatNumber, val) + L" ";
                        }
                    }
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        case NLS_LOGICAL: {
            auto* pValue = (logical*)A.getDataPointer();
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
                    if (pValue[x + y * dims.getRows()] == 1) {
                        res = res + L"true" + L" ";
                    } else {
                        res = res + L"false" + L" ";
                    }
                }
                if (StringHelpers::ends_with(res, L" ")) {
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
            auto* pValue = (int64*)A.getDataPointer();
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
                    res = res + std::to_wstring(pValue[x + y * dims.getRows()]) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
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
            auto* pValue = (uint64*)A.getDataPointer();
            for (indexType x = 0; x < dims.getRows(); x++) {
                for (indexType y = 0; y < dims.getColumns(); y++) {
                    res = res + std::to_wstring(pValue[x + y * dims.getRows()]) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res = res + L";";
            }
        } break;
        }
        if (StringHelpers::ends_with(res, L";")) {
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
