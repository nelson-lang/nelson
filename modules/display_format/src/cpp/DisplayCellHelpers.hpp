//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <type_traits>
#include "StringHelpers.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FormatHelpers.hpp"
#include "ComputeFormatInfo.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
summarizeCellMissingEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement);
//=============================================================================
std::wstring
summarizeCellObjectEntry(
    const ArrayOf& A, size_t beginningLineLength, size_t termWidth, bool asStructElement);
//=============================================================================
std::wstring
summarizeCellStringEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement);
//=============================================================================
std::wstring
summarizeCellLogicalEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement);
//=============================================================================
template <class T>
std::wstring
summarizeCellIntegerEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    std::wstring msg;
    if (A.isRowVector() || A.isScalar()) {
        const T* values = (static_cast<const T*>(A.dataPointer()));
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"[");
            }
        } else {
            msg.append(L"[");
        }
        FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
        formatInfo.trim = true;
        for (indexType k = 0; k < A.elementCount(); ++k) {
            std::wstring numberAsStr;
            switch (currentNumericFormat) {
            case NLS_NUMERIC_FORMAT_BANK:
            case NLS_NUMERIC_FORMAT_SHORTE:
            case NLS_NUMERIC_FORMAT_LONGE:
            case NLS_NUMERIC_FORMAT_SHORTG:
            case NLS_NUMERIC_FORMAT_LONGG:
            case NLS_NUMERIC_FORMAT_SHORTENG:
            case NLS_NUMERIC_FORMAT_LONGENG:
            case NLS_NUMERIC_FORMAT_LONG:
            case NLS_NUMERIC_FORMAT_RATIONAL:
            case NLS_NUMERIC_FORMAT_SHORT: {
                numberAsStr = std::to_wstring((T)values[k]);
            } break;
            case NLS_NUMERIC_FORMAT_PLUS:
            case NLS_NUMERIC_FORMAT_HEX: {
                numberAsStr = formatScalarNumber((double)values[k], false, formatInfo);
            } break;
            default: {
            } break;
            }
            msg.append(numberAsStr);
            if (k < A.elementCount() - 1) {
                msg.append(L" ");
            }
            if (msg.size() + 1 > termWidth) {
                if (asStructElement) {
                    return lightDescription(A, L"[", L"]");
                } else {
                    return lightDescription(A, L"", L"");
                }
            }
        }
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"]");
            }
        } else {
            msg.append(L"]");
        }
    } else {
        if (asStructElement) {
            msg = lightDescription(A, L"[", L"]");
        } else {
            msg = lightDescription(A, L"", L"");
        }
    }
    return msg;
}
//=============================================================================
template <class T>
std::wstring
summarizeCellRealEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    std::wstring msg;
    if (A.isSparse()) {
        if (asStructElement) {
            msg = lightDescription(A, L"[", L"]");
        } else {
            msg = lightDescription(A, L"", L"");
        }
    } else {
        if (A.isRowVector() || A.isScalar()) {
            const T* values = (static_cast<const T*>(A.dataPointer()));
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            // each element is displayed with its own scalar format: [1.5000 2]
            constexpr bool asSingle = std::is_same<T, single>::value;
            for (indexType k = 0; k < A.elementCount(); ++k) {
                FormatDisplayInformation formatInfo
                    = computeFormatInfo((double)values[k], asSingle, currentNumericFormat);
                formatInfo.trim = true;
                std::wstring numberAsStr
                    = formatScalarNumber((double)values[k], asSingle, formatInfo);
                if (currentNumericFormat == NLS_NUMERIC_FORMAT_RATIONAL) {
                    size_t nbCharsLimit = 6;
                    if (StringHelpers::contains(numberAsStr, L"/")) {
                        nbCharsLimit++;
                    }
                    if (StringHelpers::contains(numberAsStr, L"-")) {
                        nbCharsLimit++;
                    }
                    if (numberAsStr.length() > nbCharsLimit) {
                        numberAsStr = L"*";
                    }
                }
                msg.append(numberAsStr);
                if (k < A.elementCount() - 1) {
                    msg.append(L" ");
                }
                if (msg.size() + 1 > termWidth) {
                    if (asStructElement) {
                        return lightDescription(A, L"[", L"]");
                    } else {
                        return lightDescription(A, L"", L"");
                    }
                }
            }
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"]");
                }
            } else {
                msg.append(L"]");
            }
        } else {
            if (asStructElement) {
                msg = lightDescription(A, L"[", L"]");
            } else {
                msg = lightDescription(A, L"", L"");
            }
        }
    }
    return msg;
}
//=============================================================================
template <class T>
std::wstring
summarizeCellComplexEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    if (A.isSparse() || !(A.isRowVector() || A.isScalar())) {
        if (asStructElement) {
            return lightDescription(A, L"[", L"]");
        }
        return lightDescription(A, L"", L"");
    }
    const T* ap = static_cast<const T*>(A.dataPointer());
    constexpr bool asSingle = std::is_same<T, single>::value;
    std::wstring msg;
    for (indexType k = 0; k < A.elementCount(); ++k) {
        FormatDisplayInformation formatInfo = A.isScalar()
            ? computeFormatInfo(A, currentNumericFormat)
            : computeFormatInfo(
                  (double)ap[2 * k], (double)ap[2 * k + 1], asSingle, currentNumericFormat);
        formatInfo.trim = true;
        std::wstring numberAsStr = formatScalarComplexNumber(
            (double)ap[2 * k], (double)ap[2 * k + 1], asSingle, formatInfo);
        if (currentNumericFormat == NLS_NUMERIC_FORMAT_BANK) {
            size_t nbCharsLimit = (8 * 2) + 3;
            if (StringHelpers::contains(numberAsStr, L"/")) {
                nbCharsLimit++;
            }
            if (StringHelpers::contains(numberAsStr, L"-")) {
                nbCharsLimit++;
            }
            if (numberAsStr.length() > nbCharsLimit) {
                msg.clear();
                break;
            }
        }
        if (k > 0) {
            msg.append(L" ");
        }
        msg.append(numberAsStr);
        if (msg.size() + 3 > termWidth) {
            msg.clear();
            break;
        }
    }
    if (msg.empty()) {
        if (asStructElement) {
            return lightDescription(A, L"[", L"]");
        }
        return lightDescription(A, L"", L"");
    }
    if (asStructElement && A.isScalar()) {
        return msg;
    }
    return L"[" + msg + L"]";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
