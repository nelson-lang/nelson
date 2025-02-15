//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
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
summarizeCellStringEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement);
//=============================================================================
std::wstring
summarizeCellLogicalEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement);
//=============================================================================
template <class T>
std::wstring
summarizeCellIntegerEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    std::wstring msg;
    if (A.isRowVector() || A.isScalar()) {
        const T* values = (static_cast<const T*>(A.getDataPointer()));
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"[");
            }
        } else {
            msg.append(L"[");
        }
        FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
        formatInfo.trim = true;
        for (indexType k = 0; k < A.getElementCount(); ++k) {
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
            if (k < A.getElementCount() - 1) {
                msg.append(L"  ");
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
summarizeCellRealEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
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
            const T* values = (static_cast<const T*>(A.getDataPointer()));
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
            formatInfo.trim = true;
            for (indexType k = 0; k < A.getElementCount(); ++k) {
                std::wstring numberAsStr = formatScalarNumber((double)values[k], false, formatInfo);
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
                if (k < A.getElementCount() - 1) {
                    msg.append(L"  ");
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
summarizeCellComplexEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
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
            const double* ap = static_cast<const double*>(A.getDataPointer());
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            FormatDisplayInformation formatInfo = computeFormatInfo(A, currentNumericFormat);
            formatInfo.trim = true;
            for (indexType k = 0; k < A.getElementCount() * 2; k = k + 2) {
                std::wstring numberAsStr = formatScalarComplexNumber(
                    (double)ap[k], (double)ap[k + 1], false, formatInfo);
                if (currentNumericFormat == NLS_NUMERIC_FORMAT_BANK) {
                    size_t nbCharsLimit = (8 * 2) + 3;
                    if (StringHelpers::contains(numberAsStr, L"/")) {
                        nbCharsLimit++;
                    }
                    if (StringHelpers::contains(numberAsStr, L"-")) {
                        nbCharsLimit++;
                    }
                    if (numberAsStr.length() > nbCharsLimit) {
                        if (asStructElement) {
                            return lightDescription(A, L"[", L"]");
                        } else {
                            return lightDescription(A, L"", L"");
                        }
                    }
                }
                msg.append(numberAsStr);
                if (k < A.getElementCount() - 1) {
                    msg.append(L"  ");
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

}
//=============================================================================
