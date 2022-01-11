//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <boost/algorithm/string.hpp>
#include "Types.hpp"
#include "ArrayOf.hpp"
#include "NelsonConfiguration.hpp"
#include "DisplayVariableHelpers.hpp"
#include "FormatHelpers.hpp"
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
            indexType len = 1;
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            for (indexType k = 0; k < A.getElementCount(); ++k) {
                std::wstring numberAsStr
                    = formatScalarNumber(values[k], false, currentNumericFormat, true);
                if (currentNumericFormat == NLS_NUMERIC_FORMAT_RATIONAL) {
                    size_t nbCharsLimit = 6;
                    if (boost::contains(numberAsStr, L"/")) {
                        nbCharsLimit++;
                    }
                    if (boost::contains(numberAsStr, L"-")) {
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
            indexType len = 1;
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            for (indexType k = 0; k < A.getElementCount() * 2; k = k + 2) {
                std::wstring numberAsStr = formatScalarComplexNumber(
                    ap[k], ap[k + 1], false, currentNumericFormat, true);
                if (currentNumericFormat == NLS_NUMERIC_FORMAT_BANK) {
                    size_t nbCharsLimit = (8 * 2) + 3;
                    if (boost::contains(numberAsStr, L"/")) {
                        nbCharsLimit++;
                    }
                    if (boost::contains(numberAsStr, L"-")) {
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
