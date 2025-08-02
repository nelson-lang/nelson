//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DisplayCellHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
summarizeCellLogicalEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
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
            const logical* values = (static_cast<const logical*>(A.getDataPointer()));
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            for (indexType k = 0; k < A.getElementCount(); ++k) {
                std::wstring logicalAsStr;
                if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                    if (values[k]) {
                        logicalAsStr = L"+";
                    } else {
                        logicalAsStr = L" ";
                    }
                } else {
                    if (values[k]) {
                        logicalAsStr = L"true";
                    } else {
                        logicalAsStr = L"false";
                    }
                }

                msg.append(logicalAsStr);
                if (k < A.getElementCount() - 1) {
                    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                        msg.append(L" ");
                    } else {
                        msg.append(L"  ");
                    }
                }
                if (msg.size() + 1 > termWidth) {
                    if (asStructElement) {
                        return lightDescription(A, L"[", L"]");
                    }
                    return lightDescription(A, L"", L"");
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
std::wstring
summarizeCellMissingEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    std::wstring msg;
    if (A.isRowVector() || A.isScalar()) {
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"[");
            }
        } else {
            msg.append(L"[");
        }
        for (indexType k = 0; k < A.getElementCount(); ++k) {
            msg.append(L"<missing>");
            if (k < A.getElementCount() - 1) {
                msg.append(L"    ");
            }
        }
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"]");
            }
        } else {
            msg.append(L"]");
        }
        if (msg.length() >= termWidth - beginingLineLength) {
            if (asStructElement) {
                msg = lightDescription(A, L"[", L"]");
            } else {
                msg = lightDescription(A, L"", L"");
            }
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
std::wstring
summarizeCellStringEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    std::wstring msg;
    if (A.isRowVector() || A.isScalar()) {
        ArrayOf* elements = (ArrayOf*)A.getDataPointer();
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"[");
            }
        } else {
            msg.append(L"[");
        }
        for (indexType k = 0; k < A.getElementCount(); ++k) {
            if (elements[k].isCharacterArray()) {
                msg.append(L"\"" + elements[k].getContentAsWideString() + L"\"");
            } else {
                msg.append(L"<missing>");
            }
            if (k < A.getElementCount() - 1) {
                msg.append(L"    ");
            }
        }
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"]");
            }
        } else {
            msg.append(L"]");
        }
        if (msg.length() >= termWidth - beginingLineLength) {
            if (asStructElement) {
                msg = lightDescription(A, L"[", L"]");
            } else {
                msg = lightDescription(A, L"", L"");
            }
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
}
//=============================================================================
