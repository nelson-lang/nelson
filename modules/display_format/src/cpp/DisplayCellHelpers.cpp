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
summarizeCellLogicalEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
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
            const logical* values = (static_cast<const logical*>(A.dataPointer()));
            if (asStructElement) {
                if (!A.isScalar()) {
                    msg.append(L"[");
                }
            } else {
                msg.append(L"[");
            }
            for (indexType k = 0; k < A.elementCount(); ++k) {
                std::wstring logicalAsStr;
                if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
                    if (values[k]) {
                        logicalAsStr = L"+";
                    } else {
                        logicalAsStr = L" ";
                    }
                } else {
                    logicalAsStr = values[k] ? L"1" : L"0";
                }

                msg.append(logicalAsStr);
                if (k < A.elementCount() - 1) {
                    msg.append(L" ");
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
summarizeCellMissingEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
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
        for (indexType k = 0; k < A.elementCount(); ++k) {
            msg.append(L"<missing>");
            if (k < A.elementCount() - 1) {
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
        if (msg.length() >= termWidth - beginningLineLength) {
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
summarizeCellStringEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool asStructElement)
{
    std::wstring msg;
    if (A.isRowVector() || A.isScalar()) {
        ArrayOf* elements = (ArrayOf*)A.dataPointer();
        if (asStructElement) {
            if (!A.isScalar()) {
                msg.append(L"[");
            }
        } else {
            msg.append(L"[");
        }
        for (indexType k = 0; k < A.elementCount(); ++k) {
            if (elements[k].isCharacterArray()) {
                msg.append(L"\"" + elements[k].asWideString() + L"\"");
            } else {
                msg.append(L"<missing>");
            }
            if (k < A.elementCount() - 1) {
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
        if (msg.length() >= termWidth - beginningLineLength) {
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
/**
 * An object in a cell or a struct field: its compact representation when its class
 * provides one, the texts of its elements when it is a row of a class defining
 * compactElementText ([1 sec    2 sec]), its size summary otherwise.
 */
std::wstring
summarizeCellObjectEntry(
    const ArrayOf& A, size_t beginningLineLength, size_t termWidth, bool asStructElement)
{
    std::wstring msg;
    if (A.isScalar() && callObjectCompactDisplayHook(A, msg)) {
        return msg;
    }
    Dimensions dims = displayDimensions(A);
    wstringVector texts;
    bool isRow = dims.length() == 2 && dims.rows() == 1 && dims.columns() > 0;
    if (isRow && callObjectElementTextsHook(A, texts) && texts.size() == dims.columns()) {
        bool withBrackets = !asStructElement || texts.size() > 1;
        msg = withBrackets ? L"[" : L"";
        for (size_t k = 0; k < texts.size(); ++k) {
            if (k > 0) {
                msg.append(L"    ");
            }
            msg.append(texts[k]);
        }
        if (withBrackets) {
            msg.append(L"]");
        }
        if (msg.length() < termWidth - beginningLineLength) {
            return msg;
        }
    }
    return asStructElement ? lightDescription(A, L"[", L"]") : lightDescription(A, L"", L"");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
