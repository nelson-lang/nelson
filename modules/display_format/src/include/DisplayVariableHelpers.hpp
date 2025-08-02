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
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
#include "nlsDisplay_format_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define HORIZONTAL_ELLIPSIS L"\u2026" // L"..."
#define BLANKS_BETWEEN L"   "
#define BLANKS_AT_BOL L"    "
#define LENGTH_BLANKS_AT_BOL std::wstring(BLANKS_AT_BOL).length()
#define DEFAULT_NOMINAL_WIDTH 10
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP void
DisplayVariableHeader(Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP void
DisplayVariableFooter(Interface* io, bool asDisp);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
completeWithBlanksAtTheEnd(const std::wstring& msg, size_t width);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
completeWithBlanksAtBeginning(const std::wstring& msg, size_t width);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
summarizeStringArray(const ArrayOf& A, size_t beginingLineLength, size_t termWidth);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
summarizeCellEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth,
    NumericFormatDisplay currentNumericFormat, bool recursive);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
lightDescription(const ArrayOf& A, const std::wstring& firstChar, const std::wstring& lastChar);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
columnsHeader(indexType startCol, indexType endCol);
//=============================================================================
} // namespace Nelson
//=============================================================================
