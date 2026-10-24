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
#include <functional>
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
#include "nlsDisplay_format_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Hook giving the compact representation of a function handle (@sin) or of a classdef object
// (deriving from nelson.mixin.CustomCompactDisplayProvider) shown inside a container
// (cell/struct). The interpreter registers the hook; display_format only calls it. Returns
// true and fills 'representation' when the value provides one.
NLSDISPLAY_FORMAT_IMPEXP void
setObjectCompactDisplayHook(std::function<bool(const ArrayOf&, std::wstring&)> hook);
//=============================================================================
bool
callObjectCompactDisplayHook(const ArrayOf& A, std::wstring& representation);
//=============================================================================
// Hook giving the dimensions a class reports through its size method (a duration or a
// table keeps its data in hidden properties). Registered by the interpreter.
NLSDISPLAY_FORMAT_IMPEXP void
setObjectDimensionsHook(std::function<bool(const ArrayOf&, Dimensions&)> hook);
//=============================================================================
// Hook giving the text of each element of an object whose class defines a hidden
// compactElementText method (duration, datetime, calendarDuration, categorical): a row
// of such an object is shown inside a container as its element texts ({[1 sec    2 sec]}).
// Registered by the interpreter.
NLSDISPLAY_FORMAT_IMPEXP void
setObjectElementTextsHook(std::function<bool(const ArrayOf&, wstringVector&)> hook);
//=============================================================================
bool
callObjectElementTextsHook(const ArrayOf& A, wstringVector& texts);
//=============================================================================
// Dimensions shown in a size summary ({2x1 duration}): the class size method for an
// object, the stored dimensions otherwise.
Dimensions
displayDimensions(const ArrayOf& A);
//=============================================================================
#define HORIZONTAL_ELLIPSIS L"\u2026"
#define BLANKS_BETWEEN L"   "
#define BLANKS_AT_BOL L"    "
#define LENGTH_BLANKS_AT_BOL std::wstring(BLANKS_AT_BOL).length()
#define DEFAULT_NOMINAL_WIDTH 10
//=============================================================================
// Assignment sign ending the "name =" line (and the "name(:,:,k) =" page line when isPage):
// strings, structs, missing values and objects are echoed "name = " with a trailing blank,
// numeric, char, logical, cell and function handle values "name =". Pages of a cell array
// follow the object form.
NLSDISPLAY_FORMAT_IMPEXP std::wstring
displayHeaderEqualSign(const ArrayOf& A, bool isPage = false);
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
summarizeStringArray(const ArrayOf& A, size_t beginningLineLength, size_t termWidth);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP std::wstring
summarizeCellEntry(const ArrayOf& A, size_t beginningLineLength, size_t termWidth,
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
