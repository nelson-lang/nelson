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
#include "ArrayOf.hpp"
#include "Types.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
NelsonType
precisionFromString(const std::wstring& str, bool& bOK);
//=============================================================================
// fread precision: class read from the file and class returned ('src=>dst', '*src' keeps
// src, 'src' alone returns double).
bool
freadPrecision(const std::wstring& text, NelsonType& source, NelsonType& output);
//=============================================================================
// Character row vector or string scalar.
bool
isTextScalar(const ArrayOf& value);
//=============================================================================
std::wstring
toLowerText(std::wstring text);
//=============================================================================
// Interprets \n \r \t \b \f \v and \\ escapes; other backslashes are kept.
std::wstring
unescapeText(const std::wstring& text);
//=============================================================================
// Index of the choice equal to, or the only one starting with, text (case-insensitive); -1 if none.
int
matchChoicePrefix(const std::wstring& text, const wstringVector& lowerCaseChoices);
//=============================================================================
// Index of the choice named by a text value, or an error listing the choices.
size_t
matchOptionValue(const ArrayOf& value, const std::wstring& name,
    const wstringVector& lowerCaseChoices, const std::wstring& errorId);
//=============================================================================
} // namespace Nelson
//=============================================================================
