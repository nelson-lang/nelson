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
#include <vector>
#include "nlsCommons_exports.h"
//=============================================================================
namespace Nelson::StringHelpers {
//=============================================================================
#define TOWSTRING_(x) L##x
#define TOWSTRING(x) TOWSTRING_(x)
//=============================================================================
NLSCOMMONS_IMPEXP bool
starts_with(const std::wstring& mainStr, const std::wstring& toMatch);
//=============================================================================
NLSCOMMONS_IMPEXP bool
starts_with(const std::string& mainStr, const std::string& toMatch);
//=============================================================================
NLSCOMMONS_IMPEXP bool
istarts_with(const std::wstring& mainStr, const std::wstring& toMatch);
//=============================================================================
NLSCOMMONS_IMPEXP bool
ends_with(const std::wstring& mainStr, const std::wstring& toMatch);
//=============================================================================
NLSCOMMONS_IMPEXP bool
ends_with(const std::string& mainStr, const std::string& toMatch);
//=============================================================================
NLSCOMMONS_IMPEXP bool
iends_with(const std::wstring& mainStr, const std::wstring& toMatch);
//=============================================================================
NLSCOMMONS_IMPEXP void
to_upper(std::wstring& str);
//=============================================================================
NLSCOMMONS_IMPEXP void
to_lower(std::wstring& str);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
to_lower_copy(const std::wstring& str);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
to_upper_copy(const std::wstring& str);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
to_upper_copy(const std::string& str);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
to_lower_copy(const std::string& str);
//=============================================================================
NLSCOMMONS_IMPEXP bool
replace_first(std::wstring& str, const std::wstring& Search, const std::wstring& Format);
//=============================================================================
NLSCOMMONS_IMPEXP bool
replace_first(std::string& str, const std::string& Search, const std::string& Format);
//=============================================================================
NLSCOMMONS_IMPEXP bool
replace_last(std::wstring& str, const std::wstring& Search, const std::wstring& Format);
//=============================================================================
NLSCOMMONS_IMPEXP bool
replace_last(std::string& str, const std::string& Search, const std::string& Format);
//=============================================================================
NLSCOMMONS_IMPEXP void
replace_all(std::wstring& str, const std::wstring& Search, const std::wstring& Format);
//=============================================================================
NLSCOMMONS_IMPEXP void
replace_all(std::string& str, const std::string& Search, const std::string& Format);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
replace_all_copy(const std::wstring& str, const std::wstring& Search, const std::wstring& Format);
//=============================================================================
NLSCOMMONS_IMPEXP void
split(std::vector<std::wstring>& result, const std::wstring& s, wchar_t separator);
//=============================================================================
NLSCOMMONS_IMPEXP bool
iequals(const std::wstring& s1, const std::wstring& s2);
//=============================================================================
NLSCOMMONS_IMPEXP bool
iequals(const std::string& s1, const std::string& s2);
//=============================================================================
NLSCOMMONS_IMPEXP bool
str2integer(const std::wstring& str, int& value);
//=============================================================================
NLSCOMMONS_IMPEXP bool
str2longlong(const std::wstring& str, long long& value);
//=============================================================================
NLSCOMMONS_IMPEXP bool
contains(const std::wstring& in, const std::wstring& needle);
//=============================================================================
NLSCOMMONS_IMPEXP bool
contains(const std::string& in, const std::string& needle);
//=============================================================================
NLSCOMMONS_IMPEXP bool
icontains(const std::wstring& in, const std::wstring& needle);
//=============================================================================
NLSCOMMONS_IMPEXP void
trim(std::wstring& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP void
trim_left(std::wstring& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP void
trim_right(std::wstring& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP void
trim(std::string& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
trim_copy(const std::wstring& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
trim_copy(const std::string& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP void
trim_left(std::string& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP void
trim_right(std::string& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
trim_right_copy(const std::wstring& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
trim_right_copy(const std::string& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
trim_left_copy(const std::wstring& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
trim_left_copy(const std::string& in_out);
//=============================================================================
NLSCOMMONS_IMPEXP void
erase_first(std::string& input, const std::string& search);
//=============================================================================
NLSCOMMONS_IMPEXP void
erase_first(std::wstring& input, const std::wstring& search);
//=============================================================================
NLSCOMMONS_IMPEXP void
erase_all(std::string& input, const std::string& search);
//=============================================================================
NLSCOMMONS_IMPEXP void
erase_all(std::wstring& input, const std::wstring& search);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
erase_all_copy(const std::wstring& input, const std::wstring& search);
//=============================================================================
NLSCOMMONS_IMPEXP std::string
join(const std::vector<std::string>& inputs, const std::string& separator);
//=============================================================================
NLSCOMMONS_IMPEXP std::wstring
join(const std::vector<std::wstring>& inputs, const std::wstring& separator);
//=============================================================================

}
//=============================================================================
