//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsCharacters_encoding_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
// convert UTF-8 string to wstring
NLSCHARACTERS_ENCODING_IMPEXP std::wstring
utf8_to_wstring(const std::string& str);
NLSCHARACTERS_ENCODING_IMPEXP std::wstring
utf8_to_wstring(const char* str);

// convert wstring to UTF-8 string
NLSCHARACTERS_ENCODING_IMPEXP std::string
wstring_to_utf8(const std::wstring& str);
NLSCHARACTERS_ENCODING_IMPEXP std::string
wstring_to_utf8(const wchar_t* str);
} // namespace Nelson
//=============================================================================
