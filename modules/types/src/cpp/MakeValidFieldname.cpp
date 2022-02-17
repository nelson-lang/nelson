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
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include "MakeValidFieldname.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
MakeValidFieldname(const std::string& fieldname, const std::string& defaultPrefix)
{
    return wstring_to_utf8(
        MakeValidFieldname(utf8_to_wstring(fieldname), utf8_to_wstring(defaultPrefix)));
}
//=============================================================================
std::wstring
MakeValidFieldname(const std::wstring& fieldname, const std::wstring& defaultPrefix)
{
    if (fieldname.empty()) {
        return defaultPrefix;
    }
    std::wstring modifiedFieldname = fieldname;
    boost::wregex re(L"[^a-zA-Z_0-9]");
    modifiedFieldname = boost::regex_replace(fieldname, re, L"_");
    if (boost::algorithm::starts_with(modifiedFieldname, L"_")) {
        modifiedFieldname = defaultPrefix + modifiedFieldname;
    }
    if (iswdigit(modifiedFieldname[0])) {
        modifiedFieldname = defaultPrefix + L"_" + modifiedFieldname;
    }
    if (!iswalpha(modifiedFieldname[0])) {
        modifiedFieldname = defaultPrefix + modifiedFieldname;
    }
    return modifiedFieldname;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
