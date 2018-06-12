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
#include "MakeValidFieldname.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
MakeValidFieldname(std::string fieldname, std::string defaultPrefix)
{
    return wstring_to_utf8(
        MakeValidFieldname(utf8_to_wstring(fieldname), utf8_to_wstring(defaultPrefix)));
}
//=============================================================================
std::wstring
MakeValidFieldname(std::wstring fieldname, std::wstring defaultPrefix)
{
    if (!fieldname.size()) {
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
}
//=============================================================================
