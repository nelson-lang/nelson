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
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include "ChangeDirectory.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
removeSimpleQuotesAndTrim(const std::wstring& newpath)
{
    std::wstring cleanedLine = boost::algorithm::trim_copy(newpath);
    if (boost::algorithm::starts_with(cleanedLine, L"'")
        && boost::algorithm::ends_with(cleanedLine, L"'")) {
        boost::algorithm::replace_first(cleanedLine, L"'", L"");
        boost::algorithm::replace_last(cleanedLine, L"'", L"");
        boost::algorithm::trim(cleanedLine);
    }
    return cleanedLine;
}
//=============================================================================
ArrayOf
Cd(const std::wstring& newpath)
{
    boost::filesystem::path previous_pwd = boost::filesystem::current_path();
    ChangeDirectory(newpath, true, true);
    return ArrayOf::characterArrayConstructor(previous_pwd.generic_wstring());
}
//=============================================================================
ArrayOf
Cd(const std::string& newpath)
{
    return Cd(utf8_to_wstring(newpath));
}
//=============================================================================
bool
ChangeDirectory(const std::wstring& newpath, bool doException, bool trimPath)
{
    std::wstring pathApplied = newpath;
    if (trimPath) {
        pathApplied = removeSimpleQuotesAndTrim(newpath);
    }
    try {
        boost::filesystem::current_path(pathApplied);
        PathFuncManager::getInstance()->setCurrentUserPath(
            boost::filesystem::current_path().generic_wstring());
        return true;
    } catch (const boost::filesystem::filesystem_error&) {
        if (doException) {
            std::wstring msg
                = str(boost::wformat(_W("Cannot change directory '%s'.")) % pathApplied);
            Error(msg);
        }
    }
    return false;
}
//=============================================================================
bool
ChangeDirectory(const std::string& newpath, bool doException, bool trimPath)
{
    std::wstring wpath = utf8_to_wstring(newpath);
    return ChangeDirectory(wpath, doException, trimPath);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
