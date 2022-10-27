//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include "FileSystemWrapper.hpp"
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
    FileSystemWrapper::Path previous_pwd = FileSystemWrapper::Path::current_path();
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
    std::string errorMessage;
    FileSystemWrapper::Path::current_path(pathApplied, errorMessage);
    if (errorMessage.empty()) {
        PathFuncManager::getInstance()->setCurrentUserPath(
            FileSystemWrapper::Path::current_path().generic_wstring());
        return true;
    } else {
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
