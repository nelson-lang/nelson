//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <regex>
#include <filesystem>
#include "StringHelpers.hpp"
#include "FileCompleter.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using PathType = std::filesystem::path;
#ifdef _MSC_VER
using StringType = std::wstring;
#define DIR_SEPARATOR_WINDOWS L'\\'
#define DIR_SEPARATOR_OTHERS L'/'
#define DOT_M L".m"
#else
using StringType = std::string;
#define DIR_SEPARATOR_WINDOWS '\\'
#define DIR_SEPARATOR_OTHERS '/'
#define DOT_M ".m"
#endif
//=============================================================================
static void
splitpath(const StringType& prefix, StringType& path, StringType& fname)
{
    int lastslash_pos = -1;
    for (size_t k = 0; k < prefix.size(); ++k) {
        if ((prefix[k] == DIR_SEPARATOR_OTHERS) || (prefix[k] == DIR_SEPARATOR_WINDOWS)) {
            lastslash_pos = static_cast<int>(k);
        }
    }
    if (lastslash_pos != -1) {
        path = prefix.substr(0, 1 + lastslash_pos);
        fname = prefix.substr(1 + lastslash_pos, prefix.size() - lastslash_pos);
    } else {
        path.clear();
        fname = prefix;
    }
}
//=============================================================================
#ifdef _MSC_VER
wstringVector
FileCompleter(const std::wstring& prefix)
{
    wstringVector res;
    if (prefix.empty()) {
        return res;
    }
    StringType path;
    StringType filespec;
    StringType pathname;
    StringType filename;
    splitpath(prefix, pathname, filename);
    if (pathname.empty()) {
        try {
            PathType pwd = std::filesystem::current_path();
            path = pwd.generic_wstring();
        } catch (const std::filesystem::filesystem_error&) {
        }
    } else {
        path = pathname;
    }
    if (path.empty()) {
        path = StringType() + DIR_SEPARATOR_WINDOWS;
    } else {
        wchar_t ch = *path.rbegin();
        bool isSeparator = (ch == DIR_SEPARATOR_WINDOWS || ch == DIR_SEPARATOR_OTHERS);
        if (!isSeparator) {
            path.push_back(DIR_SEPARATOR_WINDOWS);
        }
    }
    filespec = filename + L"*";
    StringType mask = path + filespec;
    PathType pathfs(mask);
    PathType branch(pathfs.parent_path());
    if (branch.empty()) {
        branch = std::filesystem::current_path();
    }
    if (std::filesystem::is_directory(branch)) {
        mask = pathfs.filename().wstring();
        StringHelpers::replace_all(mask, L".", L"\\.");
        StringHelpers::replace_all(mask, L"?", L".");
        StringHelpers::replace_all(mask, L"*", L".*");
        std::wregex rmask;
        try {
            std::wregex _rmask(mask, std::wregex::icase);
            rmask = _rmask;
        } catch (std::regex_error&) {
            return res;
        }

        PathType dir = branch;
        if (std::filesystem::is_directory(branch.wstring())) {
            try {
                for (std::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                    if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                        continue;
                    }
                    StringType file(p->path().wstring());
                    if (file[0] == L'.'
                        && (file[1] == DIR_SEPARATOR_OTHERS || file[1] == DIR_SEPARATOR_WINDOWS)) {
                        file = StringType(file.begin() + 2, file.end());
                    }
                    PathType current = file;
                    StringType fname = current.wstring();
                    if (std::filesystem::is_directory(fname)) {
                        fname = fname + L"/";
                    }
                    std::wstring complet;
                    if ((*prefix.rbegin() == DIR_SEPARATOR_OTHERS)
                        || (*prefix.rbegin() == DIR_SEPARATOR_WINDOWS)) {
                        complet = fname.substr(prefix.size(), fname.size() - prefix.size() + 1);
                    } else {
                        size_t pos = StringType::npos;
                        size_t pos1 = prefix.rfind(DIR_SEPARATOR_OTHERS);
                        size_t pos2 = prefix.rfind(DIR_SEPARATOR_WINDOWS);
                        if (pos1 != StringType::npos && pos2 != StringType::npos) {
                            pos = std::max(pos1, pos2);
                        } else {
                            if (pos1 != StringType::npos) {
                                pos = pos1;
                            } else {
                                pos = pos2;
                            }
                        }
                        if (pos != StringType::npos) {
                            complet = fname.substr(pos + 1);
                        } else {
                            complet = fname.substr(path.size(), fname.size() - path.size() + 1);
                        }
                    }
                    if (!complet.empty()) {
                        if (StringHelpers::starts_with(complet, prefix)
                            && StringHelpers::iends_with(complet, DOT_M)) {
                            complet = complet.substr(0, complet.size() - 2);
                        }
                        res.push_back(complet);
                    }
                }
            } catch (const std::filesystem::filesystem_error&) {
            }
        }
    }
    return res;
}
//=============================================================================
#else
//=============================================================================
wstringVector
FileCompleter(const std::wstring& prefix)
{
    wstringVector res;
    if (prefix.empty()) {
        return res;
    }
    StringType path;
    StringType filespec;
    StringType pathname;
    StringType filename;
    splitpath(wstring_to_utf8(prefix), pathname, filename);
    if (pathname.empty()) {
        try {
            PathType pwd = std::filesystem::current_path();
            path = pwd.generic_string();
        } catch (const std::filesystem::filesystem_error&) {
        }
    } else {
        path = pathname;
    }
    if (path.empty()) {
        path = StringType() + DIR_SEPARATOR_OTHERS;
    } else {
        char ch = *path.rbegin();
        bool isSeparator = (ch == DIR_SEPARATOR_WINDOWS || ch == DIR_SEPARATOR_OTHERS);
        if (!isSeparator) {
            path.push_back(DIR_SEPARATOR_OTHERS);
        }
    }
    filespec = filename + "*";
    StringType mask = path + filespec;
    PathType pathfs(mask);
    PathType branch(pathfs.parent_path());
    if (branch.empty()) {
        branch = std::filesystem::current_path();
    }
    if (std::filesystem::is_directory(branch)) {
        mask = pathfs.filename().string();
        StringHelpers::replace_all(mask, ".", "\\.");
        StringHelpers::replace_all(mask, "?", ".");
        StringHelpers::replace_all(mask, "*", ".*");
        std::regex rmask;
        try {
            std::regex _rmask(mask, std::regex::icase);
            rmask = _rmask;
        } catch (std::regex_error&) {
            return res;
        }
        PathType dir = branch;
        PathType r = dir.root_path();
        if (std::filesystem::is_directory(branch.string())) {
            try {
                for (std::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                    if (!std::regex_match(p->path().filename().string(), rmask)) {
                        continue;
                    }
                    StringType file(p->path().string());
                    if (file[0] == '.'
                        && (file[1] == DIR_SEPARATOR_OTHERS || file[1] == DIR_SEPARATOR_WINDOWS)) {
                        file = std::string(file.begin() + 2, file.end());
                    }
                    PathType current = file;
                    StringType fname = current.string();
                    if (std::filesystem::is_directory(fname)) {
                        fname = fname + "/";
                    }
                    StringType complet;
                    if ((*prefix.rbegin() == DIR_SEPARATOR_OTHERS)
                        || (*prefix.rbegin() == DIR_SEPARATOR_WINDOWS)) {
                        complet = fname.substr(prefix.size(), fname.size() - prefix.size() + 1);
                    } else {
                        size_t pos = std::string::npos;
                        size_t pos1 = prefix.rfind(DIR_SEPARATOR_OTHERS);
                        size_t pos2 = prefix.rfind(DIR_SEPARATOR_WINDOWS);
                        if (pos1 != std::string::npos && pos2 != std::string::npos) {
                            pos = std::max(pos1, pos2);
                        } else {
                            if (pos1 != std::string::npos) {
                                pos = pos1;
                            } else {
                                pos = pos2;
                            }
                        }
                        if (pos != std::string::npos) {
                            complet = fname.substr(pos + 1);
                        } else {
                            complet = fname.substr(path.size(), fname.size() - path.size() + 1);
                        }
                    }
                    if (!complet.empty()) {
                        if (StringHelpers::starts_with(complet, wstring_to_utf8(prefix))
                            && StringHelpers::iends_with(complet, DOT_M)) {
                            complet = complet.substr(0, complet.size() - 2);
                        }
                        res.push_back(utf8_to_wstring(complet));
                    }
                }
            } catch (const std::filesystem::filesystem_error&) {
            }
        }
    }
    return res;
}
//=============================================================================
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
