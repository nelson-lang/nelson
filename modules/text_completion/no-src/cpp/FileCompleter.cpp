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
#ifdef _MSC_VER
#define DIR_SEPARATOR_WINDOWS L'\\'
#define DIR_SEPARATOR_OTHERS L'/'
//=============================================================================
static void
splitpath(const std::wstring& prefix, std::wstring& path, std::wstring& fname)
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
static std::wstring
completionEntryFromPath(const std::filesystem::path& path)
{
    std::wstring entry = path.filename().wstring();
    if (std::filesystem::is_directory(path)) {
        entry.push_back(DIR_SEPARATOR_OTHERS);
    }
    return entry;
}
//=============================================================================
wstringVector
FileCompleter(const std::wstring& prefix)
{
    wstringVector res;
    if (!prefix.empty()) {
        std::wstring path;
        std::wstring filespec;
        std::wstring pathname;
        std::wstring filename;
        splitpath(prefix, pathname, filename);
        bool prefixHasPath = !pathname.empty();
        if (pathname.empty()) {
            try {
                std::filesystem::path pwd = std::filesystem::current_path();
                path = pwd.generic_wstring();
            } catch (const std::filesystem::filesystem_error&) {
            }
        } else {
            path = pathname;
        }
        if (path.empty()) {
            path = std::wstring() + DIR_SEPARATOR_WINDOWS;
        } else {
            wchar_t ch = *path.rbegin();
            bool isSeparator = (ch == DIR_SEPARATOR_WINDOWS || ch == DIR_SEPARATOR_OTHERS);
            if (!isSeparator) {
                path.push_back(DIR_SEPARATOR_WINDOWS);
            }
        }
        filespec = filename + L"*";
        std::wstring mask = path + filespec;
        std::filesystem::path pathfs(mask);
        std::filesystem::path branch(pathfs.parent_path());
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

            std::filesystem::path dir = branch;
            std::filesystem::path r = dir.root_path();
            if (std::filesystem::is_directory(branch.wstring())) {
                try {
                    for (std::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                        if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                            continue;
                        }
                        std::wstring complet = completionEntryFromPath(p->path());
                        if (!complet.empty()) {
                            if (!prefixHasPath && StringHelpers::starts_with(complet, prefix)
                                && StringHelpers::iends_with(complet, L".m")) {
                                complet = complet.substr(0, complet.size() - 2);
                            }
                            res.push_back(complet);
                        }
                    }
                } catch (const std::filesystem::filesystem_error&) {
                }
            }
        }
    }
    return res;
}
//=============================================================================
#else
#define DIR_SEPARATOR_WINDOWS '\\'
#define DIR_SEPARATOR_OTHERS '/'
//=============================================================================
static void
splitpath(const std::string& prefix, std::string& path, std::string& fname)
{
    int lastslash_pos = -1;
    for (size_t k = 0; k < prefix.size(); ++k) {
        if (prefix[k] == DIR_SEPARATOR_OTHERS) {
            lastslash_pos = k;
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
static std::string
completionEntryFromPath(const std::filesystem::path& path)
{
    std::string entry = path.filename().string();
    if (std::filesystem::is_directory(path)) {
        entry.push_back(DIR_SEPARATOR_OTHERS);
    }
    return entry;
}
//=============================================================================
wstringVector
FileCompleter(const std::wstring& prefix)
{
    wstringVector res;
    if (!prefix.empty()) {
        std::string path;
        std::string filespec;
        std::string pathname;
        std::string filename;
        std::string prefixString = wstring_to_utf8(prefix);
        splitpath(prefixString, pathname, filename);
        bool prefixHasPath = !pathname.empty();
        if (pathname.empty()) {
            try {
                std::filesystem::path pwd = std::filesystem::current_path();
                path = pwd.generic_string();
            } catch (const std::filesystem::filesystem_error&) {
            }
        } else {
            path = pathname;
        }
        if (path.empty()) {
            path = std::string() + DIR_SEPARATOR_WINDOWS;
        } else {
            char ch = *path.rbegin();
            bool isSeparator = (ch == DIR_SEPARATOR_WINDOWS || ch == DIR_SEPARATOR_OTHERS);
            if (!isSeparator) {
                path.push_back(DIR_SEPARATOR_WINDOWS);
            }
        }
        filespec = filename + "*";
        std::string mask = path + filespec;
        std::filesystem::path pathfs(mask);
        std::filesystem::path branch(pathfs.parent_path());
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
                std::regex _rmask(mask, std::wregex::icase);
                rmask = _rmask;
            } catch (std::regex_error&) {
                return res;
            }
            std::filesystem::path dir = branch;
            std::filesystem::path r = dir.root_path();
            if (std::filesystem::is_directory(branch.string())) {
                try {
                    for (std::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                        if (!std::regex_match(p->path().filename().string(), rmask)) {
                            continue;
                        }
                        std::string complet = completionEntryFromPath(p->path());
                        if (!complet.empty()) {
                            if (!prefixHasPath && StringHelpers::starts_with(complet, prefixString)
                                && StringHelpers::iends_with(complet, ".m")) {
                                complet = complet.substr(0, complet.size() - 2);
                            }
                            res.push_back(utf8_to_wstring(complet));
                        }
                    }
                } catch (const std::filesystem::filesystem_error&) {
                }
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
