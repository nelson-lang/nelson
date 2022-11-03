//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <regex>
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "FileCompleter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DIR_SEPARATOR_WINDOWS L'\\'
#define DIR_SEPARATOR_OTHERS L'/'
//=============================================================================
static void
splitpath(const std::wstring& prefix, std::wstring& path, std::wstring& fname)
{
    const wchar_t* lastslash = nullptr;
    int lastslash_pos = -1;
    for (size_t k = 0; k < prefix.size(); ++k) {
#ifdef _MSC_VER
        if ((prefix[k] == DIR_SEPARATOR_OTHERS) || (prefix[k] == DIR_SEPARATOR_WINDOWS)) {
            lastslash_pos = static_cast<int>(k);
        }
#else
        if (prefix[k] == DIR_SEPARATOR_OTHERS) {
            lastslash_pos = k;
        }
#endif
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
        if (pathname.empty()) {
            try {
                FileSystemWrapper::Path pwd = FileSystemWrapper::Path::current_path();
                path = pwd.generic_wstring();
            } catch (const nfs::filesystem_error&) {
            }
        } else {
            path = pathname;
        }
        if (path.empty()) {
#ifdef _MSC_VER
            path = std::wstring() + DIR_SEPARATOR_WINDOWS;
#else
            path = std::wstring() + DIR_SEPARATOR_OTHERS;
#endif
        } else {
            wchar_t ch = *path.rbegin();
            bool isSeparator = (ch == DIR_SEPARATOR_WINDOWS || ch == DIR_SEPARATOR_OTHERS);
            if (!isSeparator) {
#ifdef _MSC_VER
                path.push_back(DIR_SEPARATOR_WINDOWS);
#else
                path.push_back(DIR_SEPARATOR_OTHERS);
#endif
            }
        }
        filespec = filename + L"*";
        std::wstring mask = path + filespec;
        nfs::path pathfs(mask);
        nfs::path branch(pathfs.parent_path());
        if (branch.empty()) {
            branch = nfs::current_path();
        }
        if (nfs::is_directory(branch)) {
            mask = pathfs.filename().wstring();
            StringHelpers::replace_all(mask, L".", L"\\.");
            StringHelpers::replace_all(mask, L"?", L".");
            StringHelpers::replace_all(mask, L"*", L".*");
            std::wregex rmask(mask, std::wregex::icase);
            {
                nfs::path dir = branch;
                nfs::path r = dir.root_path();
                if (FileSystemWrapper::Path::is_directory(branch.wstring())) {
                    try {
                        for (nfs::directory_iterator p(branch), end; p != end; ++p) {
                            if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                                continue;
                            }
                            std::wstring file(p->path().wstring());
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            nfs::path current = file;
                            std::wstring fname = current.wstring();
                            if (FileSystemWrapper::Path::is_directory(fname)) {
                                fname = fname + L"/";
                            }
                            std::wstring complet;
#ifdef _MSC_VER
                            if ((*prefix.rbegin() == L'/') || (*prefix.rbegin() == L'\\'))
#else
                            if ((*prefix.rbegin() == L'/'))
#endif
                            {
                                complet
                                    = fname.substr(prefix.size(), fname.size() - prefix.size() + 1);
                            } else {
                                size_t pos = std::wstring::npos;
#ifdef _MSC_VER
                                size_t pos1 = prefix.rfind(L'/');
                                size_t pos2 = prefix.rfind(L'\\');
                                if (pos1 != std::wstring::npos && pos2 != std::wstring::npos) {
                                    pos = std::max(pos1, pos2);
                                } else {
                                    if (pos1 != std::wstring::npos) {
                                        pos = pos1;
                                    } else {
                                        pos = pos2;
                                    }
                                }
#else
                                pos = prefix.rfind(L'/');
#endif
                                if (pos != std::wstring::npos) {
                                    complet = fname.substr(pos + 1);
                                } else {
                                    complet
                                        = fname.substr(path.size(), fname.size() - path.size() + 1);
                                }
                            }
                            if (!complet.empty()) {
                                res.push_back(complet);
                            }
                        }
                    } catch (const nfs::filesystem_error&) {
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
