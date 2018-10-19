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
#include "FileCompleter.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/regex.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
#define DIR_SEPARATOR_WINDOWS L'\\'
#define DIR_SEPARATOR_OTHERS L'/'
//=============================================================================
static bool
IsDirectory(std::wstring str)
{
    boost::filesystem::path data_dir(str);
    bool bRes = false;
    try {
        bRes = boost::filesystem::is_directory(data_dir);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            // ONLY FOR DEBUG
        }
        bRes = false;
    }
    return bRes;
}
//=============================================================================
static void
splitpath(std::wstring& prefix, std::wstring& path, std::wstring& fname)
{
    const wchar_t* lastslash = nullptr;
    const wchar_t* p2 = prefix.c_str();
    int lastslash_pos = -1;
    for (size_t k = 0; k < prefix.size(); ++k) {
#ifdef _MSC_VER
        if ((prefix[k] == DIR_SEPARATOR_OTHERS) || (prefix[k] == DIR_SEPARATOR_WINDOWS)) {
            lastslash_pos = (int)k;
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
        path = L"";
        fname = prefix;
    }
}
//=============================================================================
wstringVector
FileCompleter(std::wstring prefix)
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
                boost::filesystem::path pwd = boost::filesystem::current_path();
                path = pwd.generic_wstring();
            } catch (const boost::filesystem::filesystem_error&) {
            }
        } else {
            path = pathname;
        }
        if (path.empty()) {
#ifdef _MSC_VER
            path = L"" + DIR_SEPARATOR_WINDOWS;
#else
            path = L"" + DIR_SEPARATOR_OTHERS;
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
        boost::filesystem::path pathfs(mask);
        boost::filesystem::path branch(pathfs.branch_path());
        if (branch.empty()) {
            branch = boost::filesystem::current_path();
        }
        if (boost::filesystem::is_directory(branch)) {
            mask = pathfs.leaf().wstring();
            static const std::pair<boost::wregex, const wchar_t*> repl[] = {
                std::pair<boost::wregex, const wchar_t*>(boost::wregex(L"\\."), L"\\\\."),
                std::pair<boost::wregex, const wchar_t*>(boost::wregex(L"\\?"), L"."),
                std::pair<boost::wregex, const wchar_t*>(boost::wregex(L"\\*"), L".*"),
            };
            for (const std::pair<boost::wregex, const wchar_t*>* r = repl;
                 r < repl + sizeof(repl) / sizeof(*repl); ++r) {
                mask = boost::regex_replace(mask, r->first, r->second);
            }
            boost::wregex rmask(mask, boost::wregex::icase);
            {
                boost::filesystem::path dir = branch;
                boost::filesystem::path r = dir.root_path();
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (boost::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                            if (!boost::regex_match(p->path().leaf().wstring(), rmask)) {
                                continue;
                            } else {
                                std::wstring file(p->path().wstring());
                                if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                    file = std::wstring(file.begin() + 2, file.end());
                                }
                                boost::filesystem::path current = file;
                                std::wstring fname = current.wstring();
                                if (IsDirectory(fname)) {
                                    fname = fname + L"/";
                                }
                                std::wstring complet;
#ifdef _MSC_VER
                                if ((*prefix.rbegin() == L'/') || (*prefix.rbegin() == L'\\'))
#else
                                if ((*prefix.rbegin() == L'/'))
#endif
                                {
                                    complet = fname.substr(
                                        prefix.size(), fname.size() - prefix.size() + 1);
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
                                        complet = fname.substr(
                                            path.size(), fname.size() - path.size() + 1);
                                    }
                                }
                                if (!complet.empty()) {
                                    res.push_back(complet);
                                }
                            }
                        }
                    } catch (const boost::filesystem::filesystem_error&) {
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
