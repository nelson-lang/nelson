//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <sys/utime.h>
#include <windows.h>
#else
#include <utime.h>
#endif
#include <ctime>
#include <filesystem>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>

#include "Zipper.hpp"
#include <mz_os.h>
#include <fstream>
#include "ZipHelpers.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isExistingDirectory(const std::wstring& name)
{
#ifdef _MSC_VER
    std::filesystem::path p = name;
#else
    std::filesystem::path p = wstring_to_utf8(name);
#endif
    bool res = false;
    try {
        res = std::filesystem::is_directory(p);
    } catch (const std::filesystem::filesystem_error&) {
        res = false;
    }
    return res;
}
//=============================================================================
bool
isExistingFile(const std::wstring& name)
{
#ifdef _MSC_VER
    std::filesystem::path p = name;
#else
    std::filesystem::path p = wstring_to_utf8(name);
#endif
    bool res = false;
    try {
        res = std::filesystem::is_regular_file(p);
    } catch (const std::filesystem::filesystem_error&) {
        res = false;
    }
    return res;
}
//=============================================================================
std::wstring
normalizePath(const std::wstring& path)
{
#ifdef _MSC_VER
    std::filesystem::path p = path;
#else
    std::filesystem::path p = wstring_to_utf8(path);
#endif
    p = p.lexically_normal();
    if (boost::algorithm::starts_with(p.wstring(), L"./")) {
        p = p.wstring().substr(2);
    }
    return p.generic_wstring();
}
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath)
{
    std::filesystem::path p;
    if (rootpath == L".") {
        p = std::filesystem::current_path();
    } else {
#ifdef _MSC_VER
        p = rootpath;
#else
        p = wstring_to_utf8(rootpath);
#endif
    }
    return normalizePath(p.wstring());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
