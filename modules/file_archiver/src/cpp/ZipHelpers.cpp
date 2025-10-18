//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <sys/utime.h>
#else
#include <utime.h>
#endif
#include <ctime>
#include "Zipper.hpp"
#include <mz_os.h>
#include <fstream>
#include "FileSystemWrapper.hpp"
#include "ZipHelpers.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
normalizeZipPath(const std::wstring& path)
{
    FileSystemWrapper::Path p(path);
    p = p.generic_path().lexically_normal();
    if (StringHelpers::starts_with(p.wstring(), L"./")) {
        p = p.wstring().substr(2);
    }
    return p.generic_wstring();
}
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath)
{
    FileSystemWrapper::Path p;
    if (rootpath == L".") {
        p = FileSystemWrapper::Path::current_path();
    } else {
        p = FileSystemWrapper::Path(rootpath);
    }
    return normalizeZipPath(p.wstring());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
