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
#include <Windows.h>
#include <sys/utime.h>
#else
#include <sys/stat.h>
#include <sys/types.h>
#include <utime.h>
#endif
#include <ctime>
#include "characters_encoding.hpp"
#include "UnzipHelpers.hpp"
#include "ZipHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
changeFileDate(const std::wstring& filename, struct tm dateFile, uint32_t dosDate)
{
#ifdef _MSC_VER
    struct _utimbuf ut;
#else
    struct utimbuf ut;
#endif
    struct tm newdate;
    newdate.tm_sec = dateFile.tm_sec;
    newdate.tm_min = dateFile.tm_min;
    newdate.tm_hour = dateFile.tm_hour;
    newdate.tm_mday = dateFile.tm_mday;
    newdate.tm_mon = dateFile.tm_mon;
    if (dateFile.tm_year > 1900) {
        newdate.tm_year = dateFile.tm_year - 1900;
    } else {
        newdate.tm_year = dateFile.tm_year;
    }
    newdate.tm_isdst = -1;

    ut.actime = ut.modtime = mktime(&newdate);

#ifdef _MSC_VER
    _wutime(filename.c_str(), &ut);
#else
    utime(wstring_to_utf8(filename).c_str(), &ut);
#endif
}
//=============================================================================
void
changeFileOrFolderAttributes(const std::wstring& filename, uint32_t attributes)
{
#ifdef _MSC_VER
    SetFileAttributesW(filename.c_str(), attributes);
#else
    // chmod(wstring_to_utf8(filename).c_str(), (mode_t)attributes);
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
