//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include <boost/date_time/c_local_time_adjustor.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/date_time/local_time_adjustor.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "FileSystemWrapper.hpp"
#include "FileInfo.hpp"
#include "DateNumber.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::posix_time::ptime
local_ptime_from_utc_time_t(std::time_t const t)
{
    using boost::date_time::c_local_adjustor;
    using boost::posix_time::from_time_t;
    using boost::posix_time::ptime;
    return c_local_adjustor<ptime>::utc_to_local(from_time_t(t));
}
//=============================================================================
FileInfo::FileInfo(const std::wstring& filename)
{
    Nelson::FileSystemWrapper::Path _path(filename);
    // uniformize path separator
    _path = _path.generic_wstring();
    this->folder = _path.parent_path().wstring();
    this->name = _path.filename().wstring();
    try {
        this->isdir = (bool)Nelson::FileSystemWrapper::Path::is_directory(_path);
    } catch (const boost::filesystem::filesystem_error&) {
        this->isdir = false;
    }
    if (this->isdir) {
        this->bytes = 0;
    } else {
        try {
            this->bytes = (double)Nelson::FileSystemWrapper::Path::file_size(_path);
        } catch (const boost::filesystem::filesystem_error&) {
            this->bytes = -1;
        }
    }
    try {
        std::time_t t = Nelson::FileSystemWrapper::Path::last_write_time(_path);
        boost::posix_time::ptime pt = local_ptime_from_utc_time_t(t);
        int day = pt.date().day();
        int month = pt.date().month();
        int year = pt.date().year();
        auto hms = pt.time_of_day();
        int h = (int)hms.hours();
        int m = (int)hms.minutes();
        int s = (int)hms.seconds();
        this->date = boost::posix_time::to_simple_wstring(pt);
        this->datenum = DateNumber(year, month, day, h, m, s);
    } catch (const boost::filesystem::filesystem_error&) {
        this->date = std::wstring();
        this->datenum = -1;
    }
}
//=============================================================================
FileInfo::~FileInfo()
{
    this->folder = std::wstring();
    this->name = std::wstring();
    this->date = std::wstring();
    this->isdir = false;
    this->bytes = -1;
    this->datenum = -1;
}
//=============================================================================
std::wstring
FileInfo::getFolder()
{
    return this->folder;
}
//=============================================================================
std::wstring
FileInfo::getName()
{
    return this->name;
}
//=============================================================================
std::wstring
FileInfo::getDate()
{
    return this->date;
}
//=============================================================================
bool
FileInfo::isDir()
{
    return this->isdir;
}
//=============================================================================
double
FileInfo::getBytes()
{
    return this->bytes;
}
//=============================================================================
double
FileInfo::getDatenum()
{
    return this->datenum;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
