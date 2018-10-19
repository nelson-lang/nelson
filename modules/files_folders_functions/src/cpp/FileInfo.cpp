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
#define _CRT_SECURE_NO_WARNINGS
#include "FileInfo.hpp"
#include "DateNumber.hpp"
#include "characters_encoding.hpp"
#include <boost/date_time/c_local_time_adjustor.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/date_time/local_time_adjustor.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
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
FileInfo::FileInfo(const std::wstring& _filename, bool fullpath)
{
    boost::filesystem::path _path = _filename;
    // uniformize path separator
    _path = _path.generic_wstring();
    this->filename = _path.generic_wstring();
    if (fullpath) {
        this->name = _path.wstring();
    } else {
        this->name = _path.filename().wstring();
    }
    try {
        this->isdir = (bool)boost::filesystem::is_directory(_path);
    } catch (const boost::filesystem::filesystem_error&) {
        this->isdir = false;
    }
    if (this->isdir) {
        this->bytes = 0;
    } else {
        try {
            this->bytes = (double)boost::filesystem::file_size(_path);
        } catch (const boost::filesystem::filesystem_error&) {
            this->bytes = -1;
        }
    }
    try {
        std::time_t t = boost::filesystem::last_write_time(_path);
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
    this->filename = std::wstring();
    this->name = std::wstring();
    this->date = std::wstring();
    this->isdir = false;
    this->bytes = -1;
    this->datenum = -1;
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
}
//=============================================================================
