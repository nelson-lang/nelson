// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2024-2026 xlnt-community
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
// @license: http://www.opensource.org/licenses/mit-license.php
// @author: see AUTHORS file

#include <ctime>

#include <xlnt/utils/date.hpp>
#include <detail/time_helpers.hpp>

/// Invalid weekday for checking whether std::mktime was successful - see below.
/// Must be outside of the range [0, 6].
constexpr int INVALID_WDAY = -1;

namespace xlnt {

date::date(int year_, int month_, int day_) : year(year_), month(month_), day(day_), _is_null(false)
{
}

date
date::from_number(int days_since_base_year, calendar base_date)
{
    date result(0, 0, 0);

    if (base_date == calendar::mac_1904) {
        days_since_base_year += 1462;
    }

    if (days_since_base_year == 60) {
        result.day = 29;
        result.month = 2;
        result.year = 1900;

        return result;
    } else if (days_since_base_year < 60) {
        days_since_base_year++;
    }

    int l = days_since_base_year + 68569 + 2415019;
    int n = int((4 * l) / 146097);
    l = l - int((146097 * n + 3) / 4);
    int i = int((4000 * (l + 1)) / 1461001);
    l = l - int((1461 * i) / 4) + 31;
    int j = int((80 * l) / 2447);
    result.day = l - int((2447 * j) / 80);
    l = int(j / 11);
    result.month = j + 2 - (12 * l);
    result.year = 100 * (n - 49) + i + l;

    return result;
}

bool
date::operator==(const date& comparand) const
{
    return year == comparand.year && month == comparand.month && day == comparand.day
        && _is_null == comparand._is_null;
}

bool
date::operator!=(const date& comparand) const
{
    return !(*this == comparand);
}

int
date::to_number(calendar base_date) const
{
    if (_is_null) {
        throw xlnt::invalid_attribute("cannot convert invalid/empty date to a number");
    }

    if (day == 29 && month == 2 && year == 1900) {
        return 60;
    }

    int days_since_1900 = int((1461 * (year + 4800 + int((month - 14) / 12))) / 4)
        + int((367 * (month - 2 - 12 * ((month - 14) / 12))) / 12)
        - int((3 * (int((year + 4900 + int((month - 14) / 12)) / 100))) / 4) + day - 2415019
        - 32075;

    if (days_since_1900 <= 60) {
        days_since_1900--;
    }

    if (base_date == calendar::mac_1904) {
        return days_since_1900 - 1462;
    }

    return days_since_1900;
}

date
date::today()
{
    optional<std::tm> now = detail::localtime_safe(std::time(nullptr));

    if (now.is_set()) {
        return date(1900 + now.get().tm_year, now.get().tm_mon + 1, now.get().tm_mday);
    } else {
        return date();
    }
}

int
date::weekday() const
{
    if (!_is_null) {
        std::tm tm = std::tm();
        tm.tm_wday = INVALID_WDAY;
        tm.tm_mday = day;
        tm.tm_mon = month - 1;
        tm.tm_year = year - 1900;

        // Important: if the conversion made by std::mktime is successful, the time object is
        // modified. All fields of time are updated to fit their proper ranges. time->tm_wday and
        // time->tm_yday are recalculated using information available in other fields. IMPORTANT:
        // the return value -1 could either be an error or mean 1 second before 1970-1-1. However,
        // an application wishing to check for error situations should set tm_wday to a value less
        // than 0 or greater than 6 before calling mktime(). On return, if tm_wday has not changed
        // an error has occurred.
        /*std::time_t time =*/std::mktime(&tm);

        if (tm.tm_wday != INVALID_WDAY) {
            return tm.tm_wday;
        } else {
            return -1;
        }
    } else {
        return -1;
    }
}

int
date::get_year() const
{
    if (_is_null) {
        throw xlnt::invalid_attribute("access to invalid/empty year of xlnt::date");
    }

    return year;
}

int
date::get_month() const
{
    if (_is_null) {
        throw xlnt::invalid_attribute("access to invalid/empty month of xlnt::date");
    }

    return month;
}

int
date::get_day() const
{
    if (_is_null) {
        throw xlnt::invalid_attribute("access to invalid/empty day of xlnt::date");
    }

    return day;
}

} // namespace xlnt
