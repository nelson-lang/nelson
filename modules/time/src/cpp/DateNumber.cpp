//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "DateNumber.hpp"
#include "IEEEFP.hpp"
#include "IsLeapYear.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/date_time.hpp>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
double
DateNumber(double year, double month, double day, double hour, double minutes, double secondes)
{
    if (!IsFinite(year) || !IsFinite(month) || !IsFinite(day) || !IsFinite(hour)
        || !IsFinite(minutes) || !IsFinite(secondes)) {
        return nan("");
    }
    if (month < 1) {
        month = 1;
    }
    if (month > 12) {
        year += (month - 1) / 12;
        month = (static_cast<int>(month - 1) % 12) + 1;
    }
    double decimal_part = (secondes * (1.0 / (24.0 * 3600.0))) + (minutes * (1.0 / (24.0 * 60.0)))
        + (hour * (1.0 / 24.0));
    // convert of month and day
    int integer_part = static_cast<int>(day + floor((month * 3057 - 3007) / 100));
    // we remove 1 if mont is more than february
    integer_part = integer_part + (static_cast<int>(month < 3) - 1);
    // we remove again 1 if month is more februar and it is not a leap year
    integer_part = (integer_part
        + ((static_cast<int>(month < 3) | static_cast<int>(IsLeapYear(static_cast<int>(year))))
              - 1));
    if (IsLeapYear(static_cast<int>(year))) {
        double leap_year_case = year * 365 + (year / 4) - floor(year / 100) + floor(year / 400);
        integer_part = static_cast<int>(integer_part + leap_year_case);
    } else {
        double not_leap_year_case
            = year * 365 + floor(year / 4) + 1 - floor(year / 100) + floor(year / 400);
        integer_part = static_cast<int>(integer_part + not_leap_year_case);
    }
    return (integer_part + decimal_part);
}
//=============================================================================
double
DateNumber(const std::wstring& datestring, const std::wstring& formatIn, bool& bParsed)
{
    std::wstring _formatIn(formatIn);
    boost::algorithm::replace_all(_formatIn, L"yyyy", L"%Y");
    boost::algorithm::replace_all(_formatIn, L"yy", L"%y");
    boost::algorithm::replace_all(_formatIn, L"mmm", L"%b");
    boost::algorithm::replace_all(_formatIn, L"dd", L"%d");
    boost::algorithm::replace_all(_formatIn, L"HH", L"%H");
    boost::algorithm::replace_all(_formatIn, L"MM", L"%M");
    boost::algorithm::replace_all(_formatIn, L"SS", L"%ls");
    double res = nan("");
    bParsed = false;
    boost::posix_time::ptime pt(boost::posix_time::not_a_date_time);
    boost::posix_time::wtime_input_facet* ptrFacet
        = new boost::posix_time::wtime_input_facet(_formatIn);
    std::locale format = std::locale(std::locale::classic(), ptrFacet);
    std::wistringstream is(datestring);
    is.imbue(format);
    is.exceptions(std::ios_base::failbit);
    try {
        is >> pt;
    } catch (const std::ios_base::failure&) {
        return res;
    }
    if (pt != boost::posix_time::ptime()) {
        boost::gregorian::date d = pt.date();
        if (!d.is_not_a_date()) {
            double month = (double)d.month().as_number();
            double year = (double)d.year();
            double day = (double)d.day().as_number();
            double hours = (double)pt.time_of_day().hours();
            double minutes = (double)pt.time_of_day().minutes();
            double secondes = (double)pt.time_of_day().seconds();
            res = DateNumber(year, month, day, hours, minutes, secondes);
            bParsed = true;
        }
    }
    return res;
}
//=============================================================================
double
DateNumber(const std::wstring& datestring, bool& bParsed)
{
    const std::locale formats_without_date[] = {
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%H:%M:%S")),
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%H:%M")),
    };
    const std::locale formats_without_time[] = {
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%m/%d/%Y")),
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%m/%d/%y")),
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%Y/%m/%d")),
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%d-%b-%Y")),
        std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%Y-%m-%d")),
    };
    const std::locale formats_date_time[] = {
        std::locale(
            std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%d-%b-%Y %H:%M:%S")),
        std::locale(
            std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%b.%m.%Y %H:%M:%S")),
        std::locale(
            std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%Y-%m-%d %H:%M:%S")),
    };
    // bool haveAMPM = boost::algorithm::contains(datestring, L" AM") ||
    // boost::algorithm::contains(datestring, L" PM");
    bool haveAM = boost::algorithm::contains(datestring, L" AM");
    bool havePM = boost::algorithm::contains(datestring, L" PM");
    std::wstring _datestring(datestring);
    if (haveAM) {
        boost::replace_all(_datestring, L" AM", "");
    }
    if (havePM) {
        boost::replace_all(_datestring, L" PM", "");
    }
    size_t count_time_separator = std::count(datestring.begin(), datestring.end(), L':');
    size_t count_date_separator_0 = std::count(datestring.begin(), datestring.end(), L',');
    size_t count_date_separator_1 = std::count(datestring.begin(), datestring.end(), L'.');
    size_t count_date_separator_2 = std::count(datestring.begin(), datestring.end(), L'/');
    size_t count_date_separator_3 = std::count(datestring.begin(), datestring.end(), L'-');
    bParsed = false;
    if (count_date_separator_0 == 1 && count_date_separator_1 == 1) {
        bParsed = true;
        return nan("");
    }
    bool is_without_date = (count_time_separator > 0)
        && ((count_date_separator_1 != 3) || (count_date_separator_2 != 3)
               || (count_date_separator_3 != 3));
    bool is_without_time = (count_time_separator == 0);
    bool is_with_date_time = count_time_separator > 0;
    boost::posix_time::ptime pt(boost::posix_time::not_a_date_time);
    if (is_without_date) {
        boost::posix_time::ptime t(boost::posix_time::second_clock::local_time());
        boost::gregorian::date currentdate = t.date();
        const size_t formats_n = sizeof(formats_without_date) / sizeof(formats_without_date[0]);
        for (const auto& i : formats_without_date) {
            std::wistringstream is(datestring);
            is.imbue(i);
            is >> pt;
            if (pt != boost::posix_time::ptime()) {
                break;
            }
        }
        if (pt != boost::posix_time::ptime()) {
            boost::gregorian::date d = pt.date();
            double year = currentdate.year();
            double month = 1;
            double day = 1;
            double hours = (double)pt.time_of_day().hours();
            if (havePM) {
                if (hours < 12) {
                    hours = hours + 12;
                }
            }
            double minutes = (double)pt.time_of_day().minutes();
            double secondes = (double)pt.time_of_day().seconds();
            double res = DateNumber(year, month, day, hours, minutes, secondes);
            bParsed = true;
            return res;
        }
    }
    if (is_without_time) {
        if (count_date_separator_2 == 1) {
            std::locale format = std::locale(
                std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%m/%d"));
            std::wistringstream is(datestring);
            is.imbue(format);
            is >> pt;
            if (pt != boost::posix_time::ptime()) {
                boost::posix_time::ptime t(boost::posix_time::second_clock::local_time());
                boost::gregorian::date currentdate = t.date();
                boost::gregorian::date d = pt.date();
                double year = currentdate.year();
                double month = d.month().as_number();
                double day = d.day().as_number();
                double hours = 0;
                double minutes = 0;
                double secondes = 0;
                double res = DateNumber(year, month, day, hours, minutes, secondes);
                bParsed = true;
                return res;
            }
        } else {
            const size_t formats_n = sizeof(formats_without_time) / sizeof(formats_without_time[0]);
            for (const auto& i : formats_without_time) {
                std::wistringstream is(datestring);
                is.imbue(i);
                is >> pt;
                if (pt != boost::posix_time::ptime()) {
                    break;
                }
            }
        }
        if (pt != boost::posix_time::ptime()) {
            boost::gregorian::date d = pt.date();
            double year = d.year();
            double month = d.month().as_number();
            double day = d.day().as_number();
            double hours = 0;
            double minutes = 0;
            double secondes = 0;
            double res = DateNumber(year, month, day, hours, minutes, secondes);
            bParsed = true;
            return res;
        }
    }
    if (is_with_date_time) {
        const size_t formats_n = sizeof(formats_date_time) / sizeof(formats_date_time[0]);
        for (const auto& i : formats_date_time) {
            std::wistringstream is(datestring);
            is.imbue(i);
            is >> pt;
            if (pt != boost::posix_time::ptime()) {
                break;
            }
        }
        if (pt != boost::posix_time::ptime()) {
            boost::gregorian::date d = pt.date();
            double year = (double)d.year();
            double month = (double)d.month().as_number();
            double day = (double)d.day().as_number();
            double hours = (double)pt.time_of_day().hours();
            double minutes = (double)pt.time_of_day().minutes();
            double secondes = (double)pt.time_of_day().seconds();
            double res = DateNumber(year, month, day, hours, minutes, secondes);
            bParsed = true;
            return res;
        }
    }

    return nan("");
}
//=============================================================================

} // namespace Nelson
//=============================================================================
