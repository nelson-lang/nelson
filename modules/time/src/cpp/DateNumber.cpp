//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <math.h>
#include <iostream>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time.hpp>
#include <boost/date_time/gregorian/greg_date.hpp>
#include "characters_encoding.hpp"
#include "DateNumber.hpp"
#include "IsLeapYear.hpp"
#include "IEEEFP.hpp"
#include <iostream>
#include <boost/algorithm/string/replace.hpp>
//=============================================================================
namespace Nelson {
    //=============================================================================
    double DateNumber(double year, double month, double day, double hour, double minutes, double secondes)
    {
        if (!IsFinite(year) || !IsFinite(month) || !IsFinite(day) || !IsFinite(hour) || !IsFinite(minutes) || !IsFinite(secondes))
        {
            return nan("");
        }
        if (month < 1)
        {
            month = 1;
        }
        if (month > 12)
        {
            year += (month - 1) / 12;
            month = ((int)(month - 1) % 12) + 1;
        }
        double decimal_part = ((double)secondes * (1.0 / (24.0 * 3600.0))) + ((double)minutes *(1.0 / (24.0 * 60.0))) + ((double)hour *(1.0 / 24.0));
        // convert of month and day
        int integer_part = (int)(day + floor(((double)month * 3057 - 3007) / 100));
        // we remove 1 if mont is more than february
        integer_part = integer_part + (((double)month < 3) - 1);
        // we remove again 1 if month is more februar and it is not a leap year
        integer_part = (int)(integer_part + ((((double)month < 3) | (IsLeapYear((int)year))) - 1));
        if (IsLeapYear((int)year))
        {
            double leap_year_case = (double)year * 365 + ((double)year / 4) - floor((double)year / 100) + floor((double)year / 400);
            integer_part = (int)(integer_part + leap_year_case);
        }
        else
        {
            double not_leap_year_case = (double)year * 365 + floor((double)year / 4) + 1 - floor((double)year / 100) + floor((double)year / 400);
            integer_part = (int)(integer_part + not_leap_year_case);
        }
        return (double)(integer_part + decimal_part);
    }
    //=============================================================================
    double DateNumber(std::wstring datestring, std::wstring formatIn, bool &bParsed)
    {
        boost::algorithm::replace_all(formatIn, L"yyyy", L"%Y");
        boost::algorithm::replace_all(formatIn, L"yy", L"%y");
        boost::algorithm::replace_all(formatIn, L"mmm", L"%b");
        boost::algorithm::replace_all(formatIn, L"dd", L"%d");
        boost::algorithm::replace_all(formatIn, L"HH", L"%H");
        boost::algorithm::replace_all(formatIn, L"MM", L"%M");
        boost::algorithm::replace_all(formatIn, L"SS", L"%S");
        double res = nan("");
        bParsed = false;
        boost::posix_time::ptime pt(boost::posix_time::not_a_date_time);
        boost::posix_time::wtime_input_facet *ptrFacet = new boost::posix_time::wtime_input_facet(formatIn);
        std::locale format = std::locale(std::locale::classic(), ptrFacet);
        std::wistringstream is(datestring);
        is.imbue(format);
        is.exceptions(std::ios_base::failbit);
        try
        {
            is >> pt;
        }
        catch (const std::ios_base::failure &e)
        {
            return res;
        }
        if (pt != boost::posix_time::ptime())
        {
            boost::gregorian::date d = pt.date();
            if (!d.is_not_a_date())
            {
                double month = d.month().as_number();
                double year = d.year();
                double day = d.day().as_number();
                double hours = pt.time_of_day().hours();
                double minutes = pt.time_of_day().minutes();
                double secondes = pt.time_of_day().seconds();
                res = DateNumber(year, month, day, hours, minutes, secondes);
                bParsed = true;
            }
        }
        return res;
    }
    //=============================================================================
    double DateNumber(std::wstring datestring, bool &bParsed)
    {
        const std::locale formats[] =
        {
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%d-%b-%Y %H:%M:%S")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%d-%b-%Y")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%m/%d/%Y")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%m/%d/%y")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%m/%d")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%b.%m.%Y %H:%M:%S")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%b.%m,%Y")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%Y-%m-%d %H:%M:%S")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%Y-%m-%d")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%Y/%m/%d")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%H:%M:%S")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%H:%M:%S PM")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%H:%M")),
            std::locale(std::locale::classic(), new boost::posix_time::wtime_input_facet(L"%H:%M PM"))
        };
        const size_t formats_n = sizeof(formats) / sizeof(formats[0]);
        bParsed = false;
        boost::posix_time::ptime pt(boost::posix_time::not_a_date_time);
        for (size_t i = 0; i<formats_n; ++i)
        {
            std::wistringstream is(datestring);
            is.imbue(formats[i]);
            is >> pt;
            if (pt != boost::posix_time::ptime())
            {
                break;
            }
        }
        if (pt != boost::posix_time::ptime())
        {
            boost::gregorian::date d = pt.date();
            double month = d.month().as_number();
            double year = d.year();
            double day = d.day().as_number();
            double hours = pt.time_of_day().hours();
            double minutes = pt.time_of_day().minutes();
            double secondes = pt.time_of_day().seconds();
            double res = DateNumber(year, month, day, hours, minutes, secondes);
            bParsed = true;
            return res;
        }
        return nan("");
    }
    //=============================================================================

}
//=============================================================================
