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

#pragma once

#include <string>

#include <xlnt/xlnt_config.hpp>
#include <xlnt/utils/calendar.hpp>

namespace xlnt {

struct date;
struct time;

/// <summary>
/// A datetime is a combination of a date and a time.
/// IMPORTANT: The datetime could be in an empty/invalid state, so you may want to call is_null()
/// before calling any functions!
/// </summary>
struct XLNT_API datetime
{
    /// <summary>
    /// Returns the current date and time according to the system time.
    /// If the current date could not be determined, the date will be in an empty state (is_null()
    /// will return true).
    /// </summary>
    static datetime
    now();

    /// <summary>
    /// Returns the current date and time according to the system time.
    /// This is equivalent to datetime::now().
    /// If the current date could not be determined, the date will be in an empty state (is_null()
    /// will return true).
    /// </summary>
    static datetime
    today();

    /// <summary>
    /// Returns a datetime from number by converting the integer part into
    /// a date and the fractional part into a time according to date::from_number
    /// and time::from_number.
    /// </summary>
    static datetime
    from_number(double number, calendar base_date);

    /// <summary>
    /// Returns a datetime equivalent to the ISO-formatted string iso_string.
    /// If the string could not be parsed correctly, an xlnt::invalid_parameter exception is thrown.
    /// </summary>
    static datetime
    from_iso_string(const std::string& iso_string);

    /// <summary>
    /// Constructs a datetime from a date and a time.
    /// </summary>
    datetime(const date& d, const time& t);

    /// <summary>
    /// Constructs a datetime from a year, month, and day plus optional hour, minute, second, and
    /// microsecond.
    /// </summary>
    datetime(int year_, int month_, int day_, int hour_ = 0, int minute_ = 0, int second_ = 0,
        int microsecond_ = 0);

    /// <summary>
    /// Constructs an empty datetime (a call to is_null() will return true).
    /// </summary>
    datetime() = default;

    /// <summary>
    /// Returns a string represenation of this date and time. The date could be in an empty/invalid
    /// state, so you may want to call is_null() first! If this function is called when having an
    /// empty/invalid state, an empty string will be returned.
    /// </summary>
    std::string
    to_string() const;

    /// <summary>
    /// Returns an ISO-formatted string representation of this date and time. The date could be in
    /// an empty/invalid state, so you may want to call is_null() first! If this function is called
    /// when having an empty/invalid state, an empty string will be returned.
    /// </summary>
    std::string
    to_iso_string() const;

    /// <summary>
    /// Returns this datetime as a number of seconds since 1900 or 1904 (depending on base_date
    /// provided). The date could be in an empty/invalid state, so you may want to call is_null()
    /// first! If this function is called when having an empty/invalid state, an
    /// xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    double
    to_number(calendar base_date) const;

    /// <summary>
    /// Returns true if this datetime is equivalent to comparand.
    /// </summary>
    bool
    operator==(const datetime& comparand) const;

    /// <summary>
    /// Returns true if this datetime is different than comparand.
    /// </summary>
    bool
    operator!=(const datetime& comparand) const;

    /// <summary>
    /// Calculates and returns the day of the week that this date represents in the range
    /// 0 to 6 where 0 represents Sunday.
    /// Returns -1 if the weekday could not be determined.
    /// </summary>
    int
    weekday() const;

    /// <summary>
    /// Returns the year of the datetime. The datetime could be in an empty/invalid state, so you
    /// may want to call is_null() first! If this function is called when having an empty/invalid
    /// state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_year() const;

    /// <summary>
    /// Returns the month of the datetime. The datetime could be in an empty/invalid state, so you
    /// may want to call is_null() first! If this function is called when having an empty/invalid
    /// state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_month() const;

    /// <summary>
    /// Returns the day of the datetime. The datetime could be in an empty/invalid state, so you may
    /// want to call is_null() first! If this function is called when having an empty/invalid state,
    /// an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_day() const;

    /// <summary>
    /// Returns the hour of the datetime. The datetime could be in an empty/invalid state, so you
    /// may want to call is_null() first! If this function is called when having an empty/invalid
    /// state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_hour() const;

    /// <summary>
    /// Returns the minute of the datetime. The datetime could be in an empty/invalid state, so you
    /// may want to call is_null() first! If this function is called when having an empty/invalid
    /// state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_minute() const;

    /// <summary>
    /// Returns the second of the datetime. The datetime could be in an empty/invalid state, so you
    /// may want to call is_null() first! If this function is called when having an empty/invalid
    /// state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_second() const;

    /// <summary>
    /// Returns the microsecond of the datetime. The datetime could be in an empty/invalid state, so
    /// you may want to call is_null() first! If this function is called when having an
    /// empty/invalid state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_microsecond() const;

    /// <summary>
    /// Returns whether the date is in an empty/invalid state.
    /// </summary>
    bool
    is_null() const
    {
        return _is_null;
    }

    /// ----- TODO IMPORTANT: accessing the members directly is DEPRECATED and will be changed in a
    /// further release! Please use the getters instead!

    /// <summary>
    /// The year
    /// </summary>
    int year = 0;

    /// <summary>
    /// The month
    /// </summary>
    int month = 0;

    /// <summary>
    /// The day
    /// </summary>
    int day = 0;

    /// <summary>
    /// The hour
    /// </summary>
    int hour = 0;

    /// <summary>
    /// The minute
    /// </summary>
    int minute = 0;

    /// <summary>
    /// The second
    /// </summary>
    int second = 0;

    /// <summary>
    /// The microsecond
    /// </summary>
    int microsecond = 0;

private:
    /// <summary>
    /// Whether the date is in an empty state.
    /// </summary>
    bool _is_null = true;
};

} // namespace xlnt
