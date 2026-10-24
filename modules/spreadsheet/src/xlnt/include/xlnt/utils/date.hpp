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

#include <xlnt/xlnt_config.hpp>
#include <xlnt/utils/calendar.hpp>

namespace xlnt {

/// <summary>
/// A date is a specific day specified in terms of a year, month, and day.
/// It can also be initialized as a number of days since a base date using date::from_number.
/// IMPORTANT: The date could be in an empty/invalid state, so you may want to call is_null() before
/// calling any functions!
/// </summary>
struct XLNT_API date
{
    /// <summary>
    /// Returns the current date according to the system time.
    /// If the current date could not be determined, the date will be in an empty state (is_null()
    /// will return true).
    /// </summary>
    static date
    today();

    /// <summary>
    /// Returns a date by adding days_since_base_year to base_date.
    /// This includes leap years.
    /// </summary>
    static date
    from_number(int days_since_base_year, calendar base_date);

    /// <summary>
    /// Constructs a date from a given year, month, and day.
    /// </summary>
    date(int year_, int month_, int day_);

    /// <summary>
    /// Constructs an empty date (a call to is_null() will return true).
    /// </summary>
    date() = default;

    /// <summary>
    /// Returns the number of days between this date and base_date. The date could be in an
    /// empty/invalid state, so you may want to call is_null() first! If this function is called
    /// when having an empty/invalid state, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    to_number(calendar base_date) const;

    /// <summary>
    /// Calculates and returns the day of the week that this date represents in the range
    /// 0 to 6 where 0 represents Sunday.
    /// Returns -1 if the weekday could not be determined.
    /// </summary>
    int
    weekday() const;

    /// <summary>
    /// Returns the year of the date. The date could be in an empty/invalid state, so you may want
    /// to call is_null() first! If this function is called when having an empty/invalid state, an
    /// xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_year() const;

    /// <summary>
    /// Returns the month of the date. The date could be in an empty/invalid state, so you may want
    /// to call is_null() first! If this function is called when having an empty/invalid state, an
    /// xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_month() const;

    /// <summary>
    /// Returns the day of the date. The date could be in an empty/invalid state, so you may want to
    /// call is_null() first! If this function is called when having an empty/invalid state, an
    /// xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    int
    get_day() const;

    /// <summary>
    /// Returns whether the date is in an empty/invalid state.
    /// </summary>
    bool
    is_null() const
    {
        return _is_null;
    }

    /// <summary>
    /// Returns true if this date is equal to comparand.
    /// </summary>
    bool
    operator==(const date& comparand) const;

    /// <summary>
    /// Returns true if this date is equal to comparand.
    /// </summary>
    bool
    operator!=(const date& comparand) const;

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

private:
    /// <summary>
    /// Whether the date is in an empty state.
    /// </summary>
    bool _is_null = true;
};

} // namespace xlnt
