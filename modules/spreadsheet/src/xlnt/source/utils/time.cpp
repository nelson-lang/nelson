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
#include <cmath>
#include <cstdint>
#include <ctime>

#include <xlnt/utils/time.hpp>
#include <detail/time_helpers.hpp>
#include <detail/serialization/parsers.hpp>

namespace xlnt {

time
time::from_number(double raw_time)
{
    time result;

    double integer_part;
    double fractional_part = std::modf(static_cast<double>(raw_time), &integer_part);

    fractional_part *= 24;
    result.hour = static_cast<int>(fractional_part);
    fractional_part = 60 * (fractional_part - result.hour);
    result.minute = static_cast<int>(fractional_part);
    fractional_part = 60 * (fractional_part - result.minute);
    result.second = static_cast<int>(fractional_part);
    fractional_part = 1000000 * (fractional_part - result.second);
    result.microsecond = static_cast<int>(fractional_part);

    if (result.microsecond == 999999 && fractional_part - result.microsecond > 0.5) {
        result.microsecond = 0;
        result.second += 1;

        if (result.second == 60) {
            result.second = 0;
            result.minute += 1;

            // TODO: too much nesting
            if (result.minute == 60) {
                result.minute = 0;
                result.hour += 1;
            }
        }
    }

    return result;
}

time::time(int hour_, int minute_, int second_, int microsecond_)
    : hour(hour_), minute(minute_), second(second_), microsecond(microsecond_)
{
}

bool
time::operator==(const time& comparand) const
{
    return hour == comparand.hour && minute == comparand.minute && second == comparand.second
        && microsecond == comparand.microsecond;
}

bool
time::operator!=(const time& comparand) const
{
    return !(*this == comparand);
}

time::time(const std::string& time_string)
{
    bool ok = true;
    auto next_separator_index = time_string.find(':');
    next_separator_index = time_string.find(':');
    ok = ok && detail::parse(time_string.substr(0, next_separator_index), hour) == std::errc();
    auto previous_separator_index = next_separator_index;
    next_separator_index
        = ok ? time_string.find(':', previous_separator_index + 1) : next_separator_index;
    ok = ok
        && detail::parse(
               time_string.substr(previous_separator_index + 1, next_separator_index), minute)
            == std::errc();
    previous_separator_index = next_separator_index;
    next_separator_index
        = ok ? time_string.find('.', previous_separator_index + 1) : next_separator_index;
    bool subseconds_available = next_separator_index != std::string::npos;
    if (subseconds_available) {
        // First parse the seconds.
        ok = ok
            && detail::parse(
                   time_string.substr(previous_separator_index + 1, next_separator_index), second)
                == std::errc();
        previous_separator_index = next_separator_index;
    }
    next_separator_index = ok ? std::string::npos : next_separator_index;
    size_t num_characters_parsed = 0;
    ok = ok
        && detail::parse(time_string.substr(previous_separator_index + 1, next_separator_index),
               subseconds_available ? microsecond : second, &num_characters_parsed)
            == std::errc();

    if (subseconds_available) {
        constexpr size_t expected_digits = 6; // microseconds have 6 digits
        size_t actual_digits = num_characters_parsed;

        while (actual_digits > expected_digits) {
            microsecond /= 10;
            --actual_digits;
        }

        while (actual_digits < expected_digits) {
            microsecond *= 10;
            ++actual_digits;
        }
    }

    if (!ok) {
        throw xlnt::invalid_parameter("invalid ISO time \"" + time_string + "\"");
    }
}

double
time::to_number() const
{
    std::uint64_t microseconds = static_cast<std::uint64_t>(microsecond);
    microseconds += static_cast<std::uint64_t>(second * 1e6);
    microseconds += static_cast<std::uint64_t>(minute * 1e6 * 60);
    auto microseconds_per_hour = static_cast<std::uint64_t>(1e6) * 60 * 60;
    microseconds += static_cast<std::uint64_t>(hour) * microseconds_per_hour;
    auto number
        = static_cast<double>(microseconds) / (24.0 * static_cast<double>(microseconds_per_hour));
    number = std::floor(number * 100e9 + 0.5) / 100e9;

    return number;
}

time
time::now()
{
    optional<std::tm> now = detail::localtime_safe(std::time(nullptr));

    if (now.is_set()) {
        return time(now.get().tm_hour, now.get().tm_min, now.get().tm_sec);
    } else {
        return time();
    }
}

} // namespace xlnt
