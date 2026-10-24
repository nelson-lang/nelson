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
#include <system_error>
#include <fast_float/fast_float.h>
#include <xlnt/xlnt_config.hpp>
#include <detail/xlnt_config_impl.hpp>

/// The following parsers offer the same convenience as C++'s parsers (std::stoll, std::stod, etc.)
/// but do NOT require exception handling, resulting in improved performance and convenience to the
/// programmer (exception handling is not required everywhere).
namespace xlnt {
namespace detail {

    /// ----- INTERNAL FUNCTIONS -----
    namespace internal {
        static constexpr fast_float::chars_format FAST_FLOAT_FORMAT
            = fast_float::chars_format::general | fast_float::chars_format::allow_leading_plus
            | fast_float::chars_format::skip_white_space;
    }

    /// ----- INTEGER PARSING -----

    template <typename T,
        typename std::enable_if<fast_float::is_supported_integer_type<T>::value, bool>::type = true>
    std::errc
    parse(const char* string, T& result, const char** end = nullptr, int base = 10)
    {
        fast_float::parse_options options { internal::FAST_FLOAT_FORMAT, '.', base };

        auto parsing_result
            = fast_float::from_chars_int_advanced(string, string + strlen(string), result, options);

        if (end != nullptr) {
            *end = parsing_result.ptr;
        }

        return parsing_result.ec;
    }

    template <typename T,
        typename std::enable_if<fast_float::is_supported_integer_type<T>::value, bool>::type = true>
    std::errc
    parse(const std::string& string, T& result, std::size_t* num_characters_parsed = nullptr,
        int base = 10)
    {
        fast_float::parse_options options { internal::FAST_FLOAT_FORMAT, '.', base };

        auto parsing_result = fast_float::from_chars_int_advanced(
            string.c_str(), string.c_str() + string.length(), result, options);

        if (num_characters_parsed != nullptr) {
            *num_characters_parsed = parsing_result.ptr - string.c_str();
        }

        return parsing_result.ec;
    }

    /// ----- FLOATING-POINT NUMBER PARSING -----

    template <typename T,
        typename std::enable_if<fast_float::is_supported_float_type<T>::value, bool>::type = true>
    std::errc
    parse(const char* string, T& result, const char** end = nullptr, char decimal_separator = '.')
    {
        fast_float::parse_options options { internal::FAST_FLOAT_FORMAT, decimal_separator };

        auto parsing_result = fast_float::from_chars_float_advanced(
            string, string + strlen(string), result, options);

        if (end != nullptr) {
            *end = parsing_result.ptr;
        }

        return parsing_result.ec;
    }

    template <typename T,
        typename std::enable_if<fast_float::is_supported_float_type<T>::value, bool>::type = true>
    std::errc
    parse(const std::string& string, T& result, std::size_t* num_characters_parsed = nullptr,
        char decimal_separator = '.')
    {
        fast_float::parse_options options { internal::FAST_FLOAT_FORMAT, decimal_separator };

        auto parsing_result = fast_float::from_chars_float_advanced(
            string.c_str(), string.c_str() + string.length(), result, options);

        if (num_characters_parsed != nullptr) {
            *num_characters_parsed = parsing_result.ptr - string.c_str();
        }

        return parsing_result.ec;
    }

} // namespace detail
} // namespace xlnt
