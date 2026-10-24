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

// For compilers implementing the optional Annex K of C11.
// MUST be defined before including <ctime>
// See https://en.cppreference.com/w/c/header/time.html
#if defined(__STDC_LIB_EXT1__) && !defined(__STDC_WANT_LIB_EXT1__)
#define __STDC_WANT_LIB_EXT1__
#endif

#include <ctime>

#include <xlnt/utils/optional.hpp>
#include <xlnt/utils/environment.hpp>

namespace xlnt {
namespace detail {

    /// Converts given time since epoch (a time_t value) into calendar time, expressed in local
    /// time, in the struct tm format. If the conversion failed, an empty optional will be returned.
    inline optional<std::tm>
    localtime_safe(std::time_t raw_time)
    {
        optional<std::tm> returned_value;

#ifdef _MSC_VER
        // MSVC introduced localtime_s before the optional Annex K of C11 was standardized, with
        // swapped parameters.
        std::tm result {};
        errno_t err = localtime_s(&result, &raw_time);
        if (err == 0) {
            returned_value = result;
        }
#elif XLNT_HAS_C_VERSION(XLNT_C_23)                                                                \
    || /* Feature test macros from https://linux.die.net/man/3/localtime_r */                      \
    _POSIX_C_SOURCE >= 1 || _XOPEN_SOURCE || _BSD_SOURCE || _SVID_SOURCE || _POSIX_SOURCE

        // For C23 (used by C++26) compilers, and POSIX-compatible platforms.
        std::tm result {};
        if (localtime_r(&raw_time, &result)) {
            returned_value = result;
        }
#elif defined(__STDC_LIB_EXT1__) && defined(__STDC_WANT_LIB_EXT1__)
        // For compilers implementing the optional Annex K of C11
        std::tm result {};
        if (localtime_s(&raw_time, &result)) {
            returned_value = result;
        }
#else
        std::tm* tm = std::localtime(&raw_time);

        if (tm != nullptr) {
            returned_value = *tm;
        }
#endif

        return returned_value;
    }

    /// Converts given time since epoch (a time_t value) into calendar time, expressed in
    /// Coordinated Universal Time (UTC) in the struct tm format. If the conversion failed, an empty
    /// optional will be returned.
    inline optional<std::tm>
    gmtime_safe(std::time_t raw_time)
    {
        optional<std::tm> returned_value;

#ifdef _MSC_VER
        // MSVC introduced gmtime_s before the optional Annex K of C11 was standardized, with
        // swapped parameters.
        std::tm result {};
        errno_t err = gmtime_s(&result, &raw_time);
        if (err == 0) {
            returned_value = result;
        }
#elif XLNT_HAS_C_VERSION(XLNT_C_23)                                                                \
    || /* Feature test macros from https://linux.die.net/man/3/gmtime_r */                         \
    _POSIX_C_SOURCE >= 1 || _XOPEN_SOURCE || _BSD_SOURCE || _SVID_SOURCE || _POSIX_SOURCE

        // For C23 (used by C++26) compilers, and POSIX-compatible platforms.
        std::tm result {};
        if (gmtime_r(&raw_time, &result)) {
            returned_value = result;
        }
#elif defined(__STDC_LIB_EXT1__) && defined(__STDC_WANT_LIB_EXT1__)
        // For compilers implementing the optional Annex K of C11
        std::tm result {};
        if (gmtime_s(&raw_time, &result)) {
            returned_value = result;
        }
#else
        std::tm* tm = std::gmtime(&raw_time);

        if (tm != nullptr) {
            returned_value = *tm;
        }
#endif

        return returned_value;
    }

} // namespace detail
} // namespace xlnt
