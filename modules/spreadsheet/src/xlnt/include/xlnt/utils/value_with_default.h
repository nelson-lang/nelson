// Copyright (c) 2026 xlnt-community
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

#include <cmath>
#include <cstdint>
#include <limits>
#include <utility>

namespace xlnt {
namespace detail {

    template <typename T, T value> struct default_value
    {
        static constexpr T
        get()
        {
            return value;
        }
        static bool
        is(const T& v)
        {
            return get() == v;
        }
    };

    template <typename T, std::int64_t numerator, std::int64_t denominator = 1> struct default_fp
    {
        static constexpr T
        get()
        {
            return (T)numerator / denominator;
        }
        static bool
        is(const T& v)
        {
            return get() == v;
        }
    };

    template <typename T> struct default_nan
    {
        static constexpr T
        get()
        {
            return std::numeric_limits<T>::quiet_NaN();
        }
        static bool
        is(const T& v)
        {
            return std::isnan(v);
        }
    };

    /// <summary>
    /// Encapsulates a value with a default value
    /// </summary>
    template <typename T, typename DEFAULT> class value_with_default_type
    {
    public:
        explicit value_with_default_type(T value = DEFAULT::get()) : value_(std::move(value)) { }

        bool
        is_default() const
        {
            return DEFAULT::is(value_);
        }
        bool
        is_set() const
        {
            return !is_default();
        }

        const T&
        get() const
        {
            return value_;
        }
        T&
        get()
        {
            return value_;
        }
        operator const T&() const { return get(); }
        operator T&() { return get(); }

        value_with_default_type&
        operator=(T value)
        {
            value_ = std::move(value);
            return *this;
        }

    private:
        T value_;
    };

    template <typename T, T default_value_>
    using value_with_default = value_with_default_type<T, default_value<T, default_value_>>;

    template <typename T, std::int64_t numerator, std::int64_t denominator = 1>
    using fp_with_default = value_with_default_type<T, default_fp<T, numerator, denominator>>;

    template <std::int64_t numerator, std::int64_t denominator = 1>
    using double_with_default = fp_with_default<double, numerator, denominator>;

    template <std::int64_t numerator, std::int64_t denominator = 1>
    using float_with_default = fp_with_default<float, numerator, denominator>;

    template <typename T> using fp_with_default_nan = value_with_default_type<T, default_nan<T>>;

} // namespace detail
} // namespace xlnt
