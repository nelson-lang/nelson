// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2010-2015 openpyxl
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
#include <xlnt/utils/optional.hpp>

namespace xlnt {

class border;
class fill;
class font;

namespace detail {

    struct conditional_format_impl;
    struct stylesheet;
    class xlsx_consumer;
    class xlsx_producer;

} // namespace detail

class XLNT_API condition
{
public:
    static condition
    text_starts_with(const std::string& start);
    static condition
    text_ends_with(const std::string& end);
    static condition
    text_contains(const std::string& start);
    static condition
    text_does_not_contain(const std::string& start);

    bool
    operator==(const condition& rhs) const
    {
        return text_comparand_ == rhs.text_comparand_;
    }

    bool
    operator!=(const condition& rhs) const
    {
        return !(*this == rhs);
    }

private:
    friend class detail::xlsx_producer;

    enum class type
    {
        contains_text
    } type_;

    enum class condition_operator
    {
        starts_with,
        ends_with,
        contains,
        does_not_contain
    } operator_;

    std::string text_comparand_;
};

/// <summary>
/// Describes a conditional format that will be applied to all cells in the
/// associated range that satisfy the condition. This can only be constructed
/// using methods on worksheet or range.
/// </summary>
class XLNT_API conditional_format
{
public:
    /// <summary>
    /// Delete zero-argument constructor
    /// </summary>
    conditional_format() = delete;

    /// <summary>
    /// Default copy constructor. Constructs a format using the same PIMPL as other.
    /// </summary>
    conditional_format(const conditional_format& other) = default;

    // Formatting (xf) components

    /// <summary>
    /// Returns true if a border has been set for this conditional format.
    /// </summary>
    bool
    has_border() const;

    /// <summary>
    /// Returns a copy of the border of this conditional format. If no border has been set
    /// (has_border() returns false), a default-constructed border will be returned.
    /// </summary>
    class border
    border() const;

    /// <summary>
    /// Sets the border of this conditional format to new_border.
    /// Returns a wrapper pointing to this conditional format.
    /// </summary>
    conditional_format
    border(const xlnt::border& new_border);

    /// <summary>
    /// Returns true if a fill has been set for this conditional format.
    /// </summary>
    bool
    has_fill() const;

    /// <summary>
    /// Returns a copy of the fill of this conditional format. If no fill has been set (has_fill()
    /// returns false), a default-constructed fill will be returned.
    /// </summary>
    class fill
    fill() const;

    /// <summary>
    /// Sets the fill of this conditional format to new_fill.
    /// Returns a wrapper pointing to this conditional format.
    /// </summary>
    conditional_format
    fill(const xlnt::fill& new_fill);

    /// <summary>
    /// Returns true if a font has been set for this conditional format.
    /// </summary>
    bool
    has_font() const;

    /// <summary>
    /// Returns a copy of the font of this conditional format. If no font has been set (has_font()
    /// returns false), a default-constructed font will be returned.
    /// </summary>
    class font
    font() const;

    /// <summary>
    /// Sets the font of this conditional format to new_font.
    /// Returns a wrapper pointing to this conditional format.
    /// </summary>
    conditional_format
    font(const xlnt::font& new_font);

    /// <summary>
    /// Returns true if this format is equivalent to other.
    /// </summary>
    bool
    operator==(const conditional_format& other) const;

    /// <summary>
    /// Returns true if this format is not equivalent to other.
    /// </summary>
    bool
    operator!=(const conditional_format& other) const;

private:
    friend struct detail::stylesheet;
    friend class detail::xlsx_consumer;

    /// <summary>
    ///
    /// </summary>
    conditional_format(detail::conditional_format_impl* d);

    /// <summary>
    ///
    /// </summary>
    detail::conditional_format_impl* d_ = nullptr;
};

} // namespace xlnt
