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
#include <xlnt/internal/format_impl_ptr.hpp>

namespace xlnt {

class alignment;
class border;
class cell;
class fill;
class font;
class number_format;
class protection;
class style;

template <typename T> class optional;

namespace detail {

    struct format_impl;
    struct stylesheet;
    class xlsx_producer;
    class xlsx_consumer;

} // namespace detail

/// <summary>
/// Describes the formatting of a particular cell.
/// </summary>
class XLNT_API format
{
public:
    /// <summary>
    /// Returns a copy of the alignment of this format. If no alignment has been set
    /// (has_alignment() returns false), a default-constructed alignment will be returned.
    /// </summary>
    class alignment
    alignment() const;

    /// <summary>
    /// Sets the alignment of this format to new_alignment. Applied, which defaults
    /// to true, determines whether the alignment should be enabled for cells using
    /// this format.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    alignment(const xlnt::alignment& new_alignment, xlnt::optional<bool> applied = {});

    /// <summary>
    /// Returns true if an alignment has been set for this format.
    /// </summary>
    bool
    has_alignment() const;

    /// <summary>
    /// Returns true if the alignment of this format should be applied to cells
    /// using it.
    /// </summary>
    bool
    alignment_applied() const;

    /// <summary>
    /// Returns a copy of the border of this format. If no border has been set (has_border() returns
    /// false), a default-constructed border will be returned.
    /// </summary>
    class border
    border() const;

    /// <summary>
    /// Sets the border of this format to new_border. Applied, which defaults
    /// to true, determines whether the border should be enabled for cells using
    /// this format.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    border(const xlnt::border& new_border, xlnt::optional<bool> applied = {});

    /// <summary>
    /// Returns true if a border has been set for this format.
    /// </summary>
    bool
    has_border() const;

    /// <summary>
    /// Returns true if the border set for this format should be applied to cells using the format.
    /// </summary>
    bool
    border_applied() const;

    /// <summary>
    /// Returns a copy of the fill of this format. If no fill has been set (has_fill() returns
    /// false), a default-constructed fill will be returned.
    /// </summary>
    class fill
    fill() const;

    /// <summary>
    /// Sets the fill of this format to new_fill. Applied, which defaults
    /// to true, determines whether the border should be enabled for cells using
    /// this format.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    fill(const xlnt::fill& new_fill, xlnt::optional<bool> applied = {});

    /// <summary>
    /// Returns true if a fill has been set for this format.
    /// </summary>
    bool
    has_fill() const;

    /// <summary>
    /// Returns true if the fill set for this format should be applied to cells using the format.
    /// </summary>
    bool
    fill_applied() const;

    /// <summary>
    /// Returns a copy of the font of this format. If no font has been set (has_font() returns
    /// false), a default-constructed font will be returned.
    /// </summary>
    class font
    font() const;

    /// <summary>
    /// Sets the font of this format to new_font. Applied, which defaults
    /// to true, determines whether the font should be enabled for cells using
    /// this format.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    font(const xlnt::font& new_font, xlnt::optional<bool> applied = {});

    /// <summary>
    /// Returns true if a font has been set for this format.
    /// </summary>
    bool
    has_font() const;

    /// <summary>
    /// Returns true if the font set for this format should be applied to cells using the format.
    /// </summary>
    bool
    font_applied() const;

    /// <summary>
    /// Returns a copy of the number format of this format. If no number format has been set
    /// (has_number_format() returns false), a default-constructed number format will be returned.
    /// </summary>
    class number_format
    number_format() const;

    /// <summary>
    /// Sets the number format of this format to new_number_format. Applied, which defaults
    /// to true, determines whether the number format should be enabled for cells using
    /// this format.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    number_format(const xlnt::number_format& new_number_format, xlnt::optional<bool> applied = {});

    /// <summary>
    /// Returns true if a number format has been set for this format.
    /// </summary>
    bool
    has_number_format() const;

    /// <summary>
    /// Returns true if the number_format set for this format should be applied to cells using the
    /// format.
    /// </summary>
    bool
    number_format_applied() const;

    /// <summary>
    /// Returns a copy of the protection of this format. If no protection has been set
    /// (has_protection() returns false), a default-constructed protection will be returned.
    /// </summary>
    class protection
    protection() const;

    /// <summary>
    /// Returns true if protection has been set for this format.
    /// </summary>
    bool
    has_protection() const;

    /// <summary>
    /// Returns true if the protection set for this format should be applied to cells using the
    /// format.
    /// </summary>
    bool
    protection_applied() const;

    /// <summary>
    /// Sets the protection of this format to new_protection. Applied, which defaults
    /// to true, determines whether the protection should be enabled for cells using
    /// this format.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    protection(const xlnt::protection& new_protection, xlnt::optional<bool> applied = {});

    /// <summary>
    /// Returns true if the pivot table interface is enabled for this format.
    /// </summary>
    bool
    pivot_button() const;

    /// <summary>
    /// If show is true, a pivot table interface will be displayed for cells using
    /// this format.
    /// </summary>
    void
    pivot_button(bool show);

    /// <summary>
    /// Returns true if this format should add a single-quote prefix for all text values.
    /// </summary>
    bool
    quote_prefix() const;

    /// <summary>
    /// If quote is true, enables a single-quote prefix for all text values in cells
    /// using this format (e.g. "abc" will appear as "'abc"). The text will also not
    /// be stored in sharedStrings when this is enabled.
    /// </summary>
    void
    quote_prefix(bool quote);

    /// <summary>
    /// Returns true if this format has a corresponding style applied.
    /// </summary>
    bool
    has_style() const;

    /// <summary>
    /// Removes the style from this format if it exists.
    /// </summary>
    void
    clear_style();

    /// <summary>
    /// Sets the style of this format to a style with the given name.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    style(const std::string& name);

    /// <summary>
    /// Sets the style of this format to new_style.
    /// Returns a wrapper pointing to this format.
    /// </summary>
    format&
    style(const class style& new_style);

    /// <summary>
    /// Returns a wrapper pointing to the style of this format.
    /// Assumes that the style exists (please call has_style() to check).
    /// If this format has no style, an invalid_attribute exception will be thrown.
    /// </summary>
    class style
    style();

    /// <summary>
    /// Returns a wrapper pointing to the style of this format.
    /// Assumes that the style exists (please call has_style() to check).
    /// If this format has no style, an invalid_attribute exception will be thrown.
    /// </summary>
    const class style
    style() const;

private:
    friend struct detail::stylesheet;
    friend class detail::xlsx_producer;
    friend class detail::xlsx_consumer;
    friend class cell;
    friend class workbook;

    /// <summary>
    /// Constructs a format from an impl pointer.
    /// </summary>
    format(detail::format_impl_ptr d);

    /// <summary>
    /// The internal implementation of this format
    /// </summary>
    detail::format_impl_ptr d_;
};

} // namespace xlnt
