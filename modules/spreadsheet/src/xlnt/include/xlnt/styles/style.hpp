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

class alignment;
class border;
class cell;
class fill;
class font;
class number_format;
class protection;

namespace detail {

    struct style_impl;
    struct stylesheet;
    class xlsx_consumer;

} // namespace detail

/// <summary>
/// Describes a style which has a name and can be applied to multiple individual
/// formats. In Excel this is a "Cell Style".
/// </summary>
class XLNT_API style
{
public:
    /// <summary>
    /// Delete zero-argument constructor
    /// </summary>
    style() = delete;

    /// <summary>
    /// Default copy constructor. Constructs a style using the same PIMPL as other.
    /// </summary>
    style(const style& other) = default;

    /// <summary>
    /// Returns the name of this style.
    /// </summary>
    std::string
    name() const;

    /// <summary>
    /// Sets the name of this style to name.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    name(const std::string& name);

    /// <summary>
    /// Returns true if this style is hidden.
    /// </summary>
    bool
    hidden() const;

    /// <summary>
    /// Sets the hidden state of this style to value. A hidden style will not
    /// be shown in the list of selectable styles in the UI, but will still
    /// apply its formatting to cells using it.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    hidden(bool value);

    /// <summary>
    /// Returns true if this is a builtin style that has been customized and
    /// should therefore be persisted in the workbook.
    /// </summary>
    bool
    custom_builtin() const;

    /// <summary>
    /// Returns the index of the builtin style that this style is an instance
    /// of or is a customized version thereof. If style::builtin() is false,
    /// this will throw an invalid_attribute exception.
    /// </summary>
    std::size_t
    builtin_id() const;

    /// <summary>
    /// Returns true if this is a builtin style.
    /// </summary>
    bool
    builtin() const;

    // Formatting (xf) components

    /// <summary>
    /// Returns a copy of the alignment of this style. If no alignment has been set (has_alignment()
    /// returns false), a default-constructed alignment will be returned.
    /// </summary>
    class alignment
    alignment() const;

    /// <summary>
    /// Returns true if an alignment has been set for this style.
    /// </summary>
    bool
    has_alignment() const;

    /// <summary>
    /// Returns true if the alignment of this style should be applied to cells
    /// using it.
    /// </summary>
    bool
    alignment_applied() const;

    /// <summary>
    /// Sets the alignment of this style to new_alignment. Applied, which defaults
    /// to true, determines whether the alignment should be enabled for cells using
    /// this style.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    alignment(const xlnt::alignment& new_alignment, optional<bool> applied = {});

    /// <summary>
    /// Returns a copy of the border of this style. If no border has been set (has_border() returns
    /// false), a default-constructed border will be returned.
    /// </summary>
    class border
    border() const;

    /// <summary>
    /// Returns true if a border has been set for this style.
    /// </summary>
    bool
    has_border() const;

    /// <summary>
    /// Returns true if the border set for this style should be applied to cells using the style.
    /// </summary>
    bool
    border_applied() const;

    /// <summary>
    /// Sets the border of this style to new_border. Applied, which defaults
    /// to true, determines whether the border should be enabled for cells using
    /// this style.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    border(const xlnt::border& new_border, optional<bool> applied = {});

    /// <summary>
    /// Returns a copy of the fill of this style. If no fill has been set (has_fill() returns
    /// false), a default-constructed fill will be returned.
    /// </summary>
    class fill
    fill() const;

    /// <summary>
    /// Returns true if a fill has been set for this style.
    /// </summary>
    bool
    has_fill() const;

    /// <summary>
    /// Returns true if the fill set for this style should be applied to cells using the style.
    /// </summary>
    bool
    fill_applied() const;

    /// <summary>
    /// Sets the fill of this style to new_fill. Applied, which defaults
    /// to true, determines whether the border should be enabled for cells using
    /// this style.
    /// </summary>
    style
    fill(const xlnt::fill& new_fill, optional<bool> applied = {});

    /// <summary>
    /// Returns a copy of the font of this style. If no font has been set (has_font() returns
    /// false), a default-constructed font will be returned.
    /// </summary>
    class font
    font() const;

    /// <summary>
    /// Returns true if a font has been set for this style.
    /// </summary>
    bool
    has_font() const;

    /// <summary>
    /// Returns true if the font set for this style should be applied to cells using the style.
    /// </summary>
    bool
    font_applied() const;

    /// <summary>
    /// Sets the font of this style to new_font. Applied, which defaults
    /// to true, determines whether the font should be enabled for cells using
    /// this style.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    font(const xlnt::font& new_font, optional<bool> applied = {});

    /// <summary>
    /// Returns a copy of the number_format of this style. If no number format has been set
    /// (has_number_format() returns false), a default-constructed number format will be returned.
    /// </summary>
    class number_format
    number_format() const;

    /// <summary>
    /// Returns true if a number format has been set for this style.
    /// </summary>
    bool
    has_number_format() const;

    /// <summary>
    /// Returns true if the number_format set for this style should be applied to cells using the
    /// style.
    /// </summary>
    bool
    number_format_applied() const;

    /// <summary>
    /// Sets the number format of this style to new_number_format. Applied, which defaults
    /// to true, determines whether the number format should be enabled for cells using
    /// this style.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    number_format(const xlnt::number_format& new_number_format, optional<bool> applied = {});

    /// <summary>
    /// Returns a copy of the protection of this style. If no protection has been set
    /// (has_protection() returns false), a default-constructed protection will be returned.
    /// </summary>
    class protection
    protection() const;

    /// <summary>
    /// Returns true if protection has been set for this style.
    /// </summary>
    bool
    has_protection() const;

    /// <summary>
    /// Returns true if the protection set for this style should be applied to cells using the
    /// style.
    /// </summary>
    bool
    protection_applied() const;

    /// <summary>
    /// Sets the border of this style to new_protection. Applied, which defaults
    /// to true, determines whether the protection should be enabled for cells using
    /// this style.
    /// Returns a wrapper pointing to this style.
    /// </summary>
    style
    protection(const xlnt::protection& new_protection, optional<bool> applied = {});

    /// <summary>
    /// Returns true if the pivot table interface is enabled for this style.
    /// </summary>
    bool
    pivot_button() const;

    /// <summary>
    /// If show is true, a pivot table interface will be displayed for cells using
    /// this style.
    /// </summary>
    void
    pivot_button(bool show);

    /// <summary>
    /// Returns true if this style should add a single-quote prefix for all text values.
    /// </summary>
    bool
    quote_prefix() const;

    /// <summary>
    /// If quote is true, enables a single-quote prefix for all text values in cells
    /// using this style (e.g. "abc" will appear as "'abc"). The text will also not
    /// be stored in sharedStrings when this is enabled.
    /// </summary>
    void
    quote_prefix(bool quote);

    /// <summary>
    /// Returns true if this style is equivalent to other.
    /// </summary>
    bool
    operator==(const style& other) const;

    /// <summary>
    /// Returns true if this style is not equivalent to other.
    /// </summary>
    bool
    operator!=(const style& other) const;

private:
    friend struct detail::stylesheet;
    friend class detail::xlsx_consumer;

    /// <summary>
    /// Constructs a style from an impl pointer.
    /// </summary>
    style(detail::style_impl* d);

    /// <summary>
    /// The internal implementation of this style
    /// </summary>
    detail::style_impl* d_ = nullptr;
};

} // namespace xlnt
