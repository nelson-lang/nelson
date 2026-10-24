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
#include <unordered_map>

#include <xlnt/xlnt_config.hpp>
#include <xlnt/cell/cell_type.hpp>
#include <xlnt/cell/index_types.hpp>
#include <xlnt/cell/rich_text.hpp>

namespace xlnt {

enum class calendar;

class alignment;
class base_format;
class border;
class cell_reference;
class comment;
class fill;
class font;
class format;
class number_format;
class protection;
class range;
class relationship;
class style;
class workbook;
class worksheet;
class xlsx_consumer;
class xlsx_producer;
class phonetic_pr;

struct date;
struct datetime;
struct time;
struct timedelta;

namespace detail {

    class xlsx_consumer;
    class xlsx_producer;

    struct cell_impl;

} // namespace detail

/// <summary>
/// Describes a unit of data in a worksheet at a specific coordinate and its
/// associated properties.
/// </summary>
/// <remarks>
/// Properties of interest include style, type, value, and address.
/// The Cell class is required to know its value and type, display options,
/// and any other features of an Excel cell.Utilities for referencing
/// cells using Excel's 'A1' column/row nomenclature are also provided.
/// </remarks>
class XLNT_API cell
{
public:
    /// <summary>
    /// Alias xlnt::cell_type to xlnt::cell::type since it looks nicer.
    /// </summary>
    using type = cell_type;

    /// <summary>
    /// Returns a map of error strings such as \#DIV/0! and their associated indices.
    /// </summary>
    static const std::unordered_map<std::string, int>&
    error_codes();

    /// <summary>
    /// Default copy constructor.
    /// </summary>
    cell(const cell&) = default;

    // value

    /// <summary>
    /// Returns true if value has been set and has not been cleared using cell::clear_value().
    /// </summary>
    bool
    has_value() const;

    /// <summary>
    /// Returns the value of this cell as an instance of type T.
    /// Overloads exist for most C++ fundamental types like bool, int, etc. as well
    /// as for std::string and xlnt datetime types: date, time, datetime, and timedelta.
    /// </summary>
    template <typename T>
    T
    value() const;

    /// <summary>
    /// Makes this cell have a value of type null.
    /// All other cell attributes are retained.
    /// </summary>
    void
    clear_value();

    /// <summary>
    /// Sets the type of this cell to null.
    /// </summary>
    void value(std::nullptr_t);

    /// <summary>
    /// Sets the value of this cell to the given boolean value.
    /// </summary>
    void
    value(bool boolean_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(int int_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(unsigned int int_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(long long int int_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(unsigned long long int int_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(float float_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(double float_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const date& date_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const time& time_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const datetime& datetime_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const timedelta& timedelta_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const std::string& string_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const char* string_value);

    /// <summary>
    /// Sets the value of this cell to the given value.
    /// </summary>
    void
    value(const rich_text& text_value);

    /// <summary>
    /// Sets the value and formatting of this cell to that of other_cell.
    /// NOTE: if copying from a different workbook, shared strings are remapped
    /// correctly, formulas are copied as text, and formats are deep-copied
    /// automatically. External hyperlinks are copied; internal hyperlinks
    /// (cell/range references) are not yet implemented.
    /// NOTE: style is NOT deep-copied yet; if the destination workbook has a style
    /// with the same name it will be used, otherwise only formatting properties
    /// (font/fill/border/number format/alignment/protection) are copied assigned to the style name
    /// </summary>
    void
    value(const cell other_cell);

    /// <summary>
    /// Analyzes string_value to determine its type, convert it to that type,
    /// and set the value of this cell to that converted value.
    /// </summary>
    void
    value(const std::string& string_value, bool infer_type);

    /// <summary>
    /// Returns the type of this cell.
    /// </summary>
    type
    data_type() const;

    /// <summary>
    /// Sets the type of this cell. This should usually be done indirectly
    /// by setting the value of the cell to a value of that type.
    /// </summary>
    void
    data_type(type t);

    // properties

    /// <summary>
    /// There's no reason to keep a cell which has no value and is not a placeholder.
    /// Returns true if this cell has no value, style, isn't merged, etc.
    /// </summary>
    bool
    garbage_collectible() const;

    /// <summary>
    /// Returns true iff this cell's number format matches a date format.
    /// </summary>
    bool
    is_date() const;

    // position

    /// <summary>
    /// Returns a cell_reference that points to the location of this cell.
    /// </summary>
    cell_reference
    reference() const;

    /// <summary>
    /// Returns the column of this cell.
    /// </summary>
    column_t
    column() const;

    /// <summary>
    /// Returns the numeric index (A == 1) of the column of this cell.
    /// </summary>
    column_t::index_t
    column_index() const;

    /// <summary>
    /// Returns the row of this cell.
    /// </summary>
    row_t
    row() const;

    /// <summary>
    /// Returns the location of this cell as an ordered pair (left, top).
    /// </summary>
    std::pair<int, int>
    anchor() const;

    // hyperlink

    /// <summary>
    /// Returns a wrapper pointing to the relationship of this cell's hyperlink.
    /// Assumes that this cell has a hyperlink (please call has_hyperlink() to check).
    /// If this cell does not have a hyperlink, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    class hyperlink
    hyperlink() const;

    /// <summary>
    /// Adds a hyperlink to this cell pointing to the URI of the given value (which must not be
    /// empty) and sets the text value of the cell to the given parameter. If the URI is empty, an
    /// xlnt::invalid_parameter exception will be thrown.
    /// </summary>
    void
    hyperlink(const std::string& url, const std::string& display = "");

    /// <summary>
    /// Adds an internal hyperlink to this cell pointing to the given cell.
    /// </summary>
    void
    hyperlink(xlnt::cell target, const std::string& display = "");

    /// <summary>
    /// Adds an internal hyperlink to this cell pointing to the given range.
    /// </summary>
    void
    hyperlink(xlnt::range target, const std::string& display = "");

    /// <summary>
    /// Returns true if this cell has a hyperlink set.
    /// </summary>
    bool
    has_hyperlink() const;

    // computed formatting

    /// <summary>
    /// Returns the alignment that should be used when displaying this cell
    /// graphically based on the workbook default, the cell-level format,
    /// and the named style applied to the cell in that order.
    /// </summary>
    class alignment
    computed_alignment() const;

    /// <summary>
    /// Returns the border that should be used when displaying this cell
    /// graphically based on the workbook default, the cell-level format,
    /// and the named style applied to the cell in that order.
    /// </summary>
    class border
    computed_border() const;

    /// <summary>
    /// Returns the fill that should be used when displaying this cell
    /// graphically based on the workbook default, the cell-level format,
    /// and the named style applied to the cell in that order.
    /// </summary>
    class fill
    computed_fill() const;

    /// <summary>
    /// Returns the font that should be used when displaying this cell
    /// graphically based on the workbook default, the cell-level format,
    /// and the named style applied to the cell in that order.
    /// </summary>
    class font
    computed_font() const;

    /// <summary>
    /// Returns the number format that should be used when displaying this cell
    /// graphically based on the workbook default, the cell-level format,
    /// and the named style applied to the cell in that order.
    /// </summary>
    class number_format
    computed_number_format() const;

    /// <summary>
    /// Returns the protection that should be used when displaying this cell
    /// graphically based on the workbook default, the cell-level format,
    /// and the named style applied to the cell in that order.
    /// </summary>
    class protection
    computed_protection() const;

    // format

    /// <summary>
    /// Returns true if this cell has had a format applied to it.
    /// </summary>
    bool
    has_format() const;

    /// <summary>
    /// Returns a const wrapper pointing to the format applied to this cell.
    /// The returned wrapper cannot be modified directly, but is meant to be modified using the
    /// other format-related functions. In case the format will be modified later, a new format will
    /// be created, while this returned format will be left unmodified (lazy copy / copy-on-write).
    /// Assumes that the format exists (please call has_format() to check).
    /// If this cell has no format, an invalid_attribute exception will be thrown.
    /// </summary>
    const class format
    format() const;

    /// <summary>
    /// Applies the cell-level formatting of new_format to this cell.
    /// In case the format will be modified later, a new format will be created, while
    /// this original format will be left unmodified (lazy copy / copy-on-write).
    /// NOTE: when copying from a different workbook: format properties are deep-copied;
    /// If new_format has a style, the cloned format is associated with the same style name.
    /// NOTE: Deep cloning of the style itself is not yet implemented.
    /// </summary>
    void
    format(const class format new_format);

    /// <summary>
    /// Removes the cell-level formatting from this cell, if the cell has a format.
    /// This doesn't affect the style that may also be applied to the cell.
    /// </summary>
    void
    clear_format();

    /// <summary>
    /// Returns a copy of the number format of this cell. If no number format has been set
    /// (has_format() returns false, or format().has_number_format() returns false), a
    /// default-constructed number format will be returned.
    /// </summary>
    class number_format
    number_format() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its number_format
    /// to the given format, and applies the format to this cell.
    /// </summary>
    void
    number_format(const class number_format& format);

    /// <summary>
    /// Returns a copy of the font applied to the text in this cell. If no font has been set
    /// (has_format() returns false, or format().has_font() returns false), a default-constructed
    /// font will be returned.
    /// </summary>
    class font
    font() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its font
    /// to the given font, and applies the format to this cell.
    /// </summary>
    void
    font(const class font& font_);

    /// <summary>
    /// Returns a copy of the fill applied to this cell. If no fill has been set (has_format()
    /// returns false, or format().has_fill() returns false), a default-constructed fill will be
    /// returned.
    /// </summary>
    class fill
    fill() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its fill
    /// to the given fill, and applies the format to this cell.
    /// </summary>
    void
    fill(const class fill& fill_);

    /// <summary>
    /// Returns a copy of the border of this cell. If no border has been set (has_format() returns
    /// false, or format().has_border() returns false), a default-constructed border will be
    /// returned.
    /// </summary>
    class border
    border() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its border
    /// to the given border, and applies the format to this cell.
    /// </summary>
    void
    border(const class border& border_);

    /// <summary>
    /// Returns a copy of the alignment of the text in this cell. If no alignment has been set
    /// (has_format() returns false, or format().has_alignment() returns false), a
    /// default-constructed alignment will be returned.
    /// </summary>
    class alignment
    alignment() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its alignment
    /// to the given alignment, and applies the format to this cell.
    /// </summary>
    void
    alignment(const class alignment& alignment_);

    /// <summary>
    /// Returns a copy of the protection of this cell. If no protection has been set (has_format()
    /// returns false, or format().has_protection() returns false), a default-constructed protection
    /// will be returned.
    /// </summary>
    class protection
    protection() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its protection
    /// to the given protection, and applies the format to this cell.
    /// </summary>
    void
    protection(const class protection& protection_);

    // style

    /// <summary>
    /// Returns true if this cell has had a style applied to it.
    /// </summary>
    bool
    has_style() const;

    /// <summary>
    /// Returns a wrapper pointing to the named style applied to this cell.
    /// Assumes that the style exists (please call has_style() to check).
    /// If this cell does not have a style, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    class style
    style();

    /// <summary>
    /// Returns a wrapper pointing to the named style applied to this cell.
    /// Assumes that the style exists (please call has_style() to check).
    /// If this cell does not have a style, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    const class style
    style() const;

    /// <summary>
    /// Creates a new format in the workbook, sets its style to the given style,
    /// and applies the format to this cell to a style. Equivalent to style(new_style.name()).
    /// </summary>
    void
    style(const class style& new_style);

    /// <summary>
    /// Sets the named style applied to this cell to a style named style_name.
    /// Assumes that the style already exists in the workbook (please call workbook::has_style() to
    /// check). If this style has not been previously created in the workbook, a key_not_found
    /// exception will be thrown.
    /// </summary>
    void
    style(const std::string& style_name);

    /// <summary>
    /// Removes the named style from this cell, if the cell has a style.
    /// This will not affect the cell format of the cell.
    /// </summary>
    void
    clear_style();

    // formula

    /// <summary>
    /// Returns the string representation of the formula applied to this cell.
    /// Assumes that cell view has a formula (please call has_formula() to check).
    /// If this cell does not have a formula, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    std::string
    formula() const;

    /// <summary>
    /// Sets the formula of this cell to the given value.
    /// This formula string should begin with '='.
    /// </summary>
    void
    formula(const std::string& formula);

    /// <summary>
    /// Removes the formula from this cell. After this is called, has_formula() will return false.
    /// </summary>
    void
    clear_formula();

    /// <summary>
    /// Returns true if this cell has had a formula applied to it.
    /// </summary>
    bool
    has_formula() const;

    // printing

    /// <summary>
    /// Returns a string representing the value of this cell. If the data type is not a string,
    /// it will be converted according to the number format.
    /// </summary>
    std::string
    to_string() const;

    // merging

    /// <summary>
    /// Returns true iff this cell has been merged with one or more
    /// surrounding cells.
    /// </summary>
    bool
    is_merged() const;

    /// <summary>
    /// Makes this a merged cell iff merged is true.
    /// Generally, this shouldn't be called directly. Instead,
    /// use worksheet::merge_cells on its parent worksheet.
    /// </summary>
    void
    merged(bool merged);

    // phonetics

    /// <summary>
    /// Returns true if this cell is set to show phonetic information.
    /// </summary>
    bool
    phonetics_visible() const;

    /// <summary>
    /// Enables the display of phonetic information on this cell.
    /// </summary>
    void
    show_phonetics(bool phonetics);

    /// <summary>
    /// Returns the error string that is stored in this cell.
    /// Assumes that the cell type is type::error (please call data_type() to check).
    /// If the cell is not of type::error, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    std::string
    error() const;

    /// <summary>
    /// Directly assigns the value of this cell to be the given error.
    /// Assumes that the given string is an error which begins with '#'.
    /// If the given string is not an error, an xlnt::invalid_data_type exception will be thrown.
    /// </summary>
    void
    error(const std::string& error);

    /// <summary>
    /// Returns a wrapper pointing to the cell from this cell's parent workbook at
    /// a relative offset given by the parameters.
    /// </summary>
    cell
    offset(int column, int row);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet that owns this cell.
    /// </summary>
    class worksheet
    worksheet();

    /// <summary>
    /// Returns a wrapper pointing to the worksheet that owns this cell.
    /// </summary>
    const class worksheet
    worksheet() const;

    /// <summary>
    /// Returns a wrapper pointing to the workbook of the worksheet that owns this cell.
    /// </summary>
    class workbook
    workbook();

    /// <summary>
    /// Returns a wrapper pointing to the workbook of the worksheet that owns this cell.
    /// </summary>
    const class workbook
    workbook() const;

    /// <summary>
    /// Returns a copy of the base date of the parent workbook.
    /// </summary>
    calendar
    base_date() const;

    /// <summary>
    /// Returns to_check after verifying and fixing encoding, size, and illegal characters.
    /// </summary>
    std::string
    check_string(const std::string& to_check);

    // comment

    /// <summary>
    /// Returns true if this cell has a comment applied.
    /// </summary>
    bool
    has_comment() const;

    /// <summary>
    /// Deletes the comment applied to this cell if it exists.
    /// </summary>
    void
    clear_comment();

    /// <summary>
    /// Returns a copy of the comment applied to this cell.
    /// Assumes that the comment exists (please call has_comment() to check).
    /// If this cell does not have a comment, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    class comment
    comment() const;

    /// <summary>
    /// Creates a new comment with the given text and optional author and
    /// applies it to the cell.
    /// </summary>
    void
    comment(const std::string& text, const std::string& author = "Microsoft Office User");

    /// <summary>
    /// Creates a new comment with the given text, formatting, and optional
    /// author and applies it to the cell.
    /// </summary>
    void
    comment(const std::string& comment_text, const class font& comment_font,
        const std::string& author = "Microsoft Office User");

    /// <summary>
    /// Apply the comment provided as the only argument to the cell.
    /// </summary>
    void
    comment(const class comment& new_comment);

    /// <summary>
    /// Returns the width of this cell in pixels.
    /// </summary>
    double
    width() const;

    /// <summary>
    /// Returns the height of this cell in pixels.
    /// </summary>
    double
    height() const;

    // comparisons

    /// <summary>
    /// Returns true if this cell is equal to other. If compare_by_reference is true, the comparison
    /// will only check that both cells point to the same internal cell. Otherwise,
    /// if compare_by_reference is false, all cell properties except the row and column are
    /// compared.
    /// </summary>
    bool
    compare(const cell& other, bool compare_by_reference) const;

    // operators

    /// <summary>
    /// Makes this cell interally point to rhs.
    /// The cell data originally pointed to by this cell will be unchanged.
    /// </summary>
    cell&
    operator=(const cell& rhs);

    /// <summary>
    /// Returns true if this cell the same cell as comparand (compared by reference).
    /// </summary>
    bool
    operator==(const cell& comparand) const;

    /// <summary>
    /// Returns false if this cell the same cell as comparand (compared by reference).
    /// </summary>
    bool
    operator!=(const cell& comparand) const;

private:
    friend class style;
    friend class worksheet;
    friend class detail::xlsx_consumer;
    friend class detail::xlsx_producer;
    friend struct detail::cell_impl;

    /// <summary>
    /// Helper to set rich_text value without calling check_string.
    /// </summary>
    void
    value_no_check(const rich_text& text);

    /// <summary>
    /// Helper to copy value from cell in different workbook.
    /// Handles shared_string remapping, formula copying, format deep-copy.
    /// External hyperlinks are copied; internal hyperlinks (cell/range
    /// references) are not yet implemented.
    /// </summary>
    void
    copy_from_other_workbook(const cell& source);

    /// <summary>
    /// Helper to clone format from different workbook.
    /// Creates deep-copy of format in destination workbook's stylesheet,
    /// if new_format has a style, the cloned format is associated with the same style name.
    /// NOTE: Deep cloning of the style itself is not yet implemented.
    /// </summary>
    void
    copy_format_from_other_workbook(const class format& source_format);

    /// <summary>
    /// Returns a non-const wrapper pointing to the format of this cell.
    /// This is for internal use only.
    /// Assumes that this cell has a format (please call has_format() to check).
    /// If this cell does not have a format, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    class format
    modifiable_format();

    /// <summary>
    /// Delete the default zero-argument constructor.
    /// </summary>
    cell() = delete;

    /// <summary>
    /// Private constructor to create a cell from its implementation.
    /// </summary>
    cell(detail::cell_impl* d);

    /// <summary>
    /// A pointer to this cell's implementation.
    /// </summary>
    detail::cell_impl* d_ = nullptr;
};

/// <summary>
/// Returns true if this cell is uninitialized.
/// </summary>
XLNT_API bool
operator==(std::nullptr_t, const cell& cell);

/// <summary>
/// Returns true if this cell is uninitialized.
/// </summary>
XLNT_API bool
operator==(const cell& cell, std::nullptr_t);

/// <summary>
/// Returns true if this cell is initialized.
/// </summary>
XLNT_API bool
operator!=(std::nullptr_t, const cell& cell);

/// <summary>
/// Returns true if this cell is initialized.
/// </summary>
XLNT_API bool
operator!=(const cell& cell, std::nullptr_t);

/// <summary>
/// Convenience function for writing cell to an ostream.
/// Uses cell::to_string() internally.
/// </summary>
XLNT_API std::ostream&
operator<<(std::ostream& stream, const xlnt::cell& cell);

template <>
XLNT_API bool
cell::value<bool>() const;

template <>
XLNT_API int
cell::value<int>() const;

template <>
XLNT_API unsigned int
cell::value<unsigned int>() const;

template <>
XLNT_API long long int
cell::value<long long int>() const;

template <>
XLNT_API unsigned long long
cell::value<unsigned long long int>() const;

template <>
XLNT_API float
cell::value<float>() const;

template <>
XLNT_API double
cell::value<double>() const;

template <>
XLNT_API date
cell::value<date>() const;

template <>
XLNT_API time
cell::value<time>() const;

template <>
XLNT_API datetime
cell::value<datetime>() const;

template <>
XLNT_API timedelta
cell::value<timedelta>() const;

template <>
XLNT_API std::string
cell::value<std::string>() const;

template <>
XLNT_API rich_text
cell::value<rich_text>() const;

} // namespace xlnt
