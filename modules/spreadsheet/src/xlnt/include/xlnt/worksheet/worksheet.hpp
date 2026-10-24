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

#include <iterator>
#include <string>
#include <vector>

#include <xlnt/xlnt_config.hpp>
#include <xlnt/cell/index_types.hpp>
#include <xlnt/packaging/relationship.hpp>
#include <xlnt/worksheet/page_margins.hpp>
#include <xlnt/worksheet/page_setup.hpp>
#include <xlnt/worksheet/sheet_view.hpp>

namespace xlnt {

class cell;
class cell_reference;
class cell_vector;
class column_properties;
class comment;
class condition;
class conditional_format;
class const_range_iterator;
class footer;
class header;
class range;
class range_iterator;
class range_reference;
class relationship;
class row_properties;
class sheet_format_properties;
class workbook;
class phonetic_pr;

struct date;

namespace detail {

    class xlsx_consumer;
    class xlsx_producer;

    struct worksheet_impl;

} // namespace detail

/// <summary>
/// A worksheet is a 2D array of cells starting with cell A1 in the top-left corner
/// and extending indefinitely down and right as needed.
/// </summary>
class XLNT_API worksheet
{
public:
    /// <summary>
    /// Iterate over a non-const worksheet with an iterator of this type.
    /// </summary>
    using iterator = range_iterator;

    /// <summary>
    /// Iterate over a non-const worksheet with an iterator of this type.
    /// </summary>
    using const_iterator = const_range_iterator;

    /// <summary>
    /// Iterate in reverse over a non-const worksheet with an iterator of this type.
    /// </summary>
    using reverse_iterator = std::reverse_iterator<iterator>;

    /// <summary>
    /// Iterate in reverse order over a const worksheet with an iterator of this type.
    /// </summary>
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    /// <summary>
    /// Construct a null worksheet. No methods should be called on such a worksheet.
    /// </summary>
    worksheet();

    /// <summary>
    /// Copy constructor. This worksheet will point to the same memory as rhs's worksheet.
    /// </summary>
    worksheet(const worksheet& rhs);

    /// <summary>
    /// Returns the workbook this worksheet is owned by.
    /// </summary>
    class workbook
    workbook();

    /// <summary>
    /// Returns the workbook this worksheet is owned by.
    /// </summary>
    const class workbook
    workbook() const;

    /// <summary>
    /// Deletes data held in the worksheet that does not affect the internal data or display.
    /// For example, unreference styles and empty cells will be removed.
    /// </summary>
    void
    garbage_collect();

    // identification

    /// <summary>
    /// Returns the unique numeric identifier of this worksheet. This will sometimes but not
    /// necessarily be the index of the worksheet in the workbook.
    /// </summary>
    std::size_t
    id() const;

    /// <summary>
    /// Set the unique numeric identifier. The id defaults to the lowest unused id in the workbook
    /// so this should not be called without a good reason.
    /// </summary>
    void
    id(std::size_t id);

    /// <summary>
    /// Returns the title of this sheet.
    /// </summary>
    std::string
    title() const;

    /// <summary>
    /// Sets the title of this sheet.
    /// An invalid_sheet_title exception will be thrown if the new title:
    /// - is empty
    /// - is longer than 31 Unicode characters
    /// - contains any of the forbidden characters *:/\?[]
    /// - contains any characters that are not encoded as UTF-8 and or are invalid UTF-8
    /// </summary>
    void
    title(const std::string& title);

    // freeze panes

    /// <summary>
    /// Returns the top left corner of the region above and to the left of which panes are frozen.
    /// Assumes that this sheet has frozen panes (please call has_frozen_panes() to check).
    /// If this sheet does not have frozen panes, an xlnt::invalid_attribute exception will be
    /// thrown.
    /// </summary>
    cell_reference
    frozen_panes() const;

    /// <summary>
    /// Freeze panes above and to the left of top_left_cell.
    /// </summary>
    void
    freeze_panes(cell top_left_cell);

    /// <summary>
    /// Freeze panes above and to the left of top_left_coordinate.
    /// </summary>
    void
    freeze_panes(const cell_reference& top_left_coordinate);

    /// <summary>
    /// Remove frozen panes. The data in those panes will be unaffected--this affects only the view.
    /// </summary>
    void
    unfreeze_panes();

    /// <summary>
    /// Returns true if this sheet has a frozen row, frozen column, or both.
    /// </summary>
    bool
    has_frozen_panes() const;

    // container

    /// <summary>
    /// Returns true if this sheet has an initialized cell at the given reference.
    /// </summary>
    bool
    has_cell(const cell_reference& reference) const;

    /// <summary>
    /// Returns a wrapper pointing to the cell at the given reference.
    /// If the cell doesn't exist (has_cell() returns false), an empty cell will be created,
    /// added to the worksheet (has_cell() will return true afterwards), and returned.
    /// </summary>
    class cell
    cell(const cell_reference& reference);

    /// <summary>
    /// Returns a wrapper pointing to the cell at the given reference.
    /// Assumes that the cell exists (please call has_cell() to check).
    /// If the cell doesn't exist, an invalid_parameter exception will be thrown.
    /// </summary>
    const class cell
    cell(const cell_reference& reference) const;

    /// <summary>
    /// Returns a wrapper pointing to the cell at the given column and row.
    /// If the cell doesn't exist (has_cell() returns false), an empty cell will be created,
    /// added to the worksheet (has_cell() will return true afterwards), and returned.
    /// </summary>
    class cell
    cell(column_t column, row_t row);

    /// <summary>
    /// Returns a wrapper pointing to the cell at the given column and row.
    /// Assumes that the cell exists (please call has_cell() to check).
    /// If the cell doesn't exist, an invalid_parameter exception will be thrown.
    /// </summary>
    const class cell
    cell(column_t column, row_t row) const;

    /// <summary>
    /// Returns the range defined by reference string. If reference string is the name of
    /// a previously-defined named range in the sheet, it will be returned.
    /// </summary>
    class range
    range(const std::string& reference_string);

    /// <summary>
    /// Returns the range defined by reference string. If reference string is the name of
    /// a previously-defined named range in the sheet, it will be returned.
    /// </summary>
    const class range
    range(const std::string& reference_string) const;

    /// <summary>
    /// Returns the range defined by reference.
    /// </summary>
    class range
    range(const range_reference& reference);

    /// <summary>
    /// Returns the range defined by reference.
    /// </summary>
    const class range
    range(const range_reference& reference) const;

    /// <summary>
    /// Returns a range encompassing all cells in this sheet which will
    /// be iterated upon in row-major order. If skip_null is true (default),
    /// the range does not contain empty rows/columns at its boundaries and
    /// empty rows and cells will be skipped during iteration of the range.
    /// </summary>
    class range
    rows(bool skip_null = true);

    /// <summary>
    /// Returns a range encompassing all cells in this sheet which will
    /// be iterated upon in row-major order. If skip_null is true (default),
    /// the range does not contain empty rows/columns at its boundaries and
    /// empty rows and cells will be skipped during iteration of the range.
    /// </summary>
    const class range
    rows(bool skip_null = true) const;

    /// <summary>
    /// Returns a range ecompassing all cells in this sheet which will
    /// be iterated upon in column-major order. If skip_null is true (default),
    /// the range does not contain empty rows/columns at its boundaries and
    /// empty columns and cells will be skipped during iteration of the range.
    /// </summary>
    class range
    columns(bool skip_null = true);

    /// <summary>
    /// Returns a range ecompassing all cells in this sheet which will
    /// be iterated upon in column-major order. If skip_null is true (default),
    /// the range does not contain empty rows/columns at its boundaries and
    /// empty columns and cells will be skipped during iteration of the range.
    /// </summary>
    const class range
    columns(bool skip_null = true) const;

    // TODO: finish implementing cell_iterator wrapping before uncommenting
    // class cell_vector cells(bool skip_null = true);

    // TODO: finish implementing cell_iterator wrapping before uncommenting
    // const class cell_vector cells(bool skip_null = true) const;

    /// <summary>
    /// Clears memory used by the given cell.
    /// </summary>
    void
    clear_cell(const cell_reference& ref);

    /// <summary>
    /// Clears memory used by all cells in the given row.
    /// </summary>
    void
    clear_row(row_t row);

    /// <summary>
    /// Insert empty rows before the given row index
    /// If the rows are inserted out of bounds (before the minimum or after the maximum allowed
    /// indices), an invalid_parameter exception will be thrown.
    /// </summary>
    void
    insert_rows(row_t row, std::uint32_t amount);

    /// <summary>
    /// Insert empty columns before the given column index
    /// If the columns are inserted out of bounds (before the minimum or after the maximum allowed
    /// indices), an invalid_parameter exception will be thrown.
    /// </summary>
    void
    insert_columns(column_t column, std::uint32_t amount);

    /// <summary>
    /// Delete rows before the given row index
    /// If the rows are deleted out of bounds (before the minimum or after the maximum allowed
    /// indices), an invalid_parameter exception will be thrown.
    /// </summary>
    void
    delete_rows(row_t row, std::uint32_t amount);

    /// <summary>
    /// Delete columns before the given column index
    /// If the columns are deleted out of bounds (before the minimum or after the maximum allowed
    /// indices), an invalid_parameter exception will be thrown.
    /// </summary>
    void
    delete_columns(column_t column, std::uint32_t amount);

    // properties

    /// <summary>
    /// Returns a reference to the column properties for the given column.
    /// If the given column does not have properties (has_column_properties() returns false),
    /// default column properties will be created and returned.
    /// </summary>
    xlnt::column_properties&
    column_properties(column_t column);

    /// <summary>
    /// Returns a reference to the column properties for the given column.
    /// If the given column does not have properties (has_column_properties() returns false),
    /// a key_not_found exception will be thrown.
    /// </summary>
    const xlnt::column_properties&
    column_properties(column_t column) const;

    /// <summary>
    /// Returns true if column properties have been set for the given column.
    /// </summary>
    bool
    has_column_properties(column_t column) const;

    /// <summary>
    /// Sets column properties for the given column to props.
    /// </summary>
    void
    add_column_properties(column_t column, const class column_properties& props);

    /// <summary>
    /// Calculates the width of the given column. This will be the default column width if
    /// a custom width is not set on this column's column_properties.
    /// </summary>
    double
    column_width(column_t column) const;

    /// <summary>
    /// Returns a reference to the row properties for the given row.
    /// If the given row does not have properties (has_row_properties() returns false),
    /// default row properties will be created and returned.
    /// </summary>
    xlnt::row_properties&
    row_properties(row_t row);

    /// <summary>
    /// Returns a reference to the row properties for the given row.
    /// If the given row does not have properties (has_row_properties() returns false),
    /// a key_not_found exception will be thrown.
    /// </summary>
    const xlnt::row_properties&
    row_properties(row_t row) const;

    /// <summary>
    /// Returns true if row properties have been set for the given row.
    /// </summary>
    bool
    has_row_properties(row_t row) const;

    /// <summary>
    /// Sets row properties for the given row to props.
    /// </summary>
    void
    add_row_properties(row_t row, const class row_properties& props);

    /// <summary>
    /// Calculate the height of the given row. This will be the default row height if
    /// a custom height is not set on this row's row_properties.
    /// </summary>
    double
    row_height(row_t row) const;

    // positioning

    /// <summary>
    /// Returns a reference to the cell at the given point coordinates.
    /// </summary>
    cell_reference
    point_pos(int left, int top) const;

    // named range

    /// <summary>
    /// Creates a new named range with the given name encompassing the string representing a range.
    /// </summary>
    void
    create_named_range(const std::string& name, const std::string& reference_string);

    /// <summary>
    /// Creates a new named range with the given name encompassing the given range reference.
    /// </summary>
    void
    create_named_range(const std::string& name, const range_reference& reference);

    /// <summary>
    /// Returns true if this worksheet contains a named range with the given name.
    /// </summary>
    bool
    has_named_range(const std::string& name) const;

    /// <summary>
    /// Returns the named range with the given name.
    /// Assumes that the named range exists (please call has_named_range() to check).
    /// Throws key_not_found exception if the named range doesn't exist.
    /// </summary>
    class range
    named_range(const std::string& name);

    /// <summary>
    /// Returns the named range with the given name.
    /// Assumes that the named range exists (please call has_named_range() to check).
    /// Throws key_not_found exception if the named range doesn't exist.
    /// </summary>
    const class range
    named_range(const std::string& name) const;

    /// <summary>
    /// Removes a named range with the given name.
    /// Assumes that the named range exists (please call has_named_range() to check).
    /// Throws key_not_found exception if the named range doesn't exist.
    /// </summary>
    void
    remove_named_range(const std::string& name);

    // extents

    /// <summary>
    /// Returns the row of the first non-empty cell in the worksheet.
    /// </summary>
    row_t
    lowest_row() const;

    /// <summary>
    /// Returns the row of the first non-empty cell or lowest row with properties in the worksheet.
    /// </summary>
    row_t
    lowest_row_or_props() const;

    /// <summary>
    /// Returns the row of the last non-empty cell in the worksheet.
    /// </summary>
    row_t
    highest_row() const;

    /// <summary>
    /// Returns the row of the last non-empty cell or highest row with properties in the worksheet.
    /// </summary>
    row_t
    highest_row_or_props() const;

    /// <summary>
    /// Returns the row directly below the last non-empty cell in the worksheet.
    /// </summary>
    row_t
    next_row() const;

    /// <summary>
    /// Returns the column of the first non-empty cell in the worksheet.
    /// </summary>
    column_t
    lowest_column() const;

    /// <summary>
    /// Returns the column of the first non-empty cell or lowest column with properties in the
    /// worksheet.
    /// </summary>
    column_t
    lowest_column_or_props() const;

    /// <summary>
    /// Returns the column of the last non-empty cell in the worksheet.
    /// </summary>
    column_t
    highest_column() const;

    /// <summary>
    /// Returns the column of the last non-empty cell or highest column with properties in the
    /// worksheet.
    /// </summary>
    column_t
    highest_column_or_props() const;

    /// <summary>
    /// Returns a range_reference pointing to the full range of cells in the worksheet.
    /// If skip_null is true (default), empty cells (For example if the first row or column is
    /// empty) will not be included in upper left bound of range. If skip_row_props is false
    /// (default), rows with only properties being defined will be returned too.
    /// </summary>
    range_reference
    calculate_dimension(bool skip_null = true, bool skip_row_props = false) const;

    // cell merge

    /// <summary>
    /// Merges the cells within the range represented by the given string.
    /// </summary>
    void
    merge_cells(const std::string& reference_string);

    /// <summary>
    /// Merges the cells within the given range.
    /// </summary>
    void
    merge_cells(const range_reference& reference);

    /// <summary>
    /// Removes the merging of the cells in the range represented by the given string.
    /// Assumes that the cells in the range have been merged (please call merged_ranges() to get all
    /// merged ranges). If the cell has not been merged, an invalid_parameter exception will be
    /// thrown.
    /// </summary>
    void
    unmerge_cells(const std::string& reference_string);

    /// <summary>
    /// Removes the merging of the cells in the given range.
    /// Assumes that the cells in the range have been merged (please call merged_ranges() to get all
    /// merged ranges). If the cell has not been merged, an invalid_parameter exception will be
    /// thrown.
    /// </summary>
    void
    unmerge_cells(const range_reference& reference);

    /// <summary>
    /// Returns a vector of references of all merged ranges in the worksheet.
    /// </summary>
    std::vector<range_reference>
    merged_ranges() const;

    // operators

    /// <summary>
    /// Returns true if this worksheet refers to the same worksheet as other.
    /// </summary>
    bool
    operator==(const worksheet& other) const;

    /// <summary>
    /// Returns true if this worksheet doesn't refer to the same worksheet as other.
    /// </summary>
    bool
    operator!=(const worksheet& other) const;

    /// <summary>
    /// Returns true if this worksheet is null.
    /// </summary>
    bool
    operator==(std::nullptr_t) const;

    /// <summary>
    /// Returns true if this worksheet is not null.
    /// </summary>
    bool
    operator!=(std::nullptr_t) const;

    /// <summary>
    /// Sets the internal pointer of this worksheet object to point to other.
    /// </summary>
    void
    operator=(const worksheet& other);

    /// <summary>
    /// Convenience method for worksheet::cell method.
    /// Returns a wrapper pointing to the cell at the given reference.
    /// If the cell doesn't exist (has_cell() returns false), an empty cell will be created,
    /// added to the worksheet (has_cell() will return true afterwards), and returned.
    /// </summary>
    class cell
    operator[](const cell_reference& reference);

    /// <summary>
    /// Convenience method for worksheet::cell method.
    /// Returns a wrapper pointing to the cell at the given reference.
    /// Assumes that the cell exists (please call has_cell() to check).
    /// If the cell doesn't exist, an invalid_parameter exception will be thrown.
    /// </summary>
    const class cell
    operator[](const cell_reference& reference) const;

    /// <summary>
    /// Returns true if this worksheet is equal to other. If compare_by_reference is true, the
    /// comparison will only check that both worksheets point to the same sheet in the same
    /// workbook. Otherwise, if compare_by_reference is false, all worksheet properties except for
    /// the id and title are compared.
    /// </summary>
    bool
    compare(const worksheet& other, bool compare_by_reference) const;

    // page

    /// <summary>
    /// Returns true if this worksheet has a page setup.
    /// </summary>
    bool
    has_page_setup() const;

    /// <summary>
    /// Returns a copy of the page setup for this sheet.
    /// If no page setup has been set (has_page_setup() returns false),
    /// a default-constructed page setup will be returned.
    /// </summary>
    xlnt::page_setup
    page_setup() const;

    /// <summary>
    /// Sets the page setup for this sheet to setup.
    /// </summary>
    void
    page_setup(const struct page_setup& setup);

    /// <summary>
    /// Returns true if this page has margins.
    /// </summary>
    bool
    has_page_margins() const;

    /// <summary>
    /// Returns a copy of the margins of this sheet.
    /// Assumes that this sheet has page margins (please call has_page_margins() to check).
    /// If this sheet has no page margins, an invalid_attribute exception will be thrown.
    /// </summary>
    xlnt::page_margins
    page_margins() const;

    /// <summary>
    /// Sets the margins of this sheet to margins.
    /// </summary>
    void
    page_margins(const class page_margins& margins);

    /// <summary>
    /// Clears the margins of this sheet (has_page_margins() will return false afterwards).
    /// </summary>
    void
    clear_page_margins();

    // auto filter

    /// <summary>
    /// Returns the current auto-filter of this sheet.
    /// Assumes that this sheet has an auto-filter (please call has_auto_filter() to check).
    /// If this sheet has no auto-filter, an invalid_attribute exception will be thrown.
    /// </summary>
    range_reference
    auto_filter() const;

    /// <summary>
    /// Sets the auto-filter of this sheet to the range defined by range_string.
    /// </summary>
    void
    auto_filter(const std::string& range_string);

    /// <summary>
    /// Sets the auto-filter of this sheet to the given range.
    /// </summary>
    void
    auto_filter(const xlnt::range& range);

    /// <summary>
    /// Sets the auto-filter of this sheet to the range defined by reference.
    /// </summary>
    void
    auto_filter(const range_reference& reference);

    /// <summary>
    /// Clear the auto-filter from this sheet.
    /// </summary>
    void
    clear_auto_filter();

    /// <summary>
    /// Returns true if this sheet has an auto-filter set.
    /// </summary>
    bool
    has_auto_filter() const;

    /// <summary>
    /// Reserve n rows. This can be optionally called before adding many rows
    /// to improve performance.
    /// </summary>
    void
    reserve(std::size_t n);

    /// <summary>
    /// Returns true if this sheet has phonetic properties
    /// </summary>
    bool
    has_phonetic_properties() const;

    /// <summary>
    /// Returns the phonetic properties of this sheet.
    /// Assumes that this sheet has phonetic properties (please call has_phonetic_properties() to
    /// check). If this sheet has no phonetic properties, a default-constructed phonetic_pr object
    /// will be returned.
    /// </summary>
    phonetic_pr
    phonetic_properties() const;

    /// <summary>
    /// Sets the phonetic properties of this sheet to phonetic_props
    /// </summary>
    void
    phonetic_properties(const phonetic_pr& phonetic_props);

    /// <summary>
    /// Returns true if this sheet has a header/footer.
    /// </summary>
    bool
    has_header_footer() const;

    /// <summary>
    /// Returns a copy of the header/footer of this sheet.
    /// Assumes that this sheet has a header/footer (please call has_header_footer() to check).
    /// If this sheet has no header/footer, an invalid_attribute exception will be thrown.
    /// </summary>
    class header_footer
    header_footer() const;

    /// <summary>
    /// Sets the header/footer of this sheet to new_header_footer.
    /// </summary>
    void
    header_footer(const class header_footer& new_header_footer);

    /// <summary>
    /// Returns the sheet state of this sheet.
    /// </summary>
    xlnt::sheet_state
    sheet_state() const;

    /// <summary>
    /// Sets the sheet state of this sheet.
    /// </summary>
    void
    sheet_state(xlnt::sheet_state state);

    /// <summary>
    /// Returns an iterator to the first row in this sheet.
    /// </summary>
    iterator
    begin();

    /// <summary>
    /// Returns an iterator one past the last row in this sheet.
    /// </summary>
    iterator
    end();

    /// <summary>
    /// Return a constant iterator to the first row in this sheet.
    /// </summary>
    const_iterator
    begin() const;

    /// <summary>
    /// Returns a constant iterator to one past the last row in this sheet.
    /// </summary>
    const_iterator
    end() const;

    /// <summary>
    /// Return a constant iterator to the first row in this sheet.
    /// </summary>
    const_iterator
    cbegin() const;

    /// <summary>
    /// Returns a constant iterator to one past the last row in this sheet.
    /// </summary>
    const_iterator
    cend() const;

    /// <summary>
    /// Sets rows to repeat at top during printing.
    /// </summary>
    void
    print_title_rows(row_t start, row_t end);

    /// <summary>
    /// Get rows to repeat at top during printing.
    /// </summary>
    optional<std::pair<row_t, row_t>>
    print_title_rows() const;

    /// <summary>
    /// Sets columns to repeat at left during printing.
    /// </summary>
    void
    print_title_cols(column_t start, column_t end);

    /// <summary>
    /// Get columns to repeat at left during printing.
    /// </summary>
    optional<std::pair<column_t, column_t>>
    print_title_cols() const;

    /// <summary>
    /// Returns true if the sheet has print titles defined.
    /// </summary>
    bool
    has_print_titles() const;

    /// <summary>
    /// Remove all print titles.
    /// </summary>
    void
    clear_print_titles();

    /// <summary>
    /// Sets the print area of this sheet to print_area.
    /// </summary>
    void
    print_area(const std::string& print_area);

    /// <summary>
    /// Clear the print area of this sheet.
    /// </summary>
    void
    clear_print_area();

    /// <summary>
    /// Returns the print area defined for this sheet.
    /// Assumes that this sheet has a print area (please call has_print_area() to check).
    /// If this sheet has no print area, an invalid_attribute exception will be thrown.
    /// </summary>
    range_reference
    print_area() const;

    /// <summary>
    /// Returns true if the print area is defined for this sheet.
    /// </summary>
    bool
    has_print_area() const;

    /// <summary>
    /// Returns true if this sheet has any number of views defined.
    /// </summary>
    bool
    has_view() const;

    /// <summary>
    /// Returns the views.
    /// </summary>
    const std::vector<sheet_view>&
    views() const;

    /// <summary>
    /// Returns the view at the given index.
    /// Assumes that the index is valid (please call views().size() to check).
    /// If the index is out of range, an invalid_parameter exception is thrown.
    /// </summary>
    sheet_view&
    view(std::size_t index = 0) const;

    /// <summary>
    /// Adds new_view to the set of available views for this sheet.
    /// </summary>
    void
    add_view(const sheet_view& new_view);

    /// <summary>
    /// Removes the view at the given index.
    /// Assumes that the index is valid (please call views().size() to check).
    /// If the index is out of range, an invalid_parameter exception is thrown.
    /// </summary>
    void
    remove_view(std::size_t index);

    /// <summary>
    /// Clears all views.
    /// </summary>
    void
    clear_views();

    /// <summary>
    /// Set the active cell on the default worksheet view to the given reference.
    /// </summary>
    void
    active_cell(const cell_reference& ref);

    /// <summary>
    /// Returns true if the worksheet has a view and the view has an active cell.
    /// </summary>
    bool
    has_active_cell() const;

    /// <summary>
    /// Returns the active cell on the default worksheet view.
    /// Assumes that this worksheet has an active cell (please call has_active_cell() to check).
    /// If this worksheet does not have an active cell, an xlnt::invalid_attribute exception will be
    /// thrown.
    /// </summary>
    cell_reference
    active_cell() const;

    // page breaks

    /// <summary>
    /// Remove all manual column and row page breaks (represented as dashed
    /// blue lines in the page view in Excel).
    /// </summary>
    void
    clear_page_breaks();

    /// <summary>
    /// Returns vector where each element represents a row which will break a page below it.
    /// </summary>
    const std::vector<row_t>&
    page_break_rows() const;

    /// <summary>
    /// Add a page break at the given row.
    /// </summary>
    void
    page_break_at_row(row_t row);

    /// <summary>
    /// Returns vector where each element represents a column which will break a page to the right.
    /// </summary>
    const std::vector<column_t>&
    page_break_columns() const;

    /// <summary>
    /// Add a page break at the given column.
    /// </summary>
    void
    page_break_at_column(column_t column);

    /// <summary>
    /// Creates a conditional format for the given range with the given condition and returns a
    /// wrapper pointing to it.
    /// </summary>
    xlnt::conditional_format
    conditional_format(const range_reference& ref, const condition& when);

    /// <summary>
    /// Returns the path of this worksheet in the containing package.
    /// </summary>
    xlnt::path
    path() const;

    /// <summary>
    /// Returns the relationship from the parent workbook to this worksheet.
    /// </summary>
    relationship
    referring_relationship() const;

    /// <summary>
    /// Returns a copy of the current formatting properties.
    /// </summary>
    sheet_format_properties
    format_properties() const;

    /// <summary>
    /// Sets the format properties to the given properties.
    /// </summary>
    void
    format_properties(const sheet_format_properties& properties);

    /// <summary>
    /// Returns true if this worksheet has a page setup.
    /// </summary>
    bool
    has_drawing() const;

    /// <summary>
    /// Returns true if this worksheet is empty.
    /// A worksheet is considered empty if it doesn't have any cells.
    /// </summary>
    bool
    is_empty() const;

    /// <summary>
    /// Get the zoom scale (percent) of the default sheetView. Defaults to 100 if unset.
    /// </summary>
    int
    zoom_scale() const;

    /// <summary>
    /// Set the zoom scale (percent) on the default sheetView.
    /// </summary>
    void
    zoom_scale(int scale);

    /// <summary>
    /// Configure outline settings for grouping rows and columns.
    /// These settings correspond to Excel's Data -> Group and Outline -> Settings menu.
    /// </summary>
    /// <param name="visible">If true, shows collapse buttons for groups (show outline
    /// symbols)</param> <param name="symbols_below">If true, summary rows appear below detail rows
    /// (collapse symbol below group)</param> <param name="symbols_right">If true, summary columns
    /// appear to the right of detail columns</param> <param name="apply_styles">If true, automatic
    /// styles are applied to outlined groups</param>
    void
    outline_settings(bool visible, bool symbols_below, bool symbols_right, bool apply_styles);

    /// <summary>
    /// Returns true if outline symbols (collapse buttons) are shown.
    /// Default is true if not explicitly set.
    /// </summary>
    bool
    show_outline_symbols() const;

    /// <summary>
    /// Returns true if summary rows are below detail rows.
    /// When true, the collapse symbol appears below the group.
    /// Default is true if not explicitly set.
    /// </summary>
    bool
    summary_below() const;

    /// <summary>
    /// Returns true if summary columns are to the right of detail columns.
    /// When true, the collapse symbol appears to the right of the group.
    /// Default is true if not explicitly set.
    /// </summary>
    bool
    summary_right() const;

    /// <summary>
    /// Returns true if automatic styles are applied to outlined groups.
    /// Default is false if not explicitly set.
    /// </summary>
    bool
    apply_styles() const;

private:
    friend class cell;
    friend class const_range_iterator;
    friend class range_iterator;
    friend class workbook;
    friend class detail::xlsx_consumer;
    friend class detail::xlsx_producer;

    /// <summary>
    /// Constructs a worksheet impl wrapper from d.
    /// </summary>
    worksheet(detail::worksheet_impl* d);

    /// <summary>
    /// Creates a comments part in the manifest as a relationship target of this sheet.
    /// </summary>
    void
    register_comments_in_manifest();

    /// <summary>
    /// Creates a calcChain part in the manifest if it doesn't already exist.
    /// </summary>
    void
    register_calc_chain_in_manifest();

    /// <summary>
    /// Removes calcChain part from manifest if no formulae remain in workbook.
    /// </summary>
    void
    garbage_collect_formulae();

    /// <summary>
    /// Sets the parent of this worksheet to wb.
    /// </summary>
    void
    parent(class workbook& wb);

    /// <summary>
    /// Move cells after index down or right by a given amount. The direction is decided by
    /// row_or_col. If reverse is true, the cells will be moved up or left, depending on row_or_col.
    /// If the cells are moved out of bounds (before the minimum or after the maximum allowed
    /// indices), an invalid_parameter exception will be thrown.
    /// </summary>
    void
    move_cells(
        std::uint32_t index, std::uint32_t amount, row_or_col_t row_or_col, bool reverse = false);

    /// <summary>
    /// The pointer to this sheet's implementation.
    /// </summary>
    detail::worksheet_impl* d_ = nullptr;
};

} // namespace xlnt
