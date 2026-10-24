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

#include <cstdint>
#include <functional>
#include <iterator>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <xlnt/xlnt_config.hpp>
#include <xlnt/internal/features.hpp>

#if XLNT_HAS_INCLUDE(<string_view>) && XLNT_HAS_FEATURE(U8_STRING_VIEW)
#include <string_view>
#endif

namespace xlnt {

enum class calendar;
enum class core_property;
enum class extended_property;
enum class relationship_type;

class alignment;
class border;
class calculation_properties;
class cell;
class cell_style;
class color;
class const_worksheet_iterator;
class fill;
class font;
class format;
class rich_text;
class manifest;
class metadata_property;
class named_range;
class number_format;
class path;
class pattern_fill;
class protection;
class range;
class range_reference;
class relationship;
class streaming_workbook_reader;
class style;
class style_serializer;
class theme;
class variant;
class workbook_view;
class worksheet;
class worksheet_iterator;
class zip_file;

struct datetime;

namespace detail {

    struct stylesheet;
    struct workbook_impl;
    struct worksheet_impl;
    class xlsx_consumer;
    class xlsx_producer;

} // namespace detail

/// <summary>
/// workbook is the container for all other parts of the document.
/// </summary>
class XLNT_API workbook
{
public:
    /// <summary>
    /// The method for cloning workbooks.
    /// </summary>
    enum class clone_method
    {
        deep_copy,
        shallow_copy
    };

    /// <summary>
    /// typedef for the iterator used for iterating through this workbook
    /// (non-const) in a range-based for loop.
    /// </summary>
    using iterator = worksheet_iterator;

    /// <summary>
    /// typedef for the iterator used for iterating through this workbook
    /// (const) in a range-based for loop.
    /// </summary>
    using const_iterator = const_worksheet_iterator;

    /// <summary>
    /// typedef for the iterator used for iterating through this workbook
    /// (non-const) in a range-based for loop in reverse order using
    /// std::make_reverse_iterator.
    /// </summary>
    using reverse_iterator = std::reverse_iterator<iterator>;

    /// <summary>
    /// typedef for the iterator used for iterating through this workbook
    /// (const) in a range-based for loop in reverse order using
    /// std::make_reverse_iterator.
    /// </summary>
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    /// <summary>
    /// Constructs and returns an empty workbook similar to a default.
    /// Excel workbook
    /// </summary>
    static workbook
    empty();

    // Constructors

    /// <summary>
    /// Default constructor. Constructs a workbook containing a single empty
    /// worksheet using workbook::empty().
    /// </summary>
    workbook();

    /// <summary>
    /// load the xlsx file at path
    /// </summary>
    workbook(const xlnt::path& file);

    /// <summary>
    /// load the encrpyted xlsx file at path
    /// </summary>
    workbook(const xlnt::path& file, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// load the encrpyted xlsx file at path
    /// </summary>
    workbook(const xlnt::path& file, std::u8string_view password);
#endif

    /// <summary>
    /// construct the workbook from any data stream where the data is the binary form of a workbook
    /// </summary>
    workbook(std::istream& data);

    /// <summary>
    /// construct the workbook from any data stream where the data is the binary form of an
    /// encrypted workbook
    /// </summary>
    workbook(std::istream& data, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// construct the workbook from any data stream where the data is the binary form of an
    /// encrypted workbook
    /// </summary>
    workbook(std::istream& data, std::u8string_view password);
#endif

    /// <summary>
    /// Move constructor. Constructs a workbook from existing workbook, other.
    /// </summary>
    workbook(workbook&& other) = default;

    /// <summary>
    /// Copy constructor. Constructs this workbook from existing workbook, other.
    /// Creates a shallow copy, copying the workbook's internal pointers.
    /// </summary>
    workbook(const workbook& other) = default;

    /// <summary>
    /// Destroys this workbook, deallocating all internal storage space. Any pimpl
    /// wrapper classes (e.g. cell) pointing into this workbook will be invalid
    /// after this is executed.
    /// </summary>
    ~workbook() = default;

    /// <summary>
    /// Creates a clone of this workbook. A shallow copy will copy the workbook's internal pointers,
    /// while a deep copy will copy all the internal structures and create a full clone of the
    /// workbook.
    /// </summary>
    workbook
    clone(clone_method method) const;

    // Worksheets

    /// <summary>
    /// Creates a sheet after the last sheet in this workbook and returns a wrapper pointing to it.
    /// </summary>
    worksheet
    create_sheet();

    /// <summary>
    /// Creates a sheet at the specified index and returns a wrapper pointing to it.
    /// </summary>
    worksheet
    create_sheet(std::size_t index);

    /// <summary>
    /// Creates a sheet wit the specified title and relationship
    /// and returns a wrapper pointing to it.
    /// TODO: This should be private...
    /// </summary>
    worksheet
    create_sheet_with_rel(const std::string& title, const relationship& rel);

    /// <summary>
    /// Creates a new sheet after the last sheet initializing it
    /// with all of the data from the provided worksheet.
    /// Returns a wrapper pointing to the copied sheet.
    /// The worksheet to be copied needs to be part of the same workbook (have the same parent)
    /// as this workbook - otherwise, an invalid_parameter exception will be thrown.
    /// </summary>
    worksheet
    copy_sheet(worksheet worksheet);

    /// <summary>
    /// Creates a new sheet at the specified index initializing it
    /// with all of the data from the provided worksheet.
    /// Returns a wrapper pointing to the copied sheet.
    /// The worksheet to be copied needs to be part of the same workbook (have the same parent)
    /// as this workbook - otherwise, an invalid_parameter exception will be thrown.
    /// </summary>
    worksheet
    copy_sheet(worksheet worksheet, std::size_t index);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet that is determined to be active. An active
    /// sheet is that which is initially shown by the spreadsheet editor.
    /// </summary>
    worksheet
    active_sheet();

    /// <summary>
    /// Sets the worksheet that is determined to be active. An active
    /// sheet is that which is initially shown by the spreadsheet editor.
    /// </summary>
    void
    active_sheet(std::size_t index);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet with the given name. This will throw a
    /// key_not_found exception if the sheet isn't found. Use workbook::contains(const std::string
    /// &) to make sure the sheet exists before calling this method.
    /// </summary>
    worksheet
    sheet_by_title(const std::string& title);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet with the given name. This will throw a
    /// key_not_found exception if the sheet isn't found. Use workbook::contains(const std::string
    /// &) to make sure the sheet exists before calling this method.
    /// </summary>
    const worksheet
    sheet_by_title(const std::string& title) const;

    /// <summary>
    /// Returns a wrapper pointing to the worksheet at the given index. Assumes that the index is
    /// valid (please call sheet_count() to check). This method will throw an invalid_parameter
    /// exception if index is greater than or equal to the number of sheets in this workbook.
    /// </summary>
    worksheet
    sheet_by_index(std::size_t index);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet at the given index. Assumes that the index is
    /// valid (please call sheet_count() to check). This method will throw an invalid_parameter
    /// exception if index is greater than or equal to the number of sheets in this workbook.
    /// </summary>
    const worksheet
    sheet_by_index(std::size_t index) const;

    /// <summary>
    /// Returns whether this workbook has a sheet with the specified ID.
    /// Most users won't need this.
    /// </summary>
    bool
    has_sheet_id(std::size_t id) const;

    /// <summary>
    /// Returns a wrapper pointing to the worksheet with a sheetId of id. Sheet IDs are arbitrary
    /// numbers that uniquely identify a sheet. Assumes that the ID is valid (please call
    /// has_sheet_id() to check). If the ID is invalid, a key_not_found exception will be thrown.
    /// Most users won't need this.
    /// </summary>
    worksheet
    sheet_by_id(std::size_t id);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet with a sheetId of id. Sheet IDs are arbitrary
    /// numbers that uniquely identify a sheet. Assumes that the ID is valid (please call
    /// has_sheet_id() to check). If the ID is invalid, a key_not_found exception will be thrown.
    /// Most users won't need this.
    /// </summary>
    const worksheet
    sheet_by_id(std::size_t id) const;

    /// <summary>
    /// Returns the hidden identifier of the worksheet at the given index. Assumes that the index is
    /// valid (please call sheet_count() to check). This will throw an invalid_parameter exception
    /// if index is greater than or equal to the number of sheets in this workbook.
    /// </summary>
    bool
    sheet_hidden_by_index(std::size_t index) const;

    /// <summary>
    /// Returns true if this workbook contains a sheet with the given title.
    /// </summary>
    bool
    contains(const std::string& title) const;

    /// <summary>
    /// Returns the index of the given worksheet. The worksheet must be owned by this workbook.
    /// If the worksheet is not owned by this workbook, an invalid_parameter exception will be
    /// thrown.
    /// </summary>
    std::size_t
    index(worksheet worksheet) const;

    /// <summary>
    /// Moves a sheet to a new position defined. The worksheet must be owned by this workbook.
    /// Assumes that the index is valid (please call sheet_count() to check).
    /// This method will throw an invalid_parameter exception if index is greater than or equal to
    /// the number of sheets in this workbook, or if the worksheet is not part of (not owned by)
    /// this workbook.
    /// </summary>
    void
    move_sheet(worksheet worksheet, std::size_t newIndex);

    // remove worksheets

    /// <summary>
    /// Removes the given worksheet from this workbook. The worksheet must be owned by this
    /// workbook. If the worksheet is not part of (not owned by) this workbook, an invalid_parameter
    /// exception will be thrown.
    /// </summary>
    void
    remove_sheet(worksheet worksheet);

    /// <summary>
    /// Sets the contents of this workbook to be equivalent to that of
    /// a workbook returned by workbook::empty().
    /// </summary>
    void
    clear();

    // iterators

    /// <summary>
    /// Returns an iterator to the first worksheet in this workbook.
    /// </summary>
    iterator
    begin();

    /// <summary>
    /// Returns an iterator to the worksheet following the last worksheet of the workbook.
    /// This worksheet acts as a placeholder; attempting to access it will cause
    /// an xlnt::invalid_parameter exception to be thrown.
    /// </summary>
    iterator
    end();

    /// <summary>
    /// Returns a const iterator to the first worksheet in this workbook.
    /// </summary>
    const_iterator
    begin() const;

    /// <summary>
    /// Returns a const iterator to the worksheet following the last worksheet of the workbook.
    /// This worksheet acts as a placeholder; attempting to access it will cause
    /// an xlnt::invalid_parameter exception to be thrown.
    /// </summary>
    const_iterator
    end() const;

    /// <summary>
    /// Returns an iterator to the first worksheet in this workbook.
    /// </summary>
    const_iterator
    cbegin() const;

    /// <summary>
    /// Returns a const iterator to the worksheet following the last worksheet of the workbook.
    /// This worksheet acts as a placeholder; attempting to access it will cause
    /// an xlnt::invalid_parameter exception to be thrown.
    /// </summary>
    const_iterator
    cend() const;

    /// <summary>
    /// Applies the function "f" to every non-empty cell in every worksheet in this workbook.
    /// </summary>
    void
    apply_to_cells(std::function<void(cell)> f);

    /// <summary>
    /// Returns a temporary vector containing the titles of each sheet in the order
    /// of the sheets in the workbook.
    /// </summary>
    std::vector<std::string>
    sheet_titles() const;

    /// <summary>
    /// Returns the number of sheets in this workbook.
    /// </summary>
    std::size_t
    sheet_count() const;

    // Metadata Properties

    /// <summary>
    /// Returns true if the workbook has the core property with the given name.
    /// </summary>
    bool
    has_core_property(xlnt::core_property type) const;

    /// <summary>
    /// Returns a vector of the type of each core property that is set to
    /// a particular value in this workbook.
    /// </summary>
    std::vector<xlnt::core_property>
    core_properties() const;

    /// <summary>
    /// Returns a copy of the value of the given core property.
    /// Assumes that the specified core_property exists (please call has_core_property() to check).
    /// If the specified core_property does not exist, a null variant will be returned.
    /// </summary>
    variant
    core_property(xlnt::core_property type) const;

    /// <summary>
    /// Sets the given core property to the provided value.
    /// </summary>
    void
    core_property(xlnt::core_property type, const variant& value);

    /// <summary>
    /// Returns true if the workbook has the extended property with the given name.
    /// </summary>
    bool
    has_extended_property(xlnt::extended_property type) const;

    /// <summary>
    /// Returns a vector of the type of each extended property that is set to
    /// a particular value in this workbook.
    /// </summary>
    std::vector<xlnt::extended_property>
    extended_properties() const;

    /// <summary>
    /// Returns a copy of the value of the given extended property.
    /// Assumes that the specified extended_property exists (please call has_extended_property() to
    /// check). If the specified extended_property does not exist, a null variant will be returned.
    /// </summary>
    variant
    extended_property(xlnt::extended_property type) const;

    /// <summary>
    /// Sets the given extended property to the provided value.
    /// </summary>
    void
    extended_property(xlnt::extended_property type, const variant& value);

    /// <summary>
    /// Returns true if the workbook has the custom property with the given name.
    /// </summary>
    bool
    has_custom_property(const std::string& property_name) const;

    /// <summary>
    /// Returns a vector of the name of each custom property that is set to
    /// a particular value in this workbook.
    /// </summary>
    std::vector<std::string>
    custom_properties() const;

    /// <summary>
    /// Returns a copy of the value of the given custom property.
    /// Assumes that the specified custom_property exists (please call has_custom_property() to
    /// check). If the specified custom_property does not exist, a null variant will be returned.
    /// </summary>
    variant
    custom_property(const std::string& property_name) const;

    /// <summary>
    /// Creates a new custom property in this workbook and sets it to the provided value.
    /// </summary>
    void
    custom_property(const std::string& property_name, const variant& value);

    /// <summary>
    /// Returns the base date used by this workbook. This will generally be windows_1900
    /// except on Apple based systems when it will default to mac_1904 unless otherwise
    /// set via `void workbook::base_date(calendar base_date)`.
    /// </summary>
    calendar
    base_date() const;

    /// <summary>
    /// Sets the base date style of this workbook. This is the date and time that
    /// a numeric value of 0 represents.
    /// </summary>
    void
    base_date(calendar base_date);

    /// <summary>
    /// Returns true if this workbook has had its title set.
    /// </summary>
    bool
    has_title() const;

    /// <summary>
    /// Returns the title of this workbook.
    /// Assumes that this workbook has a title (please call has_title() to check).
    /// If this workbook has no title, an invalid_attribute exception will be thrown.
    /// </summary>
    std::string
    title() const;

    /// <summary>
    /// Sets the title of this workbook to title.
    /// </summary>
    void
    title(const std::string& title);

    /// <summary>
    /// Sets the absolute path of this workbook to path.
    /// </summary>
    void
    abs_path(const std::string& path);

    /// <summary>
    /// Sets the ArchID flags of this workbook to flags.
    /// </summary>
    void
    arch_id_flags(const std::size_t flags);

    // Named Ranges

    /// <summary>
    /// Returns a vector of the named ranges in this workbook.
    /// </summary>
    std::vector<xlnt::named_range>
    named_ranges() const;

    /// <summary>
    /// Creates a new names range.
    /// </summary>
    void
    create_named_range(
        const std::string& name, worksheet worksheet, const range_reference& reference);

    /// <summary>
    /// Creates a new named range.
    /// </summary>
    void
    create_named_range(
        const std::string& name, worksheet worksheet, const std::string& reference_string);

    /// <summary>
    /// Returns true if a named range of the given name exists in the workbook.
    /// </summary>
    bool
    has_named_range(const std::string& name) const;

    /// <summary>
    /// Returns the named range with the given name.
    /// Assumes that the specified named_range exists (please call has_named_range() to check).
    /// If the specified named_range does not exist, an xlnt::key_not_found exception will be
    /// thrown.
    /// </summary>
    class range
    named_range(const std::string& name);

    /// <summary>
    /// Deletes the named range with the given name.
    /// Assumes that the specified named_range exists (please call has_named_range() to check).
    /// If the specified named_range does not exist, an xlnt::key_not_found exception will be
    /// thrown.
    /// </summary>
    void
    remove_named_range(const std::string& name);

    // Serialization/Deserialization

    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the bytes into
    /// byte vector data.
    /// </summary>
    void
    save(std::vector<std::uint8_t>& data) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and saves the bytes into byte vector data.
    /// </summary>
    void
    save(std::vector<std::uint8_t>& data, const std::string& password) const;

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and saves the bytes into byte vector data.
    /// </summary>
    void
    save(std::vector<std::uint8_t>& data, std::u8string_view password) const;
#endif

    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the data into a file
    /// named filename.
    /// </summary>
    void
    save(const std::string& filename) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    void
    save(const std::string& filename, const std::string& password) const;

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the data into a file
    /// named filename.
    /// </summary>
    void
    save(std::u8string_view filename) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    void
    save(std::u8string_view filename, std::u8string_view password) const;
#endif

#ifdef _MSC_VER
    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the data into a file
    /// named filename.
    /// </summary>
    void
    save(const std::wstring& filename) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    void
    save(const std::wstring& filename, const std::string& password) const;
#endif

    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the data into a file
    /// named filename.
    /// </summary>
    void
    save(const xlnt::path& filename) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    void
    save(const xlnt::path& filename, const std::string& password) const;

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    void
    save(const xlnt::path& filename, std::u8string_view password) const;
#endif

    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the data into stream.
    /// </summary>
    void
    save(std::ostream& stream) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into the given stream.
    /// </summary>
    void
    save(std::ostream& stream, const std::string& password) const;

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into the given stream.
    /// </summary>
    void
    save(std::ostream& stream, std::u8string_view password) const;
#endif

    /// <summary>
    /// Interprets byte vector data as an XLSX file and sets the content of this
    /// workbook to match that file.
    /// If the workbook requires a password (which is not provided), an xlnt::invalid_password will
    /// be thrown. If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    void
    load(const std::vector<std::uint8_t>& data);

    /// <summary>
    /// Interprets byte vector data as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    void
    load(const std::vector<std::uint8_t>& data, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Interprets byte vector data as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    void
    load(const std::vector<std::uint8_t>& data, std::u8string_view password);
#endif

    /// <summary>
    /// Interprets file with the given filename as an XLSX file and sets
    /// the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the workbook requires a password (which is not provided), an
    /// xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const std::string& filename);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const std::string& filename, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Interprets file with the given filename as an XLSX file and sets
    /// the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the workbook requires a password (which is not provided), an
    /// xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(std::u8string_view filename);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(std::u8string_view filename, std::u8string_view password);
#endif

#ifdef _MSC_VER
    /// <summary>
    /// Interprets file with the given filename as an XLSX file and sets
    /// the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the workbook requires a password (which is not provided), an
    /// xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const std::wstring& filename);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const std::wstring& filename, const std::string& password);
#endif

    /// <summary>
    /// Interprets file with the given filename as an XLSX file and sets the
    /// content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the workbook requires a password (which is not provided), an
    /// xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const xlnt::path& filename);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const xlnt::path& filename, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown. If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// </summary>
    void
    load(const xlnt::path& filename, std::u8string_view password);
#endif

    /// <summary>
    /// Interprets data in stream as an XLSX file and sets the content of this
    /// workbook to match that file.
    /// If the workbook requires a password (which is not provided), an xlnt::invalid_password will
    /// be thrown. If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    void
    load(std::istream& stream);

    /// <summary>
    /// Interprets data in stream as an XLSX file encrypted with the given password
    /// and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    void
    load(std::istream& stream, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    /// <summary>
    /// Interprets data in stream as an XLSX file encrypted with the given password
    /// and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    void
    load(std::istream& stream, std::u8string_view password);
#endif

    // View

    /// <summary>
    /// Returns true if this workbook has a view.
    /// </summary>
    bool
    has_view() const;

    /// <summary>
    /// Returns a copy of the view.
    /// Assumes that the view exists (please call has_view() to check).
    /// If the view does not exist, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    workbook_view
    view() const;

    /// <summary>
    /// Sets the view to view.
    /// </summary>
    void
    view(const workbook_view& view);

    // Properties

    /// <summary>
    /// Returns true if a code name has been set for this workbook.
    /// </summary>
    bool
    has_code_name() const;

    /// <summary>
    /// Returns the code name that was set for this workbook.
    /// Assumes that the code_name exists (please call has_code_name() to check).
    /// If the code_name does not exist, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    std::string
    code_name() const;

    /// <summary>
    /// Sets the code name of this workbook to code_name.
    /// </summary>
    void
    code_name(const std::string& code_name);

    /// <summary>
    /// Returns true if this workbook has a file version.
    /// </summary>
    bool
    has_file_version() const;

    /// <summary>
    /// Clears the information contained by the file version (e.g. AppName, LastEdited,
    /// LowestEdited, RupBuild).
    /// </summary>
    void
    clear_file_version();

    /// <summary>
    /// Returns true if this workbook has a non-empty AppName workbook file property.
    /// </summary>
    bool
    has_app_name() const;

    /// <summary>
    /// Returns the AppName workbook file property.
    /// Assumes that this workbook has an AppName property (please call has_app_name() to check).
    /// If this workbook has no AppName property, an empty string will be returned.
    /// </summary>
    const std::string&
    app_name() const;

    /// <summary>
    /// Sets the AppName workbook file property. Creates the file version information if it does not
    /// exist yet.
    /// </summary>
    void
    app_name(const std::string& app_name);

    /// <summary>
    /// Returns true if this workbook has a non-empty LastEdited workbook file property.
    /// </summary>
    bool
    has_last_edited() const;

    /// <summary>
    /// Returns the LastEdited workbook file property, as a string.
    /// Assumes that this workbook has a LastEdited property (please call has_last_edited() to
    /// check). If this workbook has no LastEdited property, an empty string will be returned.
    /// </summary>
    const std::string&
    last_edited_str() const;

    /// <summary>
    /// Returns the LastEdited workbook file property, parsed from a string to a size_t.
    /// Assumes that this workbook has a LastEdited property (please call has_last_edited() to
    /// check). If this workbook has no LastEdited property, an invalid_attribute exception will be
    /// thrown. If the LastEdited property cannot be parsed as a size_t, an invalid_attribute
    /// exception will be thrown.
    /// </summary>
    std::size_t
    last_edited() const;

    /// <summary>
    /// Sets the LastEdited workbook file property, as a string.
    /// Creates the file version information if it does not exist yet.
    /// </summary>
    void
    last_edited(const std::string& last_edited);

    /// <summary>
    /// Sets the LastEdited workbook file property, as a number (internally converted to a string).
    /// Creates the file version information if it does not exist yet.
    /// </summary>
    void
    last_edited(std::size_t last_edited);

    /// <summary>
    /// Returns true if this workbook has a non-empty LowestEdited workbook file property.
    /// </summary>
    bool
    has_lowest_edited() const;

    /// <summary>
    /// Returns the LowestEdited workbook file property, as a string.
    /// Assumes that this workbook has a LowestEdited property (please call has_lowest_edited() to
    /// check). If this workbook has no LowestEdited property, an empty string will be returned.
    /// </summary>
    const std::string&
    lowest_edited_str() const;

    /// <summary>
    /// Returns the LowestEdited workbook file property, parsed from a string to size_t.
    /// Assumes that this workbook has a LowestEdited property (please call has_lowest_edited() to
    /// check). If this workbook has no LowestEdited property, an invalid_attribute exception will
    /// be thrown. If the LowestEdited property cannot be parsed as a size_t, an invalid_attribute
    /// exception will be thrown.
    /// </summary>
    std::size_t
    lowest_edited() const;

    /// <summary>
    /// Sets the LowestEdited workbook file property, as a string.
    /// Creates the file version information if it does not exist yet.
    /// </summary>
    void
    lowest_edited(const std::string& lowest_edited);

    /// <summary>
    /// Sets the LowestEdited workbook file property, as a number (internally converted to a
    /// string). Creates the file version information if it does not exist yet.
    /// </summary>
    void
    lowest_edited(std::size_t lowest_edited);

    /// <summary>
    /// Returns true if this workbook has a non-empty RupBuild workbook file property.
    /// </summary>
    bool
    has_rup_build() const;

    // <summary>
    /// Returns the RupBuild workbook file property, as a string.
    /// Assumes that this workbook has a RupBuild property (please call has_rup_build() to check).
    /// If this workbook has no RupBuild property, an empty string will be returned.
    /// </summary>
    const std::string&
    rup_build_str() const;

    /// <summary>
    /// Returns the RupBuild workbook file property, parsed from a string to size_t.
    /// Assumes that this workbook has a RupBuild property (please call has_rup_build() to check).
    /// If this workbook has no RupBuild property, an invalid_attribute exception will be thrown.
    /// If the RupBuild property cannot be parsed as a size_t, an invalid_attribute exception will
    /// be thrown.
    /// </summary>
    std::size_t
    rup_build() const;

    /// <summary>
    /// Sets the RupBuild workbook file property, as a string.
    /// Creates the file version information if it does not exist yet.
    /// </summary>
    void
    rup_build(const std::string& rup_build);

    /// <summary>
    /// Sets the RupBuild workbook file property, as a number (internally converted to a string).
    /// Creates the file version information if it does not exist yet.
    /// </summary>
    void
    rup_build(std::size_t rup_build);

    // Theme

    /// <summary>
    /// Returns true if this workbook has a theme defined.
    /// </summary>
    bool
    has_theme() const;

    /// <summary>
    /// Returns a const reference to this workbook's theme.
    /// Assumes that this workbook has a theme (please call has_theme() to check).
    /// If this workbook has no theme, an invalid_attribute exception will be thrown.
    /// </summary>
    const xlnt::theme&
    theme() const;

    /// <summary>
    /// Sets the theme to value.
    /// </summary>
    void
    theme(const class theme& value);

    // Formats

    /// <summary>
    /// Returns a wrapper pointing to the cell format at the given index. The index is the position
    /// of the format in xl/styles.xml.
    /// </summary>
    xlnt::format
    format(std::size_t format_index);

    /// <summary>
    /// Returns the cell format at the given index. The index is the position of
    /// the format in xl/styles.xml.
    /// </summary>
    const xlnt::format
    format(std::size_t format_index) const;

    /// <summary>
    /// Creates a new format and returns a wrapper pointing to it.
    /// </summary>
    xlnt::format
    create_format(bool default_format = false);

    /// <summary>
    /// Clear all cell-level formatting and formats from the styelsheet. This leaves
    /// all other styling in place (e.g. named styles).
    /// </summary>
    void
    clear_formats();

    /// <summary>
    /// Returns the number of formats in this workbook.
    /// </summary>
    std::size_t
    format_count() const;

    // Styles

    /// <summary>
    /// Returns true if this workbook has a style with the given name.
    /// </summary>
    bool
    has_style(const std::string& name) const;

    /// <summary>
    /// Returns a wrapper pointing to the named style with the given name.
    /// Assumes that this workbook has a style with the given name (please call has_style() to
    /// check). If this workbook has no style with the given name, a key_not_found exception will be
    /// thrown.
    /// </summary>
    class style
    style(const std::string& name);

    /// <summary>
    /// Returns a wrapper pointing to the named style with the given name.
    /// Assumes that this workbook has a style with the given name (please call has_style() to
    /// check). If this workbook has no style with the given name, a key_not_found exception will be
    /// thrown.
    /// </summary>
    const class style
    style(const std::string& name) const;

    /// <summary>
    /// Creates a new style and returns a wrapper pointing to it.
    /// </summary>
    class style
    create_style(const std::string& name);

    /// <summary>
    /// Creates a new style and returns a wrapper pointing to it.
    /// Assumes that the builtin ID exists.
    /// If the builtin ID does not exist, an invalid_parameter exception will be thrown.
    /// </summary>
    class style
    create_builtin_style(std::size_t builtin_id);

    /// <summary>
    /// Clear all named styles from cells and remove the styles from
    /// from the styelsheet. This leaves all other styling in place
    /// (e.g. cell formats).
    /// </summary>
    void
    clear_styles();

    /// <summary>
    /// Sets the default slicer style to the given value.
    /// </summary>
    void
    default_slicer_style(const std::string& value);

    /// <summary>
    /// Returns the default slicer style.
    /// </summary>
    std::string
    default_slicer_style() const;

    /// <summary>
    /// Enables knownFonts in stylesheet.
    /// </summary>
    void
    enable_known_fonts();

    /// <summary>
    /// Disables knownFonts in stylesheet.
    /// </summary>
    void
    disable_known_fonts();

    /// <summary>
    /// Returns true if knownFonts are enabled in the stylesheet.
    /// </summary>
    bool
    known_fonts_enabled() const;

    // Manifest

    /// <summary>
    /// Returns a reference to the workbook's internal manifest.
    /// </summary>
    class manifest&
    manifest();

    /// <summary>
    /// Returns a reference to the workbook's internal manifest.
    /// </summary>
    const class manifest&
    manifest() const;

    // shared strings

    /// <summary>
    /// Append a shared string to the shared string collection in this workbook.
    /// This should not generally be called unless you know what you're doing.
    /// If allow_duplicates is false and the string is already in the collection,
    /// it will not be added. Returns the index of the added string.
    /// </summary>
    std::size_t
    add_shared_string(const rich_text& shared, bool allow_duplicates = false);

    /// <summary>
    /// Returns a reference to the shared string related to the specified index
    /// </summary>
    const rich_text&
    shared_strings(std::size_t index) const;

    /// <summary>
    /// Returns a reference to the shared strings being used by cells
    /// in this workbook.
    /// </summary>
    std::vector<rich_text>&
    shared_strings();

    /// <summary>
    /// Returns a reference to the shared strings being used by cells
    /// in this workbook.
    /// </summary>
    const std::vector<rich_text>&
    shared_strings() const;

    // Thumbnail

    /// <summary>
    /// Sets the workbook's thumbnail to the given vector of bytes, thumbnail,
    /// with the given extension (e.g. jpg) and content_type (e.g. image/jpeg).
    /// </summary>
    void
    thumbnail(const std::vector<std::uint8_t>& thumbnail, const std::string& extension,
        const std::string& content_type);

    /// <summary>
    /// Returns true if this workbook has a thumbnail.
    /// </summary>
    bool
    has_thumbnail() const;

    /// <summary>
    /// Returns a vector of bytes representing the workbook's thumbnail.
    /// Assumes that this workbook has a thumbnail (please call has_thumbnail() to check).
    /// If the workbook has no thumbnail, an invalid_attribute exception will be thrown.
    /// </summary>
    const std::vector<std::uint8_t>&
    thumbnail() const;

    /// <summary>
    /// Returns stored binary data.
    /// </summary>
    const std::unordered_map<std::string, std::vector<std::uint8_t>>&
    binaries() const;

    // Calculation properties

    /// <summary>
    /// Returns true if this workbook has any calculation properties set.
    /// </summary>
    bool
    has_calculation_properties() const;

    /// <summary>
    /// Clears the calculation properties of this workbook.
    /// </summary>
    void
    clear_calculation_properties();

    /// <summary>
    /// Returns the a copy of the calculation properties used in this workbook.
    /// Assumes that this workbook has calculation properties (please call
    /// has_calculation_properties() to check). If this workbook has no calculation properties, a
    /// default-constructed calculation_properties object will be returned.
    /// </summary>
    class calculation_properties
    calculation_properties() const;

    /// <summary>
    /// Sets the calculation properties of this workbook to props.
    /// </summary>
    void
    calculation_properties(const class calculation_properties& props);

    /// <summary>
    /// Returns true if this workbook is equal to other. If compare_by_reference is true, the
    /// comparison will only check that both workbook instances point to the same internal workbook.
    /// Otherwise, if compare_by_reference is false, all workbook properties except for the abs_path
    /// are compared.
    /// </summary>
    bool
    compare(const workbook& other, bool compare_by_reference) const;

    // Operators

    /// <summary>
    /// Set the contents of this workbook to be equal to those of "other".
    /// Creates a shallow copy, copying the workbook's internal pointers.
    /// </summary>
    workbook&
    operator=(const workbook& other)
        = default;

    /// <summary>
    /// Set the contents of this workbook to be equal to those of "other"
    /// by consuming (moving) the "other" instance.
    /// </summary>
    workbook&
    operator=(workbook&& other)
        = default;

    /// <summary>
    /// Returns a wrapper pointing to the worksheet with a title of "name". This will throw a
    /// key_not_found exception if the sheet isn't found. Use workbook::contains(const std::string
    /// &) to make sure the sheet exists before calling this method.
    /// </summary>
    worksheet
    operator[](const std::string& name);

    /// <summary>
    /// Returns a wrapper pointing to the worksheet at "index".
    /// This method will throw an invalid_parameter exception if index is greater than or equal to
    /// the number of sheets in this workbook (call sheet_count() to check).
    /// </summary>
    worksheet
    operator[](std::size_t index);

    /// <summary>
    /// Return true if this workbook internal implementation points to the same
    /// memory as rhs's.
    /// </summary>
    bool
    operator==(const workbook& rhs) const;

    /// <summary>
    /// Return true if this workbook internal implementation doesn't point to the same
    /// memory as rhs's.
    /// </summary>
    bool
    operator!=(const workbook& rhs) const;

private:
    friend class cell;
    friend class streaming_workbook_reader;
    friend class worksheet;
    friend class detail::xlsx_consumer;
    friend class detail::xlsx_producer;
    friend struct detail::worksheet_impl;

    /// <summary>
    /// Creates a deep copy of source_format in this workbook's stylesheet.
    /// All format properties (alignment, border, fill, font, number format, protection)
    /// are cloned using public API calls to avoid unnecessary duplication.
    /// Pivot button and quote prefix properties are copied directly.
    /// If source_format has a style, the cloned format is associated with the same style name.
    /// NOTE: Deep cloning of the style itself is not yet implemented.
    /// The cloned format will be assigned the style name from the source format.
    /// If a style with the same name and properties already exists in the destination workbook,
    /// it will be reused.
    /// Returns the newly created format in this workbook.
    /// </summary>
    xlnt::format
    clone_format_from(const xlnt::format& source_format);

    /// <summary>
    /// Returns true if the given format belongs to this workbook's stylesheet.
    /// Used internally to detect cross-workbook format references that could cause dangling
    /// pointers.
    /// </summary>
    bool
    owns_format(const class format& fmt) const;

    /// <summary>
    /// Private constructor. Constructs a workbook from an implementation pointer.
    /// Used by static constructor to resolve circular construction dependency.
    /// </summary>
    workbook(std::shared_ptr<detail::workbook_impl> impl);

    /// <summary>
    /// Private constructor. Constructs a workbook from an implementation pointer.
    /// Used by static constructor to resolve circular construction dependency.
    /// </summary>
    workbook(std::weak_ptr<detail::workbook_impl> impl);

    /// <summary>
    /// Internal function to set the impl.
    /// </summary>
    void
    set_impl(std::shared_ptr<detail::workbook_impl> impl);

    /// <summary>
    /// load the encrpyted xlsx file at path
    /// </summary>
    template <typename T>
    void
    construct(const xlnt::path& file, const T& password);

    /// <summary>
    /// construct the workbook from any data stream where the data is the binary form of an
    /// encrypted workbook
    /// </summary>
    template <typename T>
    void
    construct(std::istream& data, const T& password);

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and saves the bytes into byte vector data.
    /// </summary>
    template <typename T>
    void
    save_internal(std::vector<std::uint8_t>& data, const T& password) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file and saves the data into a file
    /// named filename.
    /// </summary>
    template <typename T>
    void
    save_internal(const T& filename) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    template <typename T>
    void
    save_internal(const T& filename, const T& password) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into a file named filename.
    /// </summary>
    template <typename T>
    void
    save_internal(const xlnt::path& filename, const T& password) const;

    /// <summary>
    /// Serializes the workbook into an XLSX file encrypted with the given password
    /// and loads the bytes into the given stream.
    /// </summary>
    template <typename T>
    void
    save_internal(std::ostream& stream, const T& password) const;

    /// <summary>
    /// Interprets byte vector data as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    template <typename T>
    void
    load_internal(const std::vector<std::uint8_t>& data, const T& password);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file and sets
    /// the content of this workbook to match that file.
    /// If the workbook requires a password (which is not provided), an xlnt::invalid_password will
    /// be thrown. If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown.
    /// </summary>
    template <typename T>
    void
    load_internal(const T& filename);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown.
    /// </summary>
    template <typename T>
    void
    load_internal(const T& filename, const T& password);

    /// <summary>
    /// Interprets file with the given filename as an XLSX file encrypted with the
    /// given password and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file does not exist at the specified path, or is empty/malformed, an
    /// xlnt::invalid_file exception will be thrown. If the file is valid but uses features not yet
    /// supported by XLNT which cannot be ignored/skipped, an xlnt::unsupported exception will be
    /// thrown.
    /// </summary>
    template <typename T>
    void
    load_internal(const xlnt::path& filename, const T& password);

    /// <summary>
    /// Interprets data in stream as an XLSX file encrypted with the given password
    /// and sets the content of this workbook to match that file.
    /// If the specified password is invalid, an xlnt::invalid_password will be thrown.
    /// If the file is empty/malformed, an xlnt::invalid_file exception will be thrown.
    /// If the file is valid but uses features not yet supported by XLNT which cannot be
    /// ignored/skipped, an xlnt::unsupported exception will be thrown.
    /// </summary>
    template <typename T>
    void
    load_internal(std::istream& stream, const T& password);

    /// <summary>
    /// Returns a reference to the workbook implementation structure. Provides
    /// a nicer interface than constantly dereferencing workbook::d_.
    /// </summary>
    detail::workbook_impl&
    impl();

    /// <summary>
    /// Returns a reference to the workbook implementation structure. Provides
    /// a nicer interface than constantly dereferencing workbook::d_.
    /// </summary>
    const detail::workbook_impl&
    impl() const;

    /// <summary>
    /// Adds a package-level part of the given type to the manifest if it doesn't
    /// already exist. The part will have a path and content type of the default
    /// for that particular relationship type.
    /// </summary>
    void
    register_package_part(relationship_type type);

    /// <summary>
    /// Adds a workbook-level part of the given type to the manifest if it doesn't
    /// already exist. The part will have a path and content type of the default
    /// for that particular relationship type. It will be a relationship target
    /// of this workbook.
    /// </summary>
    void
    register_workbook_part(relationship_type type);

    /// <summary>
    /// Adds a worksheet-level part of the given type to the manifest if it doesn't
    /// already exist. The part will have a path and content type of the default
    /// for that particular relationship type. It will be a relationship target
    /// of the given worksheet, ws.
    /// </summary>
    void
    register_worksheet_part(worksheet ws, relationship_type type);

    /// <summary>
    /// Removes calcChain part from manifest if no formulae remain in workbook.
    /// </summary>
    void
    garbage_collect_formulae();

    /// <summary>
    /// Update extended workbook properties titlesOfParts and headingPairs when sheets change.
    /// </summary>
    void
    update_sheet_properties();

    /// <summary>
    /// Swaps the data held in this workbook with workbook other.
    /// </summary>
    void
    swap(workbook& other);

    /// <summary>
    /// Sheet 1 should be rId1, sheet 2 should be rId2, etc.
    /// </summary>
    void
    reorder_relationships();

    /// <summary>
    /// Sets the default format
    /// </summary>
    void
    default_format(const xlnt::format& format);

    /// <summary>
    /// An opaque pointer to a structure that holds all of the data relating to this workbook.
    /// </summary>
    std::shared_ptr<detail::workbook_impl> d_;
};

} // namespace xlnt
