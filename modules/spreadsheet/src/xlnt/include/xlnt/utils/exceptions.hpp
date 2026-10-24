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

#include <stdexcept>

#include <xlnt/xlnt_config.hpp>
#include <xlnt/cell/index_types.hpp>
#include <xlnt/utils/variant.hpp>

namespace xlnt {

/// <summary>
/// Parent type of all custom exceptions thrown in this library.
/// </summary>
class XLNT_API exception : public std::runtime_error
{
public:
    /// <summary>
    /// Constructs an exception with a message. This message will be
    /// returned by std::exception::what(), an inherited member of this class.
    /// </summary>
    explicit exception(const std::string& message);

    /// <summary>
    /// Sets the message after the xlnt::exception is constructed. This can show
    /// more specific information than std::exception::what().
    /// </summary>
    void
    message(const std::string& message);

    /// <summary>
    /// Gets the message containing extra information.
    /// </summary>
    const std::string&
    message();

private:
    /// <summary>
    /// The exception message
    /// </summary>
    std::string message_;
};

/// <summary>
/// Exception for a bad parameter value
/// </summary>
class XLNT_API invalid_parameter : public exception
{
public:
    /// <summary>
    /// Constructor with a message.
    /// </summary>
    explicit invalid_parameter(const std::string& message);
};

/// <summary>
/// Exception for bad sheet names.
/// </summary>
class XLNT_API invalid_sheet_title : public exception
{
public:
    /// <summary>
    /// Default constructor.
    /// </summary>
    explicit invalid_sheet_title(const std::string& title);
};

/// <summary>
/// Exception for trying to open a non-XLSX file.
/// </summary>
class XLNT_API invalid_file : public exception
{
public:
    /// <summary>
    /// Constructs an invalid_file exception thrown when attempt to access
    /// the given file, containing a description of the reason.
    /// </summary>
    explicit invalid_file(const std::string& reason);
};

/// <summary>
/// The data submitted which cannot be used directly in Excel files. It
/// must be removed or escaped.
/// </summary>
class XLNT_API illegal_character : public exception
{
public:
    /// <summary>
    /// Constructs an illegal_character exception thrown as a result of the given character.
    /// </summary>
    explicit illegal_character(char c);
};

/// <summary>
/// Exception for any data type inconsistencies.
/// </summary>
class XLNT_API invalid_data_type : public exception
{
public:
    /// <summary>
    /// Constructor with a data type name.
    /// </summary>
    invalid_data_type(const std::string& type);
};

/// <summary>
/// Exception for bad column indices in A1-style cell references.
/// </summary>
class XLNT_API invalid_column_index : public exception
{
public:
    /// <summary>
    /// Constructor with a column number.
    /// </summary>
    explicit invalid_column_index(column_t::index_t column_index);

    /// <summary>
    /// Constructor with a column.
    /// </summary>
    explicit invalid_column_index(column_t column);

    /// <summary>
    /// Constructor with a column string.
    /// </summary>
    explicit invalid_column_index(const std::string& column_str);
};

/// <summary>
/// Exception for converting between numeric and A1-style cell references.
/// </summary>
class XLNT_API invalid_cell_reference : public exception
{
public:
    /// <summary>
    /// Constructs an invalid_cell_reference exception for the given column and row.
    /// </summary>
    invalid_cell_reference(column_t column, row_t row);

    /// <summary>
    /// Constructs an invalid_cell_reference exception for the given string.
    /// </summary>
    explicit invalid_cell_reference(const std::string& reference_string);
};

/// <summary>
/// Exception when getting a class's attribute before being set/initialized,
/// or when setting a class's attribute to an invalid value.
/// </summary>
class XLNT_API invalid_attribute : public exception
{
public:
    /// <summary>
    /// Constructor with a message.
    /// </summary>
    explicit invalid_attribute(const std::string& message);
};

/// <summary>
/// Exception for a key that doesn't exist in a container
/// </summary>
class XLNT_API key_not_found : public exception
{
public:
    /// <summary>
    /// Constructor with a key name.
    /// </summary>
    explicit key_not_found(const std::string& key_name);
};

/// <summary>
/// Exception for a workbook with no visible worksheets
/// </summary>
class XLNT_API no_visible_worksheets : public exception
{
public:
    /// <summary>
    /// Default constructor.
    /// </summary>
    no_visible_worksheets();
};

/// <summary>
/// Debug exception for a switch that fell through to the default case
/// </summary>
class XLNT_API unhandled_switch_case : public exception
{
public:
    /// <summary>
    /// Constructor taking a switch value. Useful for:
    /// - integer types
    /// - enums (but rather for handling default switch cases; otherwise for handling single cases
    /// use the string overloads below)
    /// </summary>
    explicit unhandled_switch_case(long long switch_value);

    /// <summary>
    /// Constructor taking a switch value as a string.
    /// </summary>
    explicit unhandled_switch_case(const std::string& switch_value_string);

    /// <summary>
    /// Constructor taking a switch value as an optional string.
    /// This constructor with an optional message is unfortunately necessary to avoid complexity in
    /// default_case. If possible, please always provide a switch value (either as a value or as a
    /// string) instead of leaving it empty!
    /// </summary>
    explicit unhandled_switch_case(const char* switch_value_optional_string = nullptr);
};

/// <summary>
/// Exception for invalid (empty, incorrect) passwords
/// </summary>
class XLNT_API invalid_password : public exception
{
public:
    /// <summary>
    /// Constructor taking a string explaining why the password is invalid.
    /// </summary>
    explicit invalid_password(const std::string& message);
};

/// <summary>
/// Exception for attempting to use a feature which is not supported
/// </summary>
class XLNT_API unsupported : public exception
{
public:
    /// <summary>
    /// Constructs an unsupported exception with a message describing the unsupported
    /// feature.
    /// </summary>
    explicit unsupported(const std::string& message);
};

/// <summary>
/// Exception for encoding errors
/// </summary>
class XLNT_API encoding_error : public exception
{
public:
    /// <summary>
    /// Constructs an encoding_error exception with a message describing the encoding error.
    /// </summary>
    explicit encoding_error(const std::string& message);
};

/// <summary>
/// Exception for xlnt::variant access
/// </summary>
class XLNT_API bad_variant_access : public exception
{
public:
    /// <summary>
    /// Constructs a bad_variant_access exception given the mismatched types.
    /// </summary>
    explicit bad_variant_access(variant::type expected_type, variant::type actual_type);
};

} // namespace xlnt
