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

#include <xlnt/utils/exceptions.hpp>

namespace xlnt {

exception::exception(const std::string& message)
    : std::runtime_error("xlnt::exception : " + message), message_(message)
{
}

void
exception::message(const std::string& message)
{
    message_ = message;
}

const std::string&
exception::message()
{
    return message_;
}

unhandled_switch_case::unhandled_switch_case(long long switch_value)
    : xlnt::exception("unhandled switch case " + std::to_string(switch_value))
{
}

unhandled_switch_case::unhandled_switch_case(const std::string& switch_value_string)
    : xlnt::exception("unhandled switch case " + switch_value_string)
{
}

unhandled_switch_case::unhandled_switch_case(const char* switch_value_optional_string)
    : xlnt::exception(switch_value_optional_string != nullptr
              ? ("unhandled switch case " + std::string(switch_value_optional_string))
              : "unhandled switch case")
{
}

invalid_sheet_title::invalid_sheet_title(const std::string& title)
    : exception("bad worksheet title: " + title)
{
}

invalid_column_index::invalid_column_index(column_t::index_t column_index)
    : exception("column at index " + std::to_string(column_index) + " does not exist")
{
}

invalid_column_index::invalid_column_index(column_t column)
    : exception("column at index " + std::to_string(column.index) + " does not exist")
{
}

invalid_column_index::invalid_column_index(const std::string& column_str)
    : exception("column string \"" + column_str + "\" is invalid")
{
}

invalid_data_type::invalid_data_type(const std::string& type)
    : exception("data type error for type \"" + type + "\"")
{
}

invalid_file::invalid_file(const std::string& reason)
    : exception("couldn't load file, reason given: " + reason)
{
}

invalid_cell_reference::invalid_cell_reference(column_t column, row_t row)
    : exception(std::string("bad cell coordinates: (") + std::to_string(column.index) + ", "
          + std::to_string(row) + ")")
{
}

invalid_cell_reference::invalid_cell_reference(const std::string& coord_string)
    : exception(std::string("bad cell coordinates: (")
          + (coord_string.empty() ? "<empty>" : coord_string) + ")")
{
}

illegal_character::illegal_character(char c)
    : exception(
          std::string("illegal character: (") + std::to_string(static_cast<unsigned char>(c)) + ")")
{
}

invalid_parameter::invalid_parameter(const std::string& message)
    : exception("invalid parameter: " + message)
{
}

invalid_attribute::invalid_attribute(const std::string& message)
    : exception("invalid attribute: " + message)
{
}

key_not_found::key_not_found(const std::string& key_name)
    : exception("key \"" + key_name + "\" not found in container")
{
}

no_visible_worksheets::no_visible_worksheets()
    : exception("workbook needs at least one non-hidden worksheet to be saved")
{
}

invalid_password::invalid_password(const std::string& message)
    : exception("invalid password: " + message)
{
}

unsupported::unsupported(const std::string& message) : exception("unsupported: " + message) { }

encoding_error::encoding_error(const std::string& message) : exception("encoding error: " + message)
{
}

bad_variant_access::bad_variant_access(variant::type expected_type, variant::type actual_type)
    : exception("bad variant access: expected type " + xlnt::variant::get_type_string(expected_type)
          + " but got type " + xlnt::variant::get_type_string(actual_type))
{
}

} // namespace xlnt
