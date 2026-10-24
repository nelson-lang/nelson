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

#include <cstdint>
#include <string>

#include <xlnt/xlnt_config.hpp>

// We might want to change these types for various optimizations in the future
// so use typedefs.

namespace xlnt {

/// <summary>
/// All rows should be referred to by an instance of this type.
/// By default, row references range from 1 to 1048576, but the OOXML specification allows to extend
/// this range, and XLNT allows rows between 1 and 4294967295. Please note that not all applications
/// might support these extended ranges.
/// </summary>
using row_t = std::uint32_t;

/// <summary>
/// Columns can be referred to as a string A,B,...Z,AA,AB,..,ZZ,AAA,...,XFD
/// or as a 1-indexed index (indices 1 to 16384). This class encapsulates both of these forms of
/// column referencing and allows for conversions between them.
/// By default, column references range from A to XFD (column indices 1 to 16384), but
/// the OOXML specification allows to extend this range, and XLNT allows columns between 1 and
/// 4294967295. Please note that not all applications might support these extended ranges.
/// </summary>
class XLNT_API column_t
{
public:
    /// <summary>
    /// Alias declaration for the internal numeric type of this column.
    /// </summary>
    using index_t = std::uint32_t;

    /// <summary>
    /// Convert a column letter into a column number (e.g. B -> 2)
    /// </summary>
    /// <remarks>
    /// Excel only supports 1 - 3 letter column names from A->Z, AA->ZZ, AAA->XFD,
    /// so XLNT restricts column names to 1 - 3 characters, each in the range A - Z (columns A to
    /// ZZZ). Strings outside this range and malformed strings will throw an invalid_column_index
    /// exception.
    /// </remarks>
    static index_t
    column_index_from_string(const std::string& column_string);

    /// <summary>
    /// Convert a column number into a column letter (3 -> 'C')
    /// </summary>
    /// <remarks>
    /// Right shift the column, column_index, by 26 to find column letters in reverse
    /// order. These indices are 1-based, and can be converted to ASCII
    /// ordinals by adding 64.
    /// </remarks>
    static std::string
    column_string_from_index(index_t column_index);

    /// <summary>
    /// Default constructor. The column points to the "A" column.
    /// </summary>
    column_t();

    /// <summary>
    /// Constructs a column from a number.
    /// </summary>
    column_t(index_t column_index);

    /// <summary>
    /// Constructs a column from a string.
    /// Excel only supports 1 - 3 letter column names from A->Z, AA->ZZ, AAA->XFD,
    /// so XLNT restricts column names to 1 - 3 characters, each in the range A - Z (columns A to
    /// ZZZ). Strings outside this range and malformed strings will throw an invalid_column_index
    /// exception.
    /// </summary>
    column_t(const std::string& column_string);

    /// <summary>
    /// Constructs a column from a string.
    /// Excel only supports 1 - 3 letter column names from A->Z, AA->ZZ, AAA->XFD,
    /// so XLNT restricts column names to 1 - 3 characters, each in the range A - Z (columns A to
    /// ZZZ). Strings outside this range and malformed strings will throw an invalid_column_index
    /// exception.
    /// </summary>
    column_t(const char* column_string);

    /// <summary>
    /// Returns a string representation of this column index.
    /// </summary>
    std::string
    column_string() const;

    /// <summary>
    /// Sets this column to be equal to rhs and return reference to self.
    /// </summary>
    column_t&
    operator=(const std::string& rhs);

    /// <summary>
    /// Sets this column to be equal to rhs and return reference to self.
    /// </summary>
    column_t&
    operator=(const char* rhs);

    /// <summary>
    /// Returns true if this column refers to the same column as other.
    /// </summary>
    bool
    operator==(const column_t& other) const;

    /// <summary>
    /// Returns true if this column doesn't refer to the same column as other.
    /// </summary>
    bool
    operator!=(const column_t& other) const;

    /// <summary>
    /// Returns true if this column refers to the same column as other.
    /// </summary>
    bool
    operator==(int other) const;

    /// <summary>
    /// Returns true if this column refers to the same column as other.
    /// </summary>
    bool
    operator==(index_t other) const;

    /// <summary>
    /// Returns true if this column refers to the same column as other.
    /// </summary>
    bool
    operator==(const std::string& other) const;

    /// <summary>
    /// Returns true if this column refers to the same column as other.
    /// </summary>
    bool
    operator==(const char* other) const;

    /// <summary>
    /// Returns true if this column doesn't refer to the same column as other.
    /// </summary>
    bool
    operator!=(int other) const;

    /// <summary>
    /// Returns true if this column doesn't refer to the same column as other.
    /// </summary>
    bool
    operator!=(index_t other) const;

    /// <summary>
    /// Returns true if this column doesn't refer to the same column as other.
    /// </summary>
    bool
    operator!=(const std::string& other) const;

    /// <summary>
    /// Returns true if this column doesn't refer to the same column as other.
    /// </summary>
    bool
    operator!=(const char* other) const;

    /// <summary>
    /// Returns true if other is to the right of this column.
    /// </summary>
    bool
    operator>(const column_t& other) const;

    /// <summary>
    /// Returns true if other is to the right of or equal to this column.
    /// </summary>
    bool
    operator>=(const column_t& other) const;

    /// <summary>
    /// Returns true if other is to the left of this column.
    /// </summary>
    bool
    operator<(const column_t& other) const;

    /// <summary>
    /// Returns true if other is to the left of or equal to this column.
    /// </summary>
    bool
    operator<=(const column_t& other) const;

    /// <summary>
    /// Returns true if other is to the right of this column.
    /// </summary>
    bool
    operator>(const column_t::index_t& other) const;

    /// <summary>
    /// Returns true if other is to the right of or equal to this column.
    /// </summary>
    bool
    operator>=(const column_t::index_t& other) const;

    /// <summary>
    /// Returns true if other is to the left of this column.
    /// </summary>
    bool
    operator<(const column_t::index_t& other) const;

    /// <summary>
    /// Returns true if other is to the left of or equal to this column.
    /// </summary>
    bool
    operator<=(const column_t::index_t& other) const;

    /// <summary>
    /// Pre-increments this column, making it point to the column one to the right and returning a
    /// reference to it.
    /// </summary>
    column_t&
    operator++();

    /// <summary>
    /// Pre-deccrements this column, making it point to the column one to the left and returning a
    /// reference to it.
    /// </summary>
    column_t&
    operator--();

    /// <summary>
    /// Post-increments this column, making it point to the column one to the right and returning
    /// the old column.
    /// </summary>
    column_t
    operator++(int);

    /// <summary>
    /// Post-decrements this column, making it point to the column one to the left and returning the
    /// old column.
    /// </summary>
    column_t
    operator--(int);

    /// <summary>
    /// Returns the result of adding rhs to this column.
    /// </summary>
    friend XLNT_API column_t
    operator+(column_t lhs, const column_t& rhs);

    /// <summary>
    /// Returns the result of subtracing lhs by rhs column.
    /// </summary>
    friend XLNT_API column_t
    operator-(column_t lhs, const column_t& rhs);

    /// <summary>
    /// Adds rhs to this column and returns a reference to this column.
    /// </summary>
    column_t&
    operator+=(const column_t& rhs);

    /// <summary>
    /// Subtracts rhs from this column and returns a reference to this column.
    /// </summary>
    column_t&
    operator-=(const column_t& rhs);

    /// <summary>
    /// Returns true if other is to the right of this column.
    /// </summary>
    friend XLNT_API bool
    operator>(const column_t::index_t& left, const column_t& right);

    /// <summary>
    /// Returns true if other is to the right of or equal to this column.
    /// </summary>
    friend XLNT_API bool
    operator>=(const column_t::index_t& left, const column_t& right);

    /// <summary>
    /// Returns true if other is to the left of this column.
    /// </summary>
    friend XLNT_API bool
    operator<(const column_t::index_t& left, const column_t& right);

    /// <summary>
    /// Returns true if other is to the left of or equal to this column.
    /// </summary>
    friend XLNT_API bool
    operator<=(const column_t::index_t& left, const column_t& right);

    /// <summary>
    /// Swaps the columns that left and right refer to.
    /// </summary>
    friend XLNT_API void
    swap(column_t& left, column_t& right);

    /// <summary>
    /// Internal numeric value of this column index.
    /// </summary>
    index_t index;
};

enum class row_or_col_t : int
{
    row,
    column
};

/// <summary>
/// Functor for hashing a column.
/// Allows for use of std::unordered_set<column_t, column_hash> and similar.
/// </summary>
struct XLNT_API column_hash
{
    /// <summary>
    /// Returns the result of hashing column k.
    /// </summary>
    std::size_t
    operator()(const column_t& k) const;
};

} // namespace xlnt

namespace std {

/// <summary>
/// Template specialization to allow xlnt::column_t to be used as a key in a std container.
/// </summary>
template <> struct hash<xlnt::column_t>
{
    /// <summary>
    /// Returns the result of hashing column k.
    /// </summary>
    size_t
    operator()(const xlnt::column_t& k) const
    {
        static xlnt::column_hash hasher;
        return hasher(k);
    }
};

} // namespace std
