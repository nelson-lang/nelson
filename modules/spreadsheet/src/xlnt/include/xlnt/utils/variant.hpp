// Copyright (c) 2017-2022 Thomas Fussell
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
#include <vector>

#include <xlnt/xlnt_config.hpp>

namespace xlnt {

struct datetime;

/// <summary>
/// Represents an object that can have variable type.
/// </summary>
class XLNT_API variant
{
public:
    // TODO: implement remaining types?

    /// <summary>
    /// The possible types a variant can hold.
    /// </summary>
    enum class type
    {
        // variant = 0, // this variant type
        vector = 1,
        // array = 2,
        // blob = 3, // XSD type xsd:base64Binary
        // oblob = 4, // XSD type xsd:base64Binary
        // empty = 5,
        null = 6,
        // i1 = 7, // XSD type xsd:byte (8-bit signed integer)
        // i2 = 8, // XSD type xsd:short (16-bit signed integer)
        i4 = 9, // XSD type xsd:int (32-bit signed integer)
                // i8 = 10, // XSD type xsd:long (64-bit signed integer)
                // integer = 11, // XSD type xsd:int (32-bit signed integer)
                // ui1 = 12, // XSD type xsd:unsignedByte (8-bit unsigned integer)
                // ui2 = 13, // XSD type xsd:unsignedShort (16-bit unsigned integer)
                // ui4 = 14, // XSD type xsd:unsignedInt (32-bit unsigned integer)
                // ui8 = 15, // XSD type xsd:unsignedLong (64-bit unsigned integer)
                // uint = 16, // XSD type xsd:unsignedInt (32-bit unsigned integer)
                // r4 = 17, // XSD type xsd:float (32-bit floating point number)
                // r8 = 18, // XSD type xsd:double (64-bit floating point number)
                // decimal = 19, XSD type xsd:decimal
        lpstr
        = 20, // XSD type xsd:string. OOXML says: "This element specifies a string variant type."
              // lpwstr = 21, // XSD type xsd:string. OOXML says: "This element specifies a string
              // variant type." bstr = 22, // XSD type xsd:string. OOXML says: "This element defines
              // a binary basic string variant type, which can store any valid Unicode character."
        date = 23, // XSD type xsd:dateTime
                   // filetime = 24, // XSD type xsd:dateTime
        boolean = 25, // XSD type xsd:boolean (any of: 'true', 'false', '1', '0')
                      // cy = 26,
                      // error = 27,
                      // stream = 28, // XSD type xsd:base64Binary
                      // ostream = 29, // XSD type xsd:base64Binary
                      // storage = 30, // XSD type xsd:base64Binary
                      // ostorage = 31, // XSD type xsd:base64Binary
                      // vstream = 32,
                      // clsid = 33
    };

    /// <summary>
    /// Returns a displayable string for the specified type.
    /// </summary>
    static std::string
    get_type_string(type type);

    /// <summary>
    /// Default constructor. Creates a null-type variant.
    /// </summary>
    variant();

    /// <summary>
    /// Creates a string-type variant with the given value.
    /// </summary>
    variant(const std::string& value);

    /// <summary>
    /// Creates a string-type variant with the given value.
    /// </summary>
    variant(const char* value);

    /// <summary>
    /// Creates a i4-type variant with the given value.
    /// </summary>
    variant(std::int32_t value);

    /// <summary>
    /// Creates a bool-type variant with the given value.
    /// </summary>
    variant(bool value);

    /// <summary>
    /// Creates a datetime-type variant with the given value.
    /// </summary>
    variant(const datetime& value);

    /// <summary>
    /// Creates a vector_i4-type variant with the given value.
    /// </summary>
    variant(const std::initializer_list<std::int32_t>& value);

    /// <summary>
    /// Creates a vector_i4-type variant with the given value.
    /// </summary>
    variant(const std::vector<std::int32_t>& value);

    /// <summary>
    /// Creates a vector_string-type variant with the given value.
    /// </summary>
    variant(const std::initializer_list<const char*>& value);

    /// <summary>
    /// Creates a vector_string-type variant with the given value.
    /// </summary>
    variant(const std::vector<const char*>& value);

    /// <summary>
    /// Creates a vector_string-type variant with the given value.
    /// </summary>
    variant(const std::initializer_list<std::string>& value);

    /// <summary>
    /// Creates a vector_string-type variant with the given value.
    /// </summary>
    variant(const std::vector<std::string>& value);

    /// <summary>
    /// Creates a vector_bool-type variant with the given value.
    /// </summary>
    variant(const std::initializer_list<bool>& value);

    /// <summary>
    /// Creates a vector_bool-type variant with the given value.
    /// </summary>
    variant(const std::vector<bool>& value);

    /// <summary>
    /// Creates a vector_datetime-type variant with the given value.
    /// </summary>
    variant(const std::initializer_list<datetime>& value);

    /// <summary>
    /// Creates a vector_datetime-type variant with the given value.
    /// </summary>
    variant(const std::vector<datetime>& value);

    /// <summary>
    /// Creates a vector_variant-type variant with the given value.
    /// </summary>
    variant(const std::initializer_list<variant>& value);

    /// <summary>
    /// Creates a vector_variant-type variant with the given value.
    /// </summary>
    variant(const std::vector<variant>& value);

    /// <summary>
    /// Returns true if this variant is of type t.
    /// </summary>
    bool
    is(type t) const;

    /// <summary>
    /// Returns the value of this variant as type T.
    /// Assumes that the variant is of type T (please call is() or value_type() to check). For
    /// details on each type, please see the documentation for each get<T> function below. An
    /// xlnt::bad_variant_access exception will be thrown if the types are not convertible.
    /// </summary>
    template <typename T>
    T
    get() const;

    /// <summary>
    /// Returns the type of this variant.
    /// </summary>
    type
    value_type() const;

    bool
    operator==(const variant& rhs) const;

    bool
    operator!=(const variant& rhs) const;

private:
    template <typename T>
    void
    construct_vector_internal(const T& vec);

    template <typename T>
    std::vector<T>
    get_vector_internal() const;

    type type_;
    std::vector<variant> vector_value_;
    std::int32_t i4_value_;
    std::string lpstr_value_;
};

/// <summary>
/// Returns the value of this variant as type bool.
/// Assumes that the variant is of type::boolean (please call is() or value_type() to check).
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API bool
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::int32_t.
/// Assumes that the variant is of type::i4 (please call is() or value_type() to check).
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API std::int32_t
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::string.
/// Please call is() or value_type() to check the type. The following types are compatible:
/// - type::lpstr
/// - type::date (DEPRECATED, will no longer be allowed in a future major version)
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API std::string
variant::get() const;

/// <summary>
/// Returns the value of this variant as type xlnt::datetime.
/// Assumes that the variant is of type::date (please call is() or value_type() to check).
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API datetime
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::vector<xlnt::variant>.
/// Please call is() or value_type() to check the type. The following types are compatible:
/// - type::vector containing elements of type::variant
/// - type::vector containing any other elements (DEPRECATED, will no longer be allowed in a future
/// major version) An xlnt::bad_variant_access exception will be thrown if the types are not
/// compatible.
/// </summary>
template <>
XLNT_API std::vector<variant>
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::vector<bool>.
/// Assumes that the variant is of type::vector (please call is() or value_type() to check)
/// containing elements of type::boolean.
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API std::vector<bool>
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::vector<std::int32_t>.
/// Assumes that the variant is of type::vector (please call is() or value_type() to check)
/// containing elements of type::i4.
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API std::vector<std::int32_t>
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::vector<std::string>.
/// Assumes that the variant is of type::vector (please call is() or value_type() to check)
/// containing elements of type:
/// - type::lpstr
/// - type::date (DEPRECATED, will no longer be allowed in a future major version)
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API std::vector<std::string>
variant::get() const;

/// <summary>
/// Returns the value of this variant as type std::vector<xlnt::datetime>.
/// Assumes that the variant is of type::vector (please call is() or value_type() to check)
/// containing elements of type::date.
/// An xlnt::bad_variant_access exception will be thrown if the types are not compatible.
/// </summary>
template <>
XLNT_API std::vector<datetime>
variant::get() const;

} // namespace xlnt
