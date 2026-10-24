// Copyright (c) 2026 xlnt-community
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

#include <xlnt/internal/features.hpp>

#if XLNT_HAS_INCLUDE(<string_view>) && XLNT_HAS_FEATURE(U8_STRING_VIEW)
#include <string_view>
#endif

namespace xlnt {

// Replacement for u8 from C++ that always returns const char *,
// like u8 did in C++11, C++14 and C++17 - but XLNT_U8 also does this with C++20 and newer.
#define XLNT_U8(a) xlnt::to_char_ptr(XLNT_DETAIL_U8STRING_LITERAL(a))

// Casts a UTF-8 C string to a narrow C string without changing its encoding or performing any
// conversions, and without copying the string. It performs the required casts depending on the C++
// version. It is mainly meant for C++20 or newer to provide compatibility with std::string(_view).
// However, a fallback function is available for code that needs to compile with both C++20 (or
// newer) and C++17 (or older), without any code changes.
#ifdef __cpp_char8_t
/// Casts a const char8_t array to a const char array
/// without changing its encoding or performing any conversions,
/// and without copying the string.
inline const char*
to_char_ptr(const char8_t* utf8)
{
    return reinterpret_cast<const char*>(utf8);
}
#else
/// For C++11, C++14 and C++17, simply return the C string as it is.
inline const char*
to_char_ptr(const char* utf8)
{
    return utf8;
}
#endif

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
/// Casts std::u8string(_view) to std::string_view
/// without changing its encoding or performing any conversions,
/// and without copying the string.
inline std::string_view
to_string_view(std::u8string_view utf8)
{
    return std::string_view { to_char_ptr(utf8.data()), utf8.length() };
}

/// Copies std::u8string(_view) to std::string
/// without changing its encoding or performing any conversions.
/// A full copy of the string is made.
inline std::string
to_string(std::u8string_view utf8)
{
    return std::string { utf8.begin(), utf8.end() };
}
#endif

/// ----- INTERNAL MACROS AND FUNCTIONS -----

// Prepends the string literal prefix to the provided string literal.
// Useful when defining a string literal once, then using it with multiple string types.
#define XLNT_DETAIL_LSTRING_LITERAL2(a) L##a
#define XLNT_DETAIL_U8STRING_LITERAL2(a) u8##a
#define XLNT_DETAIL_U16STRING_LITERAL2(a) u##a
#define XLNT_DETAIL_U32STRING_LITERAL2(a) U##a
#define XLNT_DETAIL_LSTRING_LITERAL(a) XLNT_DETAIL_LSTRING_LITERAL2(a)
#define XLNT_DETAIL_U8STRING_LITERAL(a) XLNT_DETAIL_U8STRING_LITERAL2(a)
#define XLNT_DETAIL_U16STRING_LITERAL(a) XLNT_DETAIL_U16STRING_LITERAL2(a)
#define XLNT_DETAIL_U32STRING_LITERAL(a) XLNT_DETAIL_U32STRING_LITERAL2(a)

} // namespace xlnt
