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

#include <xlnt/internal/features.hpp>
#include <detail/xlnt_config_impl.hpp>

#if XLNT_HAS_INCLUDE(<string_view>) && XLNT_HAS_FEATURE(U8_STRING_VIEW)
#include <string_view>
#endif

namespace xlnt {
namespace detail {

    XLNT_API_INTERNAL std::u16string
    utf8_to_utf16(const std::string& utf8_string);
    XLNT_API_INTERNAL std::u32string
    utf8_to_utf32(const std::string& utf8_string);
    XLNT_API_INTERNAL std::string
    utf16_to_utf8(const std::u16string& utf16_string);
    XLNT_API_INTERNAL std::string
    utf32_to_utf8(const std::u32string& utf32_string);
    XLNT_API_INTERNAL std::string
    latin1_to_utf8(const std::string& latin1);
    XLNT_API_INTERNAL size_t
    string_length(const std::string& utf8_string);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    XLNT_API_INTERNAL std::u16string
    utf8_to_utf16(std::u8string_view utf8_string);
    XLNT_API_INTERNAL std::u32string
    utf8_to_utf32(std::u8string_view utf8_string);
    XLNT_API_INTERNAL std::u8string
    utf16_to_utf8_u8(std::u16string_view utf16_string);
    XLNT_API_INTERNAL std::u8string
    utf32_to_utf8_u8(std::u32string_view utf32_string);
#endif

} // namespace detail
} // namespace xlnt
