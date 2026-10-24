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

#ifndef _MSC_VER
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"
#pragma clang diagnostic ignored "-Wweak-vtables"
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wsuggest-override"
#pragma clang diagnostic ignored "-Wswitch-default"
#include <utf8.h>
#pragma clang diagnostic pop
#else
#include <utf8.h>
#endif

#include <xlnt/utils/exceptions.hpp>
#include <detail/unicode.hpp>

namespace xlnt {
namespace detail {

    std::u16string
    utf8_to_utf16(const std::string& utf8_string)
    {
        std::u16string result;
        utf8::utf8to16(utf8_string.begin(), utf8_string.end(), std::back_inserter(result));
        return result;
    }

    std::u32string
    utf8_to_utf32(const std::string& utf8_string)
    {
        std::u32string result;
        utf8::utf8to32(utf8_string.begin(), utf8_string.end(), std::back_inserter(result));
        return result;
    }

    std::string
    utf16_to_utf8(const std::u16string& utf16_string)
    {
        std::string result;
        utf8::utf16to8(utf16_string.begin(), utf16_string.end(), std::back_inserter(result));
        return result;
    }

    std::string
    utf32_to_utf8(const std::u32string& utf32_string)
    {
        std::string result;
        utf8::utf32to8(utf32_string.begin(), utf32_string.end(), std::back_inserter(result));
        return result;
    }

    std::string
    latin1_to_utf8(const std::string& latin1)
    {
        std::string utf8;

        for (auto character : latin1) {
            if (character >= 0) {
                utf8.push_back(character);
            } else {
                utf8.push_back(static_cast<char>(0xc0 + (character >> 6)));
                utf8.push_back(static_cast<char>(0x80 + (character & 0x3f)));
            }
        }

        return utf8;
    }

    size_t
    string_length(const std::string& utf8_string)
    {
        auto end_it = utf8::find_invalid(utf8_string.begin(), utf8_string.end());
        if (end_it != utf8_string.end()) {
            throw xlnt::encoding_error(
                "Invalid UTF-8 encoding detected for string \"" + utf8_string + "\"");
        }

        return static_cast<std::size_t>(utf8::distance(utf8_string.begin(), end_it));
    }

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    std::u16string
    utf8_to_utf16(std::u8string_view utf8_string)
    {
        std::u16string result;
        utf8::utf8to16(utf8_string.begin(), utf8_string.end(), std::back_inserter(result));
        return result;
    }

    std::u32string
    utf8_to_utf32(std::u8string_view utf8_string)
    {
        std::u32string result;
        utf8::utf8to32(utf8_string.begin(), utf8_string.end(), std::back_inserter(result));
        return result;
    }

    std::u8string
    utf16_to_utf8_u8(std::u16string_view utf16_string)
    {
        std::u8string result;
        utf8::utf16to8(utf16_string.begin(), utf16_string.end(), std::back_inserter(result));
        return result;
    }

    std::u8string
    utf32_to_utf8_u8(std::u32string_view utf32_string)
    {
        std::u8string result;
        utf8::utf32to8(utf32_string.begin(), utf32_string.end(), std::back_inserter(result));
        return result;
    }
#endif

} // namespace detail
} // namespace xlnt
