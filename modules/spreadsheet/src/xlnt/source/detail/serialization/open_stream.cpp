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

#include <xlnt/utils/path.hpp>
#include <detail/serialization/open_stream.hpp>

#if XLNT_HAS_FEATURE(FILESYSTEM)
#include <filesystem>
#endif

namespace xlnt {
namespace detail {

    void
    open_stream(std::ifstream& stream, const std::string& path)
    {
#ifdef _MSC_VER
        open_stream(stream, xlnt::path(path).wstring());
#else
        stream.open(path, std::ios::binary);
#endif
    }

    void
    open_stream(std::ofstream& stream, const std::string& path)
    {
#ifdef _MSC_VER
        open_stream(stream, xlnt::path(path).wstring());
#else
        stream.open(path, std::ios::binary);
#endif
    }

#ifdef _MSC_VER
    void
    open_stream(std::ifstream& stream, const std::wstring& path)
    {
        stream.open(path, std::ios::binary);
    }

    void
    open_stream(std::ofstream& stream, const std::wstring& path)
    {
        stream.open(path, std::ios::binary);
    }
#endif

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    void
    open_stream(std::ifstream& stream, std::u8string_view path)
    {
#ifdef _MSC_VER
        open_stream(stream, xlnt::path(path).wstring());
#elif XLNT_HAS_FEATURE(FILESYSTEM)
        stream.open(std::filesystem::path(path), std::ios::binary);
#else
        // TODO: this cannot work if the user's locale is not UTF-8. In such cases we cannot ensure
        // that this always works - however, in such cases we can still attempt to do a conversion
        // to the locale encoding, which will still work if the string can be represented with the
        // user's locale encoding. NOTE: this code will only run if C++17 is only partially
        // implemented, but C++17 string_view and C++20 char8_t are implemented, while C++17
        // filesystem is not.
        stream.open(to_char_ptr(path.data()), std::ios::binary);
#endif
    }

    void
    open_stream(std::ofstream& stream, std::u8string_view path)
    {
#ifdef _MSC_VER
        open_stream(stream, xlnt::path(path).wstring());
#elif XLNT_HAS_FEATURE(FILESYSTEM)
        stream.open(std::filesystem::path(path), std::ios::binary);
#else
        // TODO: this cannot work if the user's locale is not UTF-8. In such cases we cannot ensure
        // that this always works - however, in such cases we can still attempt to do a conversion
        // to the locale encoding, which will still work if the string can be represented with the
        // user's locale encoding. NOTE: this code will only run if C++17 is only partially
        // implemented, but C++17 string_view and C++20 char8_t are implemented, while C++17
        // filesystem is not.
        stream.open(to_char_ptr(path.data()), std::ios::binary);
#endif
    }
#endif

} // namespace detail
} // namespace xlnt
