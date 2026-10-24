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

#include <fstream>
#include <string>

#include <xlnt/internal/features.hpp>

#if XLNT_HAS_INCLUDE(<string_view>) && XLNT_HAS_FEATURE(U8_STRING_VIEW)
#include <string_view>
#endif

namespace xlnt {
namespace detail {

    void
    open_stream(std::ifstream& stream, const std::string& path);

    void
    open_stream(std::ofstream& stream, const std::string& path);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    void
    open_stream(std::ifstream& stream, std::u8string_view path);

    void
    open_stream(std::ofstream& stream, std::u8string_view path);
#endif

#ifdef _MSC_VER
    void
    open_stream(std::ifstream& stream, const std::wstring& path);

    void
    open_stream(std::ofstream& stream, const std::wstring& path);
#endif

} // namespace detail
} // namespace xlnt
