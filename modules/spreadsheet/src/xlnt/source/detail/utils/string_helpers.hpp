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
#include <vector>

#include <detail/xlnt_config_impl.hpp>

namespace xlnt {
namespace detail {

    /// <summary>
    /// Return a vector containing string split at each delim.
    /// If the input string is empty, an empty vector is returned.
    /// </summary>
    XLNT_API_INTERNAL std::vector<std::string>
    split_string(const std::string& string, char delim);

    /// <summary>
    /// Concatenate all the provided items by converting them to a string using its to_string member
    /// function.
    /// </summary>
    template <typename T>
    std::string
    join(const std::vector<T>& items, char delim)
    {
        std::string refs;
        for (const auto& item : items) {
            if (!refs.empty()) {
                refs.push_back(delim);
            }

            refs.append(item.to_string());
        }

        return refs;
    }

} // namespace detail
} // namespace xlnt
