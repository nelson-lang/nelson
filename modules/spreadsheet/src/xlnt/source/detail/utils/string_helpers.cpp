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

#include "string_helpers.hpp"

namespace xlnt {
namespace detail {

    std::vector<std::string>
    split_string(const std::string& string, char delim)
    {
        std::vector<std::string> split;

        if (string.empty()) {
            return split;
        }

        std::string::size_type previous_index = 0;
        auto separator_index = string.find(delim);

        while (separator_index != std::string::npos) {
            auto part = string.substr(previous_index, separator_index - previous_index);
            split.push_back(part);

            previous_index = separator_index + 1;
            separator_index = string.find(delim, previous_index);
        }

        split.push_back(string.substr(previous_index));

        return split;
    }

} // namespace detail
} // namespace xlnt
