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

#include <memory>
#include <string>

#include <xlnt/cell/cell_type.hpp>
#include <xlnt/cell/comment.hpp>
#include <xlnt/cell/index_types.hpp>
#include <xlnt/cell/rich_text.hpp>
#include <xlnt/packaging/relationship.hpp>
#include <xlnt/utils/numeric.hpp>
#include <xlnt/utils/optional.hpp>
#include <detail/implementations/format_impl.hpp>
#include <detail/implementations/hyperlink_impl.hpp>

namespace xlnt {
namespace detail {

    struct worksheet_impl;

    struct cell_impl
    {
        cell_type type_ = cell_type::empty;

        worksheet_impl* parent_ = nullptr;

        // According to the OOXML specification:
        // "In SpreadsheetML, cell references range from column A1–A1048576 (column A:A) to column
        // XFD1–XFD1048576 (column XFD:XFD). An implementation can extend this range."
        column_t column_
            = 1; // default range: ["A", "XFD"] -> [1, 16384], but XLNT allows [1, 4294967295]
        row_t row_ = 1; // default range: [1, 1048576], but XLNT allows [1, 4294967295]

        bool is_merged_ = false;
        bool phonetics_visible_ = false;

        rich_text value_text_;
        double value_numeric_ = 0.0;

        optional<std::string> formula_;
        optional<hyperlink_impl> hyperlink_;
        format_impl_ptr format_;
        optional<comment*> comment_;

        bool
        is_garbage_collectible() const
        {
            return !(type_ != cell_type::empty || is_merged_ || phonetics_visible_
                || formula_.is_set() || format_.is_set() || hyperlink_.is_set());
        }
    };

    inline bool
    operator==(const cell_impl& lhs, const cell_impl& rhs)
    {
        // not comparing parent, row, column
        return lhs.type_ == rhs.type_ && lhs.is_merged_ == rhs.is_merged_
            && lhs.phonetics_visible_ == rhs.phonetics_visible_
            && lhs.value_text_ == rhs.value_text_
            && float_equals(lhs.value_numeric_, rhs.value_numeric_) && lhs.formula_ == rhs.formula_
            && lhs.hyperlink_ == rhs.hyperlink_
            && (lhs.format_.is_set() == rhs.format_.is_set()
                && (!lhs.format_.is_set() || *lhs.format_.get() == *rhs.format_.get()))
            && (lhs.comment_.is_set() == rhs.comment_.is_set()
                && (!lhs.comment_.is_set() || *lhs.comment_.get() == *rhs.comment_.get()));
    }

    inline bool
    operator!=(const cell_impl& lhs, const cell_impl& rhs)
    {
        return !(lhs == rhs);
    }

} // namespace detail
} // namespace xlnt
