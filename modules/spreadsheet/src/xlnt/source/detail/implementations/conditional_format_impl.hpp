#pragma once

#include <cstddef>

#include <xlnt/styles/conditional_format.hpp>
#include <xlnt/utils/optional.hpp>
#include <xlnt/worksheet/range_reference.hpp>

namespace xlnt {

class border;
class fill;
class font;

namespace detail {

    struct stylesheet;
    struct worksheet_impl;

    struct conditional_format_impl
    {
        stylesheet* parent = nullptr;
        worksheet_impl* target_sheet = nullptr;

        bool
        operator==(const conditional_format_impl& rhs) const
        {
            // not comparing parent or target sheet
            return target_range == rhs.target_range && priority == rhs.priority
                && differential_format_id == rhs.differential_format_id && when == rhs.when
                && border_id == rhs.border_id && fill_id == rhs.fill_id && font_id == rhs.font_id;
        }

        bool
        operator!=(const conditional_format_impl& rhs) const
        {
            return !(*this == rhs);
        }

        range_reference target_range;

        std::size_t priority = 0;
        std::size_t differential_format_id = 0;

        condition when;

        optional<std::size_t> border_id;
        optional<std::size_t> fill_id;
        optional<std::size_t> font_id;
    };

} // namespace detail
} // namespace xlnt
