#pragma once

#include <cstddef>
#include <memory>

#include <detail/xlnt_config_impl.hpp>

#include <xlnt/internal/format_impl_ptr.hpp>
#include <xlnt/styles/alignment.hpp>
#include <xlnt/styles/border.hpp>
#include <xlnt/styles/fill.hpp>
#include <xlnt/styles/font.hpp>
#include <xlnt/styles/number_format.hpp>
#include <xlnt/styles/protection.hpp>
#include <xlnt/utils/optional.hpp>

namespace xlnt {

class alignment;
class border;
class fill;
class font;
class number_format;
class protection;

namespace detail {

    struct stylesheet;

    class references
    {
    public:
        references() = default;
        references(const references& /*reference*/) { }
        references(references&& /*reference*/) { }
        ~references() = default;

        operator std::size_t() const { return count; }

        references&
        operator++()
        {
            ++count;
            return *this;
        }
        references&
        operator--()
        {
            --count;
            return *this;
        }

        references&
        operator=(const references& /*reference*/)
        {
            return *this;
        }
        references&
        operator=(references&& /*reference*/)
        {
            return *this;
        }

    private:
        std::size_t count = 0;
    };

    struct format_impl
    {
        stylesheet* parent = nullptr;

        std::size_t id = 0;

        optional<std::size_t> alignment_id;
        optional<std::size_t> border_id;
        optional<std::size_t> fill_id;
        optional<std::size_t> font_id;
        optional<std::size_t> number_format_id;
        optional<std::size_t> protection_id;

        optional<bool> alignment_applied;
        optional<bool> border_applied;
        optional<bool> fill_applied;
        optional<bool> font_applied;
        optional<bool> number_format_applied;
        optional<bool> protection_applied;

        bool pivot_button_ = false;
        bool quote_prefix_ = false;

        optional<std::string> style;

        friend bool
        operator==(const format_impl& left, const format_impl& right)
        {
            // not comparing parent
            return left.alignment_id == right.alignment_id
                && left.alignment_applied == right.alignment_applied
                && left.border_id == right.border_id && left.border_applied == right.border_applied
                && left.fill_id == right.fill_id && left.fill_applied == right.fill_applied
                && left.font_id == right.font_id && left.font_applied == right.font_applied
                && left.number_format_id == right.number_format_id
                && left.number_format_applied == right.number_format_applied
                && left.protection_id == right.protection_id
                && left.protection_applied == right.protection_applied
                && left.pivot_button_ == right.pivot_button_
                && left.quote_prefix_ == right.quote_prefix_ && left.style == right.style;
        }

        bool
        is_used() const
        {
            return references > 0;
        }
        bool
        is_shared() const
        {
            return references > 1;
        }

    private:
        class references references;
        friend class format_impl_ptr;
    };

    class format_impl_list_item
    {
    public:
        format_impl_list_item() : format_(new format_impl()) { }
        format_impl_list_item(const format_impl& R) : format_(new format_impl(R)) { }
        format_impl_list_item(const format_impl_list_item& R)
            : format_impl_list_item(*R.format_) { }
        format_impl_list_item(format_impl_list_item&& R) = default;
        ~format_impl_list_item() = default;

        format_impl_list_item&
        operator=(const format_impl& R)
        {
            format_.reset(new format_impl(R));
            return *this;
        }
        format_impl_list_item&
        operator=(const format_impl_list_item& R)
        {
            return operator=(*R.format_);
        }
        format_impl_list_item&
        operator=(format_impl_list_item&& R)
            = default;

        format_impl*
        operator->()
        {
            return format_.get();
        }
        format_impl&
        operator*()
        {
            return *format_;
        }
        const format_impl&
        operator*() const
        {
            return *format_;
        }
        operator format_impl_ptr() { return format_.get(); }

        bool
        operator==(const format_impl_list_item& R) const
        {
            return *format_ == *R.format_;
        }

    protected:
        struct D
        {
            void
            operator()(format_impl* impl) const
            {
                if (impl->is_used()) {
                    impl->parent = nullptr; // will be destructed by format_impl_ptr::decrement
                } else {
                    delete impl;
                }
            }
        };

        std::unique_ptr<format_impl, D> format_;
    };

} // namespace detail
} // namespace xlnt
