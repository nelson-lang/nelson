// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2010-2015 openpyxl
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

// detail imports must come first in this file.
#include <detail/implementations/format_impl.hpp>
#include <detail/implementations/stylesheet.hpp>

#include <xlnt/styles/format.hpp>
#include <xlnt/styles/style.hpp>

namespace xlnt {

format::format(detail::format_impl_ptr d) : d_(std::move(d)) { }

void
format::clear_style()
{
    d_->style.clear();
}

format&
format::style(const xlnt::style& new_style)
{
    d_ = d_->parent->find_or_create_with(d_, new_style.name());
    return *this;
}

format&
format::style(const std::string& new_style)
{
    d_->style = new_style;
    return *this;
}

bool
format::has_style() const
{
    return d_->style.is_set();
}

style
format::style()
{
    if (!has_style()) {
        throw invalid_attribute("the format has no style");
    }

    return d_->parent->style(d_->style.get());
}

const style
format::style() const
{
    if (!has_style()) {
        throw invalid_attribute("the format has no style");
    }

    return d_->parent->style(d_->style.get());
}

xlnt::alignment
format::alignment() const
{
    if (d_->alignment_id.is_set()) {
        return d_->parent->alignments.at(d_->alignment_id.get());
    }
    return {};
}

format&
format::alignment(const xlnt::alignment& new_alignment, optional<bool> applied)
{
    d_ = d_->parent->find_or_create_with(d_, new_alignment, applied);
    return *this;
}

xlnt::border
format::border() const
{
    if (d_->border_id.is_set()) {
        return d_->parent->borders.at(d_->border_id.get());
    }
    return {};
}

format&
format::border(const xlnt::border& new_border, optional<bool> applied)
{
    d_ = d_->parent->find_or_create_with(d_, new_border, applied);
    return *this;
}

xlnt::fill
format::fill() const
{
    if (d_->fill_id.is_set()) {
        return d_->parent->fills.at(d_->fill_id.get());
    }
    return {};
}

format&
format::fill(const xlnt::fill& new_fill, optional<bool> applied)
{
    d_ = d_->parent->find_or_create_with(d_, new_fill, applied);
    return *this;
}

xlnt::font
format::font() const
{
    if (d_->font_id.is_set()) {
        return d_->parent->fonts.at(d_->font_id.get());
    }
    return {};
}

format&
format::font(const xlnt::font& new_font, optional<bool> applied)
{
    d_ = d_->parent->find_or_create_with(d_, new_font, applied);
    return *this;
}

xlnt::number_format
format::number_format() const
{
    if (d_->number_format_id.is_set()) {
        const auto number_format_id = d_->number_format_id.get();

        if (number_format::is_builtin_format(number_format_id)) {
            return number_format::from_builtin_id(number_format_id);
        }

        const auto it = std::find_if(d_->parent->number_formats.begin(),
            d_->parent->number_formats.end(), [number_format_id](const xlnt::number_format& nf) {
                return nf.id() == number_format_id;
            });
        if (it != d_->parent->number_formats.end()) {
            return *it;
        }
    }

    return xlnt::number_format();
}

format&
format::number_format(const xlnt::number_format& new_number_format, optional<bool> applied)
{
    d_ = d_->parent->find_or_create_with(d_, new_number_format, applied);
    return *this;
}

xlnt::protection
format::protection() const
{
    if (d_->protection_id.is_set()) {
        return d_->parent->protections.at(d_->protection_id.get());
    }
    return {};
}

format&
format::protection(const xlnt::protection& new_protection, optional<bool> applied)
{
    d_ = d_->parent->find_or_create_with(d_, new_protection, applied);
    return *this;
}

bool
format::has_alignment() const
{
    return d_->alignment_id.is_set();
}

bool
format::alignment_applied() const
{
    return d_->alignment_applied.is_set() ? d_->alignment_applied.get() : d_->alignment_id.is_set();
}

bool
format::has_border() const
{
    return d_->border_id.is_set();
}

bool
format::border_applied() const
{
    return d_->border_applied.is_set() ? d_->border_applied.get() : d_->border_id.is_set();
}

bool
format::has_fill() const
{
    return d_->fill_id.is_set();
}

bool
format::fill_applied() const
{
    return d_->fill_applied.is_set() ? d_->fill_applied.get() : d_->fill_id.is_set();
}

bool
format::has_font() const
{
    return d_->font_id.is_set();
}

bool
format::font_applied() const
{
    return d_->font_applied.is_set() ? d_->font_applied.get() : d_->font_id.is_set();
}

bool
format::has_number_format() const
{
    return d_->number_format_id.is_set();
}

bool
format::number_format_applied() const
{
    return d_->number_format_applied.is_set() ? d_->number_format_applied.get()
                                              : d_->number_format_id.is_set();
}

bool
format::has_protection() const
{
    return d_->protection_id.is_set();
}

bool
format::protection_applied() const
{
    return d_->protection_applied.is_set() ? d_->protection_applied.get()
                                           : d_->protection_id.is_set();
}

bool
format::pivot_button() const
{
    return d_->pivot_button_;
}

void
format::pivot_button(bool show)
{
    d_->pivot_button_ = show;
}

bool
format::quote_prefix() const
{
    return d_->quote_prefix_;
}

void
format::quote_prefix(bool quote)
{
    d_->quote_prefix_ = quote;
}

void
detail::format_impl_ptr::increment()
{
    if (format_) {
        ++format_->references;
    }
}

void
detail::format_impl_ptr::decrement()
{
    if (format_ && --format_->references == 0) {
        if (format_->parent) {
            format_->parent->garbage_collect();
        } else {
            delete format_;
        }
    }
}

std::size_t
detail::format_impl_ptr::use_count() const
{
    return format_->references;
}

} // namespace xlnt
