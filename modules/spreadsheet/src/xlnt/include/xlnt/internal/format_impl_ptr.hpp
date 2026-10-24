// Copyright (c) 2025-2026 xlnt-community
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

namespace xlnt {
namespace detail {

    struct format_impl;

    class XLNT_API format_impl_ptr
    {
    public:
        format_impl_ptr() = default;
        format_impl_ptr(const format_impl_ptr& r) : format_(r.format_) { increment(); }
        format_impl_ptr(format_impl_ptr&& r) : format_(r.format_) { r.format_ = nullptr; }
        format_impl_ptr(format_impl* format) : format_(format) { increment(); }
        ~format_impl_ptr() { decrement(); }

        format_impl_ptr&
        operator=(const format_impl_ptr& r)
        {
            if (this == &r) {
                return *this;
            }

            decrement();
            format_ = r.format_;
            increment();
            return *this;
        }

        format_impl_ptr&
        operator=(format_impl_ptr&& r)
        {
            decrement();
            format_ = r.format_;
            r.format_ = nullptr;
            return *this;
        }

        std::size_t
        use_count() const;

        bool
        is_set() const
        {
            return format_ != nullptr;
        }
        void
        clear()
        {
            operator=(nullptr);
        }

        format_impl*
        get() const
        {
            return format_;
        }
        format_impl*
        operator->() const
        {
            return get();
        }
        operator format_impl*() const { return get(); }

        bool
        operator==(const format_impl_ptr& r) const
        {
            return format_ == r.format_;
        }
        bool
        operator==(format_impl* format) const
        {
            return format_ == format;
        }

    protected:
        void
        increment();
        void
        decrement();

    protected:
        format_impl* format_ = nullptr;
    };

} // namespace detail
} // namespace xlnt
