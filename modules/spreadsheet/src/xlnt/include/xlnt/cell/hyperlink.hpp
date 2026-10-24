// Copyright (c) 2018-2022 Thomas Fussell
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

#include <xlnt/xlnt_config.hpp>
#include <string>

namespace xlnt {

namespace detail {
    struct hyperlink_impl;
}

class cell;
class range;
class relationship;

/// <summary>
/// Describes a hyperlink pointing from a cell to another cell or a URL.
/// </summary>
class XLNT_API hyperlink
{
public:
    bool
    external() const;

    /// <summary>
    /// Returns the relationship of this hyperlink.
    /// If this hyperlink does not have a relationship because it is internal (external() returns
    /// false), an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    class relationship
    relationship() const;

    /// <summary>
    /// Returns the URL (external target) of this hyperlink.
    /// If this hyperlink does not have a URL because it is internal (external() returns false),
    /// an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    std::string
    url() const;

    /// <summary>
    /// Returns the target range (internal target) of this hyperlink.
    /// If this hyperlink does not have a target range because it is external (external() returns
    /// true), an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    std::string
    target_range() const;

    /// <summary>
    /// Returns whether this hyperlink has a displayed text.
    /// </summary>
    bool
    has_display() const;

    /// <summary>
    /// Sets the displayed text of this hyperlink.
    /// </summary>
    void
    display(const std::string& value);

    /// <summary>
    /// Clears the displayed text of this hyperlink (has_display() will return false afterwards).
    /// </summary>
    void
    clear_display();

    /// <summary>
    /// Returns the displayed text of this hyperlink.
    /// Assumes that this hyperlink has a displayed text (please call has_display() to check).
    /// If this hyperlink does not have a displayed text, an empty string will be returned.
    /// </summary>
    const std::string&
    display() const;

    /// <summary>
    /// Returns true if this hyperlink has a tooltip.
    /// </summary>
    bool
    has_tooltip() const;

    /// <summary>
    /// Sets the tooltip of this hyperlink.
    /// </summary>
    void
    tooltip(const std::string& value);

    /// <summary>
    /// Clears the tooltip of this hyperlink (has_tooltip() will return false afterwards).
    /// </summary>
    void
    clear_tooltip();

    /// <summary>
    /// Returns the tooltip of this hyperlink.
    /// Assumes that this hyperlink has a tooltip (please call has_tooltip() to check).
    /// If this hyperlink does not have a tooltip, an empty string will be returned.
    /// </summary>
    const std::string&
    tooltip() const;

    /// <summary>
    /// Returns whether this hyperlink has a location.
    /// </summary>
    bool
    has_location() const;

    /// <summary>
    /// Sets the location of this hyperlink.
    /// </summary>
    void
    location(const std::string& value);

    /// <summary>
    /// Clears the location of this hyperlink (has_location() will return false afterwards).
    /// </summary>
    void
    clear_location();

    /// <summary>
    /// Returns the location of this hyperlink.
    /// Assumes that this hyperlink has a location (please call has_location() to check).
    /// If this hyperlink does not have a location, an empty string will be returned.
    /// </summary>
    const std::string&
    location() const;

private:
    friend class cell;
    hyperlink(detail::hyperlink_impl* d);
    detail::hyperlink_impl* d_ = nullptr;
};

} // namespace xlnt
