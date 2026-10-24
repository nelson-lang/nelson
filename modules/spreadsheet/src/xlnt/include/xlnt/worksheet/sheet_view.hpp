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

#pragma once

#include <xlnt/xlnt_config.hpp>
#include <xlnt/utils/optional.hpp>
#include <xlnt/worksheet/pane.hpp>
#include <xlnt/worksheet/selection.hpp>

namespace xlnt {

/// <summary>
/// Enumeration of possible types of sheet views
/// </summary>
enum class sheet_view_type
{
    normal,
    page_break_preview,
    page_layout
};

/// <summary>
/// Describes a view of a worksheet.
/// Worksheets can have multiple views which show the data differently.
/// </summary>
class XLNT_API sheet_view
{
public:
    /// <summary>
    /// Sets the ID of this view to new_id.
    /// </summary>
    void
    id(std::size_t new_id)
    {
        id_ = new_id;
    }

    /// <summary>
    /// Returns the ID of this view.
    /// </summary>
    std::size_t
    id() const
    {
        return id_;
    }

    /// <summary>
    /// Returns true if this view has a pane defined.
    /// </summary>
    bool
    has_pane() const
    {
        return pane_.is_set();
    }

    /// <summary>
    /// Returns a reference to this view's pane.
    /// Assumes that this view has a pane (please call has_pane() to check).
    /// If this view does not have a pane, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    struct pane&
    pane()
    {
        if (!pane_.is_set()) {
            throw xlnt::invalid_attribute("sheet view has no pane");
        }
        return pane_.get();
    }

    /// <summary>
    /// Returns a reference to this view's pane.
    /// Assumes that this view has a pane (please call has_pane() to check).
    /// If this view does not have a pane, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    const struct pane&
    pane() const
    {
        if (!pane_.is_set()) {
            throw xlnt::invalid_attribute("sheet view has no pane");
        }
        return pane_.get();
    }

    /// <summary>
    /// Removes the defined pane from this view.
    /// </summary>
    void
    clear_pane()
    {
        pane_.clear();
    }

    /// <summary>
    /// Sets the pane of this view to new_pane.
    /// </summary>
    void
    pane(const struct pane& new_pane)
    {
        pane_ = new_pane;
    }

    /// <summary>
    /// Returns true if this view has any selections.
    /// </summary>
    bool
    has_selections() const
    {
        return !selections_.empty();
    }

    /// <summary>
    /// Adds the given selection to the collection of selections.
    /// </summary>
    void
    add_selection(const class selection& new_selection)
    {
        selections_.push_back(new_selection);
    }

    /// <summary>
    /// Removes all selections.
    /// </summary>
    void
    clear_selections()
    {
        selections_.clear();
    }

    /// <summary>
    /// Returns the collection of selections as a vector.
    /// </summary>
    std::vector<xlnt::selection>
    selections() const
    {
        return selections_;
    }

    /// <summary>
    /// Returns the selection at the given index (please call selections().size() if the number of
    /// selections is unknown). If the index is out of bounds, an invalid_parameter exception will
    /// be thrown.
    /// </summary>
    class xlnt::selection&
    selection(std::size_t index)
    {
        if (index >= selections_.size()) {
            throw xlnt::invalid_parameter("sheet view selection at index " + std::to_string(index)
                + " does not exist (" + std::to_string(selections_.size())
                + " selections available)");
        }

        return selections_.at(index);
    }

    /// <summary>
    /// If show is true, grid lines will be shown for sheets using this view.
    /// </summary>
    void
    show_grid_lines(bool show)
    {
        show_grid_lines_ = show;
    }

    /// <summary>
    /// Returns true if grid lines will be shown for sheets using this view.
    /// </summary>
    bool
    show_grid_lines() const
    {
        return show_grid_lines_;
    }

    /// <summary>
    /// If is_default is true, the default grid color will be used.
    /// </summary>
    void
    default_grid_color(bool is_default)
    {
        default_grid_color_ = is_default;
    }

    /// <summary>
    /// Returns true if the default grid color will be used.
    /// </summary>
    bool
    default_grid_color() const
    {
        return default_grid_color_;
    }

    /// <summary>
    /// Sets the type of this view.
    /// </summary>
    void
    type(sheet_view_type new_type)
    {
        type_ = new_type;
    }

    /// <summary>
    /// Returns the type of this view.
    /// </summary>
    sheet_view_type
    type() const
    {
        return type_;
    }

    /// <summary>
    /// has a  top left cell?
    /// </summary>
    bool
    has_top_left_cell() const
    {
        return top_left_cell_.is_set();
    }

    /// <summary>
    /// Sets the top left cell of this view.
    /// </summary>
    void
    top_left_cell(const cell_reference& ref)
    {
        top_left_cell_.set(ref);
    }

    /// <summary>
    /// Returns the top left cell of this view.
    /// Assumes that this view has a top left (please call has_top_left_cell() to check).
    /// If this view does not have a top left cell, an xlnt::invalid_attribute exception will be
    /// thrown.
    /// </summary>
    cell_reference
    top_left_cell() const
    {
        if (!top_left_cell_.is_set()) {
            throw xlnt::invalid_attribute("sheet view has no top left cell");
        }
        return top_left_cell_.get();
    }

    /// <summary>
    /// Sets the zoom scale (percentage) for this view.
    /// </summary>
    void
    zoom_scale(int scale)
    {
        zoom_scale_ = scale;
    }

    /// <summary>
    /// Returns the zoom scale (percentage).
    /// </summary>
    int
    zoom_scale() const
    {
        return zoom_scale_;
    }

    /// <summary>
    /// Returns true if this view is equal to rhs based on its id, grid lines setting,
    /// default grid color, pane, and selections.
    /// </summary>
    bool
    operator==(const sheet_view& rhs) const
    {
        return id_ == rhs.id_ && show_grid_lines_ == rhs.show_grid_lines_
            && default_grid_color_ == rhs.default_grid_color_ && pane_ == rhs.pane_
            && selections_ == rhs.selections_ && top_left_cell_ == rhs.top_left_cell_
            && zoom_scale_ == rhs.zoom_scale_;
    }

    /// <summary>
    /// Returns the negation of the equality operator.
    /// </summary>
    bool
    operator!=(const sheet_view& rhs) const
    {
        return !(*this == rhs);
    }

private:
    /// <summary>
    /// The id
    /// </summary>
    std::size_t id_ = 0;

    /// <summary>
    /// Whether or not to show grid lines
    /// </summary>
    bool show_grid_lines_ = true;

    /// <summary>
    /// Whether or not to use the default grid color
    /// </summary>
    bool default_grid_color_ = true;

    /// <summary>
    /// The type of this view
    /// </summary>
    sheet_view_type type_ = sheet_view_type::normal;

    /// <summary>
    /// The optional pane
    /// </summary>
    optional<xlnt::pane> pane_;

    /// <summary>
    /// The top left cell
    /// </summary>
    optional<cell_reference> top_left_cell_;

    /// <summary>
    /// The collection of selections
    /// </summary>
    std::vector<xlnt::selection> selections_;

    /// <summary>
    /// The zoom scale (percent), e.g. 100
    /// </summary>
    int zoom_scale_ = 100;
};

} // namespace xlnt
