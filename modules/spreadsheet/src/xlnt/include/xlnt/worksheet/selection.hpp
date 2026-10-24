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

#include <vector>

#include <xlnt/xlnt_config.hpp>
#include <xlnt/cell/cell_reference.hpp>
#include <xlnt/worksheet/pane.hpp>
#include <xlnt/worksheet/range_reference.hpp>
#include <xlnt/utils/exceptions.hpp>
#include <xlnt/utils/optional.hpp>

namespace xlnt {

/// <summary>
/// The selected area of a worksheet.
/// </summary>
class XLNT_API selection
{
public:
    /// <summary>
    /// default ctor
    /// </summary>
    explicit selection() = default;

    /// <summary>
    /// ctor when no range selected
    /// sqref == active_cell
    /// </summary>
    explicit selection(pane_corner quadrant, cell_reference active_cell)
        : active_cell_(active_cell)
        , sqref_({ range_reference(active_cell, active_cell) })
        , pane_(quadrant)
    {
    }

    /// <summary>
    /// ctor with selected range
    /// sqref must contain active_cell
    /// </summary>
    explicit selection(pane_corner quadrant, cell_reference active_cell, range_reference selected)
        : active_cell_(active_cell), sqref_({ selected }), pane_(quadrant)
    {
    }

    /// <summary>
    /// Returns true if this selection has a defined active cell.
    /// </summary>
    bool
    has_active_cell() const
    {
        return active_cell_.is_set();
    }

    /// <summary>
    /// Returns the cell reference of the active cell.
    /// Assumes that this selection has an active cell (please call has_active_cell() to check).
    /// If this selection does not have an active cell, an xlnt::invalid_attribute exception will be
    /// thrown.
    /// </summary>
    cell_reference
    active_cell() const
    {
        if (!active_cell_.is_set()) {
            throw xlnt::invalid_attribute("selection has no active cell");
        }
        return active_cell_.get();
    }

    /// <summary>
    /// Sets the active cell to that pointed to by ref.
    /// </summary>
    void
    active_cell(const cell_reference& ref)
    {
        active_cell_ = ref;
    }

    /// <summary>
    /// Clears the active cell.
    /// </summary>
    void
    clear_active_cell()
    {
        active_cell_.clear();
    }

    /// <summary>
    /// Returns true if this selection has a defined sqref.
    /// </summary>
    bool
    has_sqref() const
    {
        return !sqref_.empty();
    }

    /// <summary>
    /// Returns the range encompassed by this selection.
    /// If the range contains multiple (non-contiguous) regions, the first range is returned.
    /// Use sqrefs to obtain the full selection.
    /// Assumes that at least one region exists (please call has_sqref() to check).
    /// If there are no regions, an xlnt::invalid_attribute exception will be thrown.
    /// </summary>
    /// <deprecated>
    /// Use sqrefs instead.
    /// </deprecated>
    XLNT_DEPRECATED range_reference
    sqref() const
    {
        if (!has_sqref()) {
            throw invalid_attribute("the selection has no sqref");
        }

        return sqref_.front();
    }

    /// <summary>
    /// Returns the range encompassed by this selection.
    /// </summary>
    const std::vector<range_reference>&
    sqrefs() const
    {
        return sqref_;
    }

    /// <summary>
    /// Sets the range encompassed by this selection.
    /// </summary>
    void
    sqref(const range_reference& ref)
    {
        sqref_ = { ref };
    }

    /// <summary>
    /// Sets the range encompassed by this selection.
    /// </summary>
    void
    sqref(const std::vector<range_reference>& ref)
    {
        sqref_ = ref;
    }

    /// <summary>
    /// Sets the range encompassed by this selection.
    /// The provided range should be a space delimited list of range references.
    /// E.g. "A1 B2:C3"
    /// </summary>
    void
    sqref(const std::string& ref);

    /// <summary>
    /// Returns the sheet quadrant of this selection.
    /// </summary>
    pane_corner
    pane() const
    {
        return pane_;
    }

    /// <summary>
    /// Sets the sheet quadrant of this selection to corner.
    /// </summary>
    void
    pane(pane_corner corner)
    {
        pane_ = corner;
    }

    /// <summary>
    /// Returns true if this selection is equal to rhs based on its active cell,
    /// sqref, and pane.
    /// </summary>
    bool
    operator==(const selection& rhs) const
    {
        return active_cell_ == rhs.active_cell_ && sqref_ == rhs.sqref_ && pane_ == rhs.pane_;
    }

    // <summary>
    /// Returns the negation of the equality operator.
    /// </summary>
    bool
    operator!=(const selection& rhs) const
    {
        return !(*this == rhs);
    }

private:
    /// <summary>
    /// The last selected cell in the selection
    /// </summary>
    optional<cell_reference> active_cell_;

    /// <summary>
    /// The last selected block in the selection
    /// contains active_cell_, normally == to active_cell_
    /// </summary>
    std::vector<range_reference> sqref_;

    /// <summary>
    /// The corner of the worksheet that this selection extends to
    /// </summary>
    pane_corner pane_ = pane_corner::top_left;
};

} // namespace xlnt
