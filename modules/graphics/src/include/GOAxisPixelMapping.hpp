//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <array>
#include <vector>
#include "nlsGraphics_exports.h"
//=============================================================================
class RenderInterface;
//=============================================================================
namespace Nelson {
//=============================================================================
class GOAxis;
//=============================================================================
// The axes projection is orthographic, so mapped data -> (pixel x, pixel y, eye
// depth) is affine: it is sampled once on the projection set on the renderer and
// inverted to turn window pixels back into data coordinates.
class NLSGRAPHICS_IMPEXP GOAxisPixelMapping
{
public:
    using Vec3 = std::array<double, 3>;
    // Samples the projection of 'gc' (set by GOAxis::setupProjectionOnly).
    bool
    initialize(GOAxis* axis, RenderInterface& gc);
    // Data coordinates -> window pixel x, pixel y and eye depth.
    Vec3
    dataToWindow(const std::vector<double>& data) const;
    // Window pixel at the given eye depth -> data coordinates.
    bool
    windowToData(double px, double py, double depth, Vec3& data) const;
    // Eye depths of the nearest and farthest corners of the axes box.
    double
    nearDepth() const
    {
        return m_nearDepth;
    }
    double
    farDepth() const
    {
        return m_farDepth;
    }

private:
    Vec3
    toWindow(const Vec3& mapped) const;
    GOAxis* m_axis = nullptr;
    RenderInterface* m_gc = nullptr;
    Vec3 m_origin { 0, 0, 0 };
    Vec3 m_base { 0, 0, 0 };
    std::array<Vec3, 3> m_columns {};
    double m_nearDepth = 0;
    double m_farDepth = 0;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
