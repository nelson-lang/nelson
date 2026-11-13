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
//=============================================================================
namespace Nelson {
//=============================================================================
// Visible box [xmin xmax ymin ymax zmin zmax]; an infinite bound is open.
// logScale[k] clips dimension k in log10 space, as the axis draws it.
class LimitsRange
{
public:
    LimitsRange(const std::vector<double>& box, const std::array<bool, 3>& logScale);
    bool
    contains(const std::array<double, 3>& p) const;
    // Data point to clip space (NaN when it cannot be shown on a log axis).
    std::array<double, 3>
    toClip(double x, double y, double z) const;
    std::array<double, 3>
    fromClip(const std::array<double, 3>& p) const;
    double lower[3];
    double upper[3];
    bool log[3];
};
//=============================================================================
// Min/max of the data kept inside a LimitsRange.
class LimitsInRangeAccumulator
{
public:
    explicit LimitsInRangeAccumulator(const LimitsRange& range);
    // Data points inside the range.
    void
    addPoints(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& zs);
    // Polyline segments clipped to the range; non-finite points are dropped first.
    void
    addPolyline(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& zs);
    // Closed polygon clipped to the range (data coordinates).
    void
    addPolygon(const std::vector<std::array<double, 3>>& polygon);
    bool
    empty() const;
    // Spatial limits from the kept data, colour/alpha limits from fullLimits; {} when empty.
    std::vector<double>
    result(const std::vector<double>& fullLimits) const;

private:
    void
    addClipPoint(const std::array<double, 3>& p);
    const LimitsRange& range;
    double mins[3];
    double maxs[3];
    bool hasData = false;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
