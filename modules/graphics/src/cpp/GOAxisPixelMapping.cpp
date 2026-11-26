//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include "GOAxisPixelMapping.hpp"
#include "GOAxis.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using Vec3 = GOAxisPixelMapping::Vec3;
//=============================================================================
static double
determinant(const std::array<Vec3, 3>& m)
{
    return m[0][0] * (m[1][1] * m[2][2] - m[2][1] * m[1][2])
        - m[1][0] * (m[0][1] * m[2][2] - m[2][1] * m[0][2])
        + m[2][0] * (m[0][1] * m[1][2] - m[1][1] * m[0][2]);
}
//=============================================================================
// Solves columns * d = rhs (Cramer), columns[i] being the i-th column.
static bool
solve3(const std::array<Vec3, 3>& columns, const Vec3& rhs, Vec3& d)
{
    double det = determinant(columns);
    if (!std::isfinite(det) || std::abs(det) < 1e-300) {
        return false;
    }
    for (int i = 0; i < 3; i++) {
        std::array<Vec3, 3> m = columns;
        m[i] = rhs;
        d[i] = determinant(m) / det;
    }
    return true;
}
//=============================================================================
// Inverse of GOAxis::mapX/mapY/mapZ: the log scale is applied first, then the
// reversed direction mirrors the value inside the limits.
static double
unmapCoordinate(GOAxis* axis, double value, const std::wstring& dirName,
    const std::wstring& limName, const std::wstring& scaleName)
{
    if (axis->stringCheck(dirName, GO_PROPERTY_VALUE_REVERSE_STR)) {
        std::vector<double> lims = axis->findVectorDoubleProperty(limName);
        if (lims.size() == 2) {
            value = lims[0] + lims[1] - value;
        }
    }
    if (axis->stringCheck(scaleName, GO_PROPERTY_VALUE_LOG_STR)) {
        value = std::pow(10.0, value);
    }
    return value;
}
//=============================================================================
Vec3
GOAxisPixelMapping::toWindow(const Vec3& p) const
{
    Vec3 w { 0, 0, 0 };
    double eyeX = 0;
    double eyeY = 0;
    m_gc->toPixels(p[0], p[1], p[2], w[0], w[1]);
    m_gc->mapPoint(p[0], p[1], p[2], eyeX, eyeY, w[2]);
    return w;
}
//=============================================================================
bool
GOAxisPixelMapping::initialize(GOAxis* axis, RenderInterface& gc)
{
    m_axis = axis;
    m_gc = &gc;
    std::vector<double> xlim = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
    std::vector<double> ylim = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
    std::vector<double> zlim = axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);
    if (xlim.size() != 2 || ylim.size() != 2 || zlim.size() != 2) {
        return false;
    }
    std::vector<double> corners;
    corners.reserve(24);
    for (double cx : xlim) {
        for (double cy : ylim) {
            for (double cz : zlim) {
                corners.insert(corners.end(), { cx, cy, cz });
            }
        }
    }
    std::vector<double> mapped = axis->reMap(corners, false);
    if (mapped.size() < 3) {
        return false;
    }
    Vec3 low { mapped[0], mapped[1], mapped[2] };
    Vec3 high = low;
    m_nearDepth = -HUGE_VAL;
    m_farDepth = HUGE_VAL;
    for (size_t k = 0; k + 2 < mapped.size(); k += 3) {
        Vec3 corner { mapped[k], mapped[k + 1], mapped[k + 2] };
        for (int i = 0; i < 3; i++) {
            low[i] = std::min(low[i], corner[i]);
            high[i] = std::max(high[i], corner[i]);
        }
        double depth = toWindow(corner)[2];
        m_nearDepth = std::max(m_nearDepth, depth);
        m_farDepth = std::min(m_farDepth, depth);
    }
    for (int i = 0; i < 3; i++) {
        m_origin[i] = (low[i] + high[i]) / 2.0;
    }
    m_base = toWindow(m_origin);
    for (int i = 0; i < 3; i++) {
        double step = std::max(high[i] - low[i], 1.0);
        Vec3 shifted = m_origin;
        shifted[i] += step;
        Vec3 moved = toWindow(shifted);
        for (int r = 0; r < 3; r++) {
            m_columns[i][r] = (moved[r] - m_base[r]) / step;
        }
    }
    return true;
}
//=============================================================================
Vec3
GOAxisPixelMapping::dataToWindow(const std::vector<double>& data) const
{
    std::vector<double> mapped = m_axis->reMap(data, false);
    if (mapped.size() < 3) {
        return { 0, 0, 0 };
    }
    return toWindow({ mapped[0], mapped[1], mapped[2] });
}
//=============================================================================
bool
GOAxisPixelMapping::windowToData(double px, double py, double depth, Vec3& data) const
{
    Vec3 d { 0, 0, 0 };
    if (!solve3(m_columns, { px - m_base[0], py - m_base[1], depth - m_base[2] }, d)) {
        return false;
    }
    data[0] = unmapCoordinate(m_axis, m_origin[0] + d[0], GO_X_DIR_PROPERTY_NAME_STR,
        GO_X_LIM_PROPERTY_NAME_STR, GO_X_SCALE_PROPERTY_NAME_STR);
    data[1] = unmapCoordinate(m_axis, m_origin[1] + d[1], GO_Y_DIR_PROPERTY_NAME_STR,
        GO_Y_LIM_PROPERTY_NAME_STR, GO_Y_SCALE_PROPERTY_NAME_STR);
    data[2] = unmapCoordinate(m_axis, m_origin[2] + d[2], GO_Z_DIR_PROPERTY_NAME_STR,
        GO_Z_LIM_PROPERTY_NAME_STR, GO_Z_SCALE_PROPERTY_NAME_STR);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
