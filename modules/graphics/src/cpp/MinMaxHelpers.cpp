//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <algorithm>
#include <limits>
#include "nlsBuildConfig.h"
#include "MinMaxHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
findVectorMin(const std::vector<double>& v)
{
    double min = 0;
    bool first = true;
#if WITH_OPENMP
#pragma omp parallel for reduction(min : min) if (v.size() > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)v.size(); i++) {
        if (std::isfinite(v[i])) {
            if (first) {
                first = false;
                min = v[i];
            } else if (v[i] < min) {
                min = v[i];
            }
        }
    }
    return min;
}
//=============================================================================
double
findVectorMax(const std::vector<double>& v)
{
    double max = 0;
    bool first = true;
#if WITH_OPENMP
#pragma omp parallel for reduction(max : max) if (v.size() > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)v.size(); i++) {
        if (std::isfinite(v[i])) {
            if (first) {
                first = false;
                max = v[i];
            } else if (v[i] > max) {
                max = v[i];
            }
        }
    }
    return max;
}
//=============================================================================
double
ArrayOfMin(const ArrayOf& a)
{
    ArrayOf _a(a);
    if (a.isEmpty()) {
        return 0;
    }
    _a.promoteType(NLS_DOUBLE);
    const double* v = (const double*)_a.dataPointer();
    indexType len = a.elementCount();
    double min = 0;
    bool first = true;
#if WITH_OPENMP
#pragma omp parallel for reduction(min : min) if (len > OMP_DEFAULT_THRESHOLD)
#endif
    for (indexType i = 0; i < len; i++) {
        if (std::isfinite(v[i])) {
            if (first) {
                first = false;
                min = v[i];
            } else if (v[i] < min) {
                min = v[i];
            }
        }
    }
    return min;
}
//=============================================================================
double
ArrayOfMax(const ArrayOf& a)
{
    ArrayOf _a(a);
    if (_a.isEmpty()) {
        return 0;
    }
    _a.promoteType(NLS_DOUBLE);
    const double* v = (const double*)_a.dataPointer();
    indexType len = _a.elementCount();
    double max = 0;
    bool first = true;
#if WITH_OPENMP
#pragma omp parallel for reduction(max : max) if (len > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
        if (std::isfinite(v[i])) {
            if (first) {
                first = false;
                max = v[i];
            } else if (v[i] > max) {
                max = v[i];
            }
        }
    }
    return max;
}
//=============================================================================
void
PointsExtent::add(double x, double y, double z)
{
    if (!std::isfinite(x) || !std::isfinite(y) || !std::isfinite(z)) {
        return;
    }
    if (empty) {
        box[0] = box[1] = x;
        box[2] = box[3] = y;
        box[4] = box[5] = z;
        empty = false;
        return;
    }
    box[0] = std::min(box[0], x);
    box[1] = std::max(box[1], x);
    box[2] = std::min(box[2], y);
    box[3] = std::max(box[3], y);
    box[4] = std::min(box[4], z);
    box[5] = std::max(box[5], z);
}
//=============================================================================
void
PointsExtent::appendTo(std::vector<double>& limits) const
{
    const double inf = std::numeric_limits<double>::infinity();
    for (int k = 0; k < 6; k += 2) {
        limits.push_back(empty ? inf : box[k]);
        limits.push_back(empty ? -inf : box[k + 1]);
    }
}
//=============================================================================
void
appendPointsExtent(std::vector<double>& limits, const std::vector<double>& xs,
    const std::vector<double>& ys, const std::vector<double>& zs)
{
    PointsExtent extent;
    size_t n = std::min(xs.size(), ys.size());
    for (size_t i = 0; i < n; i++) {
        extent.add(xs[i], ys[i], i < zs.size() ? zs[i] : 0.0);
    }
    extent.appendTo(limits);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
