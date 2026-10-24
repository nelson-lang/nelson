//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <algorithm>
#include <cmath>
#include <vector>
//=============================================================================
namespace nflow {
struct NormalizeResult
{
    std::vector<double> trimmed;
    bool allZero;
};
//=============================================================================
inline NormalizeResult
normalizePoly(const std::vector<double>& values)
{
    std::vector<double> arr;
    arr.reserve(values.size());
    for (double v : values) {
        arr.push_back(std::isfinite(v) ? v : 0.0);
    }
    size_t idx = 0;
    while (idx + 1 < arr.size() && std::fabs(arr[idx]) < 1e-12) {
        ++idx;
    }
    std::vector<double> tr;
    if (arr.empty()) {
        tr.push_back(0.0);
    } else {
        tr.assign(arr.begin() + static_cast<std::ptrdiff_t>(idx), arr.end());
    }
    bool az = true;
    for (double v : tr) {
        if (std::fabs(v) >= 1e-12) {
            az = false;
            break;
        }
    }
    if (az) {
        tr = { 0.0 };
    }
    return { tr, az };
}
//=============================================================================
inline std::vector<double>
polyAdd(const std::vector<double>& a, const std::vector<double>& b)
{
    size_t len = std::max(a.size(), b.size());
    std::vector<double> out(len, 0.0);
    for (size_t i = 0; i < len; ++i) {
        out[i] = (i < a.size() ? a[i] : 0.0) + (i < b.size() ? b[i] : 0.0);
    }
    return out;
}
//=============================================================================
inline std::vector<double>
polyScale(const std::vector<double>& a, double k)
{
    std::vector<double> out;
    out.reserve(a.size());
    for (double v : a) {
        out.push_back(v * k);
    }
    return out;
}
//=============================================================================
inline std::vector<double>
polyMul(const std::vector<double>& a, const std::vector<double>& b)
{
    if (a.empty() || b.empty()) {
        return { 0.0 };
    }
    std::vector<double> out(a.size() + b.size() - 1, 0.0);
    for (size_t i = 0; i < a.size(); ++i) {
        for (size_t j = 0; j < b.size(); ++j) {
            out[i + j] += a[i] * b[j];
        }
    }
    return out;
}
//=============================================================================
inline std::vector<double>
polyPow(const std::vector<double>& base, int exp)
{
    std::vector<double> out = { 1.0 };
    for (int i = 0; i < exp; ++i) {
        out = polyMul(out, base);
    }
    return out;
}
//=============================================================================
} // namespace nflow
//=============================================================================
