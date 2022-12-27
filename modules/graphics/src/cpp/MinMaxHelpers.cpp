//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "MinMaxHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
findVectorMin(const std::vector<double>& v)
{
    double min = 0;
    bool first = true;
    for (int i = 0; i < v.size(); i++) {
        if (std::isfinite(v[i]))
            if (first) {
                first = false;
                min = v[i];
            } else if (v[i] < min) {
                min = v[i];
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
    for (int i = 0; i < v.size(); i++) {
        if (std::isfinite(v[i]))
            if (first) {
                first = false;
                max = v[i];
            } else if (v[i] > max) {
                max = v[i];
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
    const double* v = (const double*)_a.getDataPointer();
    indexType len = a.getElementCount();
    double min = 0;
    bool first = true;
    for (indexType i = 0; i < len; i++) {
        if (std::isfinite(v[i]))
            if (first) {
                first = false;
                min = v[i];
            } else if (v[i] < min) {
                min = v[i];
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
    const double* v = (const double*)_a.getDataPointer();
    indexType len = _a.getElementCount();
    double max = 0;
    bool first = true;
    for (indexType i = 0; i < len; i++) {
        if (std::isfinite(v[i]))
            if (first) {
                first = false;
                max = v[i];
            } else if (v[i] > max) {
                max = v[i];
            }
    }
    return max;
}
//=============================================================================
}
//=============================================================================
