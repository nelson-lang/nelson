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
#include <cstring>
#include <vector>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline void
parseTrapzArgs(
    const ArrayOfVector& argIn, ArrayOf& x, ArrayOf& y, bool& hasX, indexType& dimArg, bool& hasDim)
{
    hasX = false;
    hasDim = false;
    dimArg = 0;
    size_t n = argIn.size();
    if (n == 1) {
        y = argIn[0];
    } else if (n == 2) {
        if (argIn[1].isScalar()) {
            y = argIn[0];
            dimArg = (indexType)argIn[1].asDoubleScalar();
            hasDim = true;
        } else {
            x = argIn[0];
            y = argIn[1];
            hasX = true;
        }
    } else {
        x = argIn[0];
        y = argIn[1];
        hasX = true;
        dimArg = (indexType)argIn[2].asDoubleScalar();
        hasDim = true;
    }
}
//=============================================================================
template <class T>
ArrayOf
applyTrapz(const ArrayOf& y, const ArrayOf& xArg, bool hasX, indexType opdim, NelsonType cls,
    bool cumulative)
{
    Dimensions dims = y.dimensions();
    indexType nd = dims.length();
    indexType n = (opdim < nd) ? dims[opdim] : 1;
    indexType total = y.elementCount();
    const T* src = (const T*)y.dataPointer();
    indexType stride = 1;
    for (indexType i = 0; i < opdim; ++i) {
        stride *= dims[i];
    }
    indexType numLines = (n == 0) ? 0 : total / n;
    std::vector<T> dx;
    if (hasX && n >= 2) {
        if (xArg.elementCount() != n) {
            Error(L"Nelson:linear_algebra:trapzLengthMismatch",
                L"X must have the same length as the operating dimension of Y.");
        }
        const T* xp = (const T*)xArg.dataPointer();
        dx.resize(n - 1);
        for (indexType k = 0; k < n - 1; ++k) {
            dx[k] = xp[k + 1] - xp[k];
        }
    }
    if (!cumulative) {
        Dimensions od = dims;
        if (opdim < nd) {
            od[opdim] = 1;
        }
        indexType outCount = od.elementCount();
        T* out = (T*)ArrayOf::allocateArrayOf(cls, outCount);
        memset(out, 0, sizeof(T) * outCount);
        if (n >= 2) {
            for (indexType line = 0; line < numLines; ++line) {
                indexType base = (line / stride) * (stride * n) + (line % stride);
                T acc = (T)0;
                for (indexType k = 0; k < n - 1; ++k) {
                    T h = dx.empty() ? (T)1 : dx[k];
                    acc += (src[base + k * stride] + src[base + (k + 1) * stride]) * (T)0.5 * h;
                }
                out[line] = acc;
            }
        }
        return ArrayOf(cls, od, out);
    }
    T* out = (T*)ArrayOf::allocateArrayOf(cls, total);
    memset(out, 0, sizeof(T) * total);
    if (n >= 2) {
        for (indexType line = 0; line < numLines; ++line) {
            indexType base = (line / stride) * (stride * n) + (line % stride);
            T run = (T)0;
            for (indexType k = 0; k < n - 1; ++k) {
                T h = dx.empty() ? (T)1 : dx[k];
                run += (src[base + k * stride] + src[base + (k + 1) * stride]) * (T)0.5 * h;
                out[base + (k + 1) * stride] = run;
            }
        }
    }
    return ArrayOf(cls, dims, out);
}
//=============================================================================
inline ArrayOf
trapzDispatch(const ArrayOfVector& argIn, bool cumulative)
{
    ArrayOf x;
    ArrayOf y;
    bool hasX = false;
    bool hasDim = false;
    indexType dimArg = 0;
    parseTrapzArgs(argIn, x, y, hasX, dimArg, hasDim);
    if (y.isEmpty()) {
        return ArrayOf::doubleScalar(0.0);
    }
    if (y.isClassType() || y.isHandle()) {
        Error(L"Nelson:linear_algebra:expectedRealNumericArray",
            L"Input must be a real numeric array.");
    }
    NelsonType cls = (y.dataClass() == NLS_SINGLE) ? NLS_SINGLE : NLS_DOUBLE;
    ArrayOf yy(y);
    yy.promoteType(cls);
    ArrayOf xx;
    if (hasX) {
        xx = ArrayOf(x);
        xx.promoteType(cls);
    }
    Dimensions dims = yy.dimensions();
    indexType nd = dims.length();
    indexType opdim = 0;
    if (hasDim) {
        if (dimArg < 1) {
            Error(L"Nelson:linear_algebra:invalidDimension",
                L"Dimension argument must be a positive integer scalar.");
        }
        opdim = dimArg - 1;
    } else {
        opdim = 0;
        for (indexType i = 0; i < nd; ++i) {
            if (dims[i] != 1) {
                opdim = i;
                break;
            }
        }
    }
    if (cls == NLS_SINGLE) {
        return applyTrapz<single>(yy, xx, hasX, opdim, cls, cumulative);
    }
    return applyTrapz<double>(yy, xx, hasX, opdim, cls, cumulative);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
