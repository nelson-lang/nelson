//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Find.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
FindTrim(ArrayOfVector a, int cnt, bool first_flag);
//=============================================================================
static int
findCountArgument(const ArrayOf& value)
{
    if ((!value.isNumeric() && !value.isLogical()) || !value.isReal() || !value.isScalar()) {
        Error(L"Nelson:elementary_functions:secondArgumentPositiveScalarInteger",
            L"%s must be a positive integer scalar.", L"Second argument");
    }
    double count = value.asDoubleScalar();
    if (std::isinf(count) && count > 0) {
        return -1;
    }
    if (!std::isfinite(count) || count <= 0 || std::floor(count) != count) {
        Error(L"Nelson:elementary_functions:secondArgumentPositiveScalarInteger",
            L"%s must be a positive integer scalar.", L"Second argument");
    }
    if (count > std::numeric_limits<int>::max()) {
        return std::numeric_limits<int>::max();
    }
    return static_cast<int>(count);
}
//=============================================================================
static Dimensions
findResultDimensions(const ArrayOf& x, indexType nonZero)
{
    if (nonZero == 0) {
        // A scalar or 0x0 input gives a 0x0 empty; other empty results keep their shape.
        Dimensions dx = x.dimensions();
        if (x.isScalar() || (dx.rows() == 0 && dx.columns() == 0)) {
            return Dimensions(0, 0);
        }
    }
    return x.isRowVector() ? Dimensions(1, nonZero) : Dimensions(nonZero, 1);
}
//=============================================================================
template <class T>
static ArrayOfVector
SparseFindMode(const ArrayOf& x, int nLhs)
{
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(x.sparseDataPointer());
    indexType nonZero = 0;
    for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, col);
            it; ++it) {
            if (it.value() != T(0)) {
                ++nonZero;
            }
        }
    }
    double* opLinear = nullptr;
    double* opRow = nullptr;
    double* opCol = nullptr;
    T* opVal = nullptr;
    Dimensions retDim = findResultDimensions(x, nonZero);

    if (nLhs <= 1) {
        opLinear = new_with_exception<double>(nonZero);
    } else {
        opRow = new_with_exception<double>(nonZero);
        opCol = new_with_exception<double>(nonZero);
        if (nLhs > 2) {
            opVal = new_with_exception<T>(nonZero);
        }
    }

    indexType ndx = 0;
    for (signedIndexType col = 0; col < spmatSRC->outerSize(); ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, col);
            it; ++it) {
            if (it.value() == T(0)) {
                continue;
            }
            if (nLhs <= 1) {
                opLinear[ndx] = (double)(it.row() + it.col() * x.rows() + 1);
            } else {
                opRow[ndx] = (double)(it.row() + 1);
                opCol[ndx] = (double)(it.col() + 1);
                if (nLhs > 2) {
                    opVal[ndx] = it.value();
                }
            }
            ++ndx;
        }
    }

    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << ArrayOf(NLS_DOUBLE, retDim, opLinear);
    } else {
        retval << ArrayOf(NLS_DOUBLE, retDim, opRow);
        retval << ArrayOf(NLS_DOUBLE, retDim, opCol);
        if (nLhs > 2) {
            retval << ArrayOf(x.dataClass(), retDim, opVal);
        }
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
SparseFind(const ArrayOfVector& argIn, int nLhs, bool& needToOverload)
{
    const ArrayOf& x = argIn[0];
    needToOverload = false;
    ArrayOfVector retval;
    switch (x.dataClass()) {
    case NLS_LOGICAL:
        retval = SparseFindMode<logical>(x, nLhs);
        break;
    case NLS_DOUBLE:
        retval = SparseFindMode<double>(x, nLhs);
        break;
    case NLS_SINGLE:
        retval = SparseFindMode<single>(x, nLhs);
        break;
    case NLS_DCOMPLEX:
        retval = SparseFindMode<doublecomplex>(x, nLhs);
        break;
    case NLS_SCOMPLEX:
        retval = SparseFindMode<singlecomplex>(x, nLhs);
        break;
    default:
        needToOverload = true;
        return {};
    }

    int k = -1;
    bool firstFlag = true;
    if (argIn.size() > 1) {
        ArrayOf param2(argIn[1]);
        k = findCountArgument(param2);
    }
    if (argIn.size() == 3) {
        std::wstring flag = argIn[2].asWideString();
        std::transform(flag.begin(), flag.end(), flag.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
        if (flag == L"first") {
            firstFlag = true;
        } else if (flag == L"last") {
            firstFlag = false;
        } else {
            Error(L"Nelson:elementary_functions:invalidSearchOptionFirstLast",
                L"Invalid search option. Must be 'first' or 'last'.");
        }
    }
    return FindTrim(retval, k, firstFlag);
}
//=============================================================================
ArrayOfVector
SingleFindModeFull(ArrayOf x)
{
    x.promoteType(NLS_LOGICAL);
    const auto* dp = (const logical*)x.dataPointer();
    indexType len = x.elementCount();
    indexType nonZero = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i] != 0u) {
            nonZero++;
        }
    }
    auto* op = new_with_exception<double>(nonZero);
    indexType ndx = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i] != 0u) {
            op[ndx++] = (double)(i + 1);
        }
    }
    Dimensions retDim = findResultDimensions(x, nonZero);
    ArrayOfVector retval(1);
    retval << ArrayOf(NLS_DOUBLE, retDim, op);
    return retval;
}
//=============================================================================
ArrayOfVector
RCFindModeFull(ArrayOf x)
{
    x.promoteType(NLS_LOGICAL);
    const logical* dp = (const logical*)x.dataPointer();
    indexType len = x.elementCount();
    indexType nonZero = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i] != 0u) {
            nonZero++;
        }
    }
    double* op_row = new_with_exception<double>(nonZero);
    double* op_col = new_with_exception<double>(nonZero);
    indexType rows = x.dimensionLength(0);
    indexType ndx = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i] != 0u) {
            op_row[ndx] = (double)((i % rows) + 1);
            op_col[ndx++] = (double)((i / rows) + 1);
        }
    }
    Dimensions retDim = findResultDimensions(x, nonZero);
    ArrayOfVector retval(2);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_row);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_col);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
RCVFindModeFullReal(const ArrayOf& x)
{
    const T* dp = (const T*)x.dataPointer();
    indexType len = x.elementCount();
    indexType nonZero = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i]) {
            nonZero++;
        }
    }
    auto* op_row = new_with_exception<double>(nonZero);
    auto* op_col = new_with_exception<double>(nonZero);
    T* op_val = new_with_exception<T>(nonZero);
    indexType ndx = 0;
    indexType rows = x.dimensionLength(0);
    for (indexType i = 0; i < len; i++) {
        if (dp[i]) {
            op_row[ndx] = (double)((i % rows) + 1);
            op_col[ndx] = (double)((i / rows) + 1);
            op_val[ndx++] = dp[i];
        }
    }
    Dimensions retDim = findResultDimensions(x, nonZero);
    ArrayOfVector retval(3);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_row);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_col);
    retval << ArrayOf(x.dataClass(), retDim, op_val);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
RCVFindModeFullComplex(const ArrayOf& x)
{
    const T* dp = (const T*)x.dataPointer();
    indexType len = x.elementCount();
    indexType nonZero = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[2 * i] || dp[2 * i + 1]) {
            nonZero++;
        }
    }
    double* op_row = new_with_exception<double>(nonZero);
    double* op_col = new_with_exception<double>(nonZero);
    T* op_val = new_with_exception<T>(2 * nonZero);
    indexType ndx = 0;
    indexType rows = x.dimensionLength(0);
    for (indexType i = 0; i < len; i++) {
        if (dp[2 * i] || dp[2 * i + 1]) {
            op_row[ndx] = (double)((i % rows) + 1);
            op_col[ndx] = (double)((i / rows) + 1);
            op_val[2 * ndx] = dp[2 * i];
            op_val[2 * ndx + 1] = dp[2 * i + 1];
            ndx++;
        }
    }
    Dimensions retDim = findResultDimensions(x, nonZero);
    ArrayOfVector retval(3);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_row);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_col);
    retval << ArrayOf(x.dataClass(), retDim, op_val);
    return retval;
}
//=============================================================================
static ArrayOfVector
RCVFindModeFull(const ArrayOf& x)
{
    switch (x.dataClass()) {
    default: {
    } break;
    case NLS_LOGICAL:
        return RCVFindModeFullReal<logical>(x);
    case NLS_UINT8:
        return RCVFindModeFullReal<uint8>(x);
    case NLS_INT8:
        return RCVFindModeFullReal<int8>(x);
    case NLS_UINT16:
        return RCVFindModeFullReal<uint16>(x);
    case NLS_INT16:
        return RCVFindModeFullReal<int16>(x);
    case NLS_UINT32:
        return RCVFindModeFullReal<uint32>(x);
    case NLS_INT32:
        return RCVFindModeFullReal<int32>(x);
    case NLS_UINT64:
        return RCVFindModeFullReal<uint64>(x);
    case NLS_INT64:
        return RCVFindModeFullReal<int64>(x);
    case NLS_SINGLE:
        return RCVFindModeFullReal<single>(x);
    case NLS_DOUBLE:
        return RCVFindModeFullReal<double>(x);
    case NLS_SCOMPLEX:
        return RCVFindModeFullComplex<single>(x);
    case NLS_DCOMPLEX:
        return RCVFindModeFullComplex<double>(x);
    case NLS_CHAR:
        return RCVFindModeFullReal<charType>(x);
    }
    return {};
}
//=============================================================================
ArrayOfVector
FindTrim(ArrayOfVector a, int cnt, bool first_flag)
{
    if (cnt < 0 || a.empty()) {
        return a;
    }
    indexType N = a[0].elementCount();
    if (cnt > N) {
        return a;
    }
    ArrayOfVector ret;
    ArrayOf ndx;
    bool vertflag = !(a[0].isRowVector());
    if (first_flag) {
        ndx = ArrayOf::integerRange(1, 1, cnt, vertflag);
    } else {
        ndx = ArrayOf::integerRange((N - cnt) + 1, 1, N, vertflag);
    }
    for (auto& i : a) {
        ret.push_back(i.vectorSubset(ndx));
    }
    return ret;
}
//=============================================================================
ArrayOfVector
Find(const ArrayOfVector& argIn, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
    needToOverload = false;
    if (argIn[0].isSparse()) {
        return SparseFind(argIn, nLhs, needToOverload);
    }
    if (argIn[0].isReferenceType()) {
        needToOverload = true;
        return retval;
    }
    ArrayOf x(argIn[0]);
    switch (x.dataClass()) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR: {
        needToOverload = false;
    } break;
    default: {
        needToOverload = true;
        return retval;
    } break;
    }
    int k = -1;
    bool first_flag = true;
    if (argIn.size() > 1) {
        ArrayOf param2(argIn[1]);
        k = findCountArgument(param2);
    }
    if (argIn.size() == 3) {
        std::wstring flag = argIn[2].asWideString();
        std::transform(flag.begin(), flag.end(), flag.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
        if (flag == L"first") {
            first_flag = true;
        } else if (flag == L"last") {
            first_flag = false;
        } else {
            Error(L"Nelson:elementary_functions:invalidSearchOptionFirstLast",
                L"Invalid search option. Must be 'first' or 'last'.");
        }
    }
    switch (nLhs) {
    case 0:
    case 1: {
        return FindTrim(SingleFindModeFull(x), k, first_flag);
    } break;
    case 2: {
        return FindTrim(RCFindModeFull(x), k, first_flag);
    } break;
    case 3: {
        return FindTrim(RCVFindModeFull(x), k, first_flag);
    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
