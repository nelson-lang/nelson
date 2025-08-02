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
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
SingleFindModeFull(ArrayOf x)
{
    x.promoteType(NLS_LOGICAL);
    const auto* dp = (const logical*)x.getDataPointer();
    indexType len = x.getElementCount();
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
    Dimensions retDim(2);
    if (x.isRowVector()) {
        retDim.setDimensionLength(0, 1);
        retDim.setDimensionLength(1, nonZero);
    } else {
        retDim.setDimensionLength(0, nonZero);
        retDim.setDimensionLength(1, 1);
    }
    ArrayOfVector retval(1);
    retval << ArrayOf(NLS_DOUBLE, retDim, op);
    return retval;
}
//=============================================================================
ArrayOfVector
RCFindModeFull(ArrayOf x)
{
    x.promoteType(NLS_LOGICAL);
    const logical* dp = (const logical*)x.getDataPointer();
    indexType len = x.getElementCount();
    indexType nonZero = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i] != 0u) {
            nonZero++;
        }
    }
    double* op_row = new_with_exception<double>(nonZero);
    double* op_col = new_with_exception<double>(nonZero);
    indexType rows = x.getDimensionLength(0);
    indexType ndx = 0;
    for (indexType i = 0; i < len; i++) {
        if (dp[i] != 0u) {
            op_row[ndx] = (double)((i % rows) + 1);
            op_col[ndx++] = (double)((i / rows) + 1);
        }
    }
    Dimensions retDim(2);
    if (x.isRowVector()) {
        retDim.setDimensionLength(0, 1);
        retDim.setDimensionLength(1, nonZero);
    } else {
        retDim.setDimensionLength(0, nonZero);
        retDim.setDimensionLength(1, 1);
    }
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
    const T* dp = (const T*)x.getDataPointer();
    indexType len = x.getElementCount();
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
    indexType rows = x.getDimensionLength(0);
    for (indexType i = 0; i < len; i++) {
        if (dp[i]) {
            op_row[ndx] = (double)((i % rows) + 1);
            op_col[ndx] = (double)((i / rows) + 1);
            op_val[ndx++] = dp[i];
        }
    }
    Dimensions retDim(2);
    if (x.isRowVector()) {
        retDim.setDimensionLength(0, 1);
        retDim.setDimensionLength(1, nonZero);
    } else {
        retDim.setDimensionLength(0, nonZero);
        retDim.setDimensionLength(1, 1);
    }
    ArrayOfVector retval(3);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_row);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_col);
    retval << ArrayOf(x.getDataClass(), retDim, op_val);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
RCVFindModeFullComplex(const ArrayOf& x)
{
    const T* dp = (const T*)x.getDataPointer();
    indexType len = x.getElementCount();
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
    indexType rows = x.getDimensionLength(0);
    for (indexType i = 0; i < len; i++) {
        if (dp[2 * i] || dp[2 * i + 1]) {
            op_row[ndx] = (double)((i % rows) + 1);
            op_col[ndx] = (double)((i / rows) + 1);
            op_val[2 * ndx] = dp[2 * i];
            op_val[2 * ndx + 1] = dp[2 * i + 1];
            ndx++;
        }
    }
    Dimensions retDim(2);
    if (x.isRowVector()) {
        retDim.setDimensionLength(0, 1);
        retDim.setDimensionLength(1, nonZero);
    } else {
        retDim.setDimensionLength(0, nonZero);
        retDim.setDimensionLength(1, 1);
    }
    ArrayOfVector retval(3);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_row);
    retval << ArrayOf(NLS_DOUBLE, retDim, op_col);
    retval << ArrayOf(x.getDataClass(), retDim, op_val);
    return retval;
}
//=============================================================================
static ArrayOfVector
RCVFindModeFull(const ArrayOf& x)
{
    switch (x.getDataClass()) {
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
    indexType N = a[0].getElementCount();
    if (cnt > N) {
        return a;
    }
    ArrayOfVector ret;
    ArrayOf ndx;
    bool vertflag = !(a[0].isRowVector());
    if (first_flag) {
        ndx = ArrayOf::integerRangeConstructor(1, 1, cnt, vertflag);
    } else {
        ndx = ArrayOf::integerRangeConstructor((N - cnt) + 1, 1, N, vertflag);
    }
    for (auto& i : a) {
        ret.push_back(i.getVectorSubset(ndx));
    }
    return ret;
}
//=============================================================================
ArrayOfVector
Find(const ArrayOfVector& argIn, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
    needToOverload = false;
    if (argIn[0].isSparse() || argIn[0].isReferenceType()) {
        needToOverload = true;
        return retval;
    }
    ArrayOf x(argIn[0]);
    switch (x.getDataClass()) {
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
        k = param2.getContentAsInteger32Scalar();
    }
    if (argIn.size() == 3) {
        std::wstring flag = argIn[2].getContentAsWideString();
        if (flag == L"first") {
            first_flag = true;
        } else if (flag == L"last") {
            first_flag = false;
        } else {
            Error(_W("Invalid third input argument. 'first' or 'last' expected."));
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
