//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <algorithm>
#include <cstring>
#include "HorzCat.hpp"
#include "HorzCatSparseDouble.hpp"
#include "HorzCatSparseLogical.hpp"
#include "MatrixCheck.hpp"
#include "StructConcatenationFields.hpp"
#include "ConcatenateNdArray.hpp"
#include "ConcatenateDimensions.hpp"
#include "ConcatenateKeptOperand.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr indexType HORZCAT_AXIS = 1;
//=============================================================================
static ArrayOf
HorzCatFields(
    const ArrayOf& A, const ArrayOf& B, NelsonType classType, const stringVector& fieldnames);
static ArrayOf
HorzCatClassWithEmpty(const ArrayOf& A, const ArrayOf& B);
static ArrayOf
HorzCatOperands(const ArrayOfVector& argIn, NelsonType commonType);
//=============================================================================
template <class T>
ArrayOf
HorzCatTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
{
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    void* pRes = ArrayOf::allocateArrayOf(
        A.dataClass(), dimsRes.rows() * dimsRes.columns(), stringVector(), false);
    T* ptrC = (T*)pRes;
    const indexType countA = A.elementCount();
    const indexType countB = B.elementCount();
    std::memcpy(ptrC, ptrA, countA * sizeof(T));
    std::memcpy(ptrC + countA, ptrB, countB * sizeof(T));
    return ArrayOf(A.dataClass(), dimsRes, pRes);
}
//=============================================================================
template <class T>
ArrayOf
HorzCatComplexTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
{
    void* pRes
        = ArrayOf::allocateArrayOf(A.dataClass(), dimsRes.elementCount(), stringVector(), false);
    T* ptrC = (T*)pRes;
    const auto* ptrA = static_cast<const T*>(A.dataPointer());
    const auto* ptrB = static_cast<const T*>(B.dataPointer());
    const indexType countA = A.elementCount() * 2;
    const indexType countB = B.elementCount() * 2;
    std::memcpy(ptrC, ptrA, countA * sizeof(T));
    std::memcpy(ptrC + countA, ptrB, countB * sizeof(T));
    return ArrayOf(A.dataClass(), dimsRes, pRes);
}
//=============================================================================
static ArrayOf
HorzCatNdArray(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    ArrayOfMatrix m;
    ArrayOfVector v;
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    v.push_back(_A);
    v.push_back(_B);
    m.push_back(v);
    return ConcatenateNdArray(m, commonType);
}
//=============================================================================
static ArrayOf
HorzCat2DGeneric(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    ArrayOf res;
    indexType newColumnsSize = A.columns() + B.columns();
    indexType newRowsSize = A.rows();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    switch (commonType) {
    case NLS_LOGICAL: {
        res = HorzCatTemplate<logical>(_A, _B, dimsC);
    } break;
    case NLS_UINT8: {
        res = HorzCatTemplate<uint8>(_A, _B, dimsC);
    } break;
    case NLS_INT8: {
        res = HorzCatTemplate<int8>(_A, _B, dimsC);
    } break;
    case NLS_UINT16: {
        res = HorzCatTemplate<uint16>(_A, _B, dimsC);
    } break;
    case NLS_INT16: {
        res = HorzCatTemplate<int16>(_A, _B, dimsC);
    } break;
    case NLS_UINT32: {
        res = HorzCatTemplate<uint32>(_A, _B, dimsC);
    } break;
    case NLS_INT32: {
        res = HorzCatTemplate<int32>(_A, _B, dimsC);
    } break;
    case NLS_UINT64: {
        res = HorzCatTemplate<uint64>(_A, _B, dimsC);
    } break;
    case NLS_INT64: {
        res = HorzCatTemplate<int64>(_A, _B, dimsC);
    } break;
    case NLS_SINGLE: {
        res = HorzCatTemplate<single>(_A, _B, dimsC);
    } break;
    case NLS_DOUBLE: {
        res = HorzCatTemplate<double>(_A, _B, dimsC);
    } break;
    case NLS_SCOMPLEX: {
        res = HorzCatComplexTemplate<single>(_A, _B, dimsC);
    } break;
    case NLS_DCOMPLEX: {
        res = HorzCatComplexTemplate<double>(_A, _B, dimsC);
    } break;
    case NLS_CHAR: {
        res = HorzCatTemplate<charType>(_A, _B, dimsC);
    } break;
    default: {
        res = HorzCatNdArray(_A, _B, commonType);
    } break;
    }
    return res;
}
//=============================================================================
static void
checkDimensions(const ArrayOf& A, const ArrayOf& B)
{
    if (A.rows() == B.rows() && A.is2D() && B.is2D()) {
        return;
    }
    checkConcatenationDimensions(A.dimensions(), B.dimensions(), HORZCAT_AXIS);
}
//=============================================================================
static ArrayOf
HorzCatEmpty(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    Dimensions dimsC = concatenatedEmptyDimensions(A.dimensions(), B.dimensions(), HORZCAT_AXIS);
    if (commonType == NLS_STRUCT_ARRAY) {
        // the fields of the first struct that has some
        stringVector fieldnames = A.isStruct() ? A.fieldNames() : stringVector();
        if (fieldnames.empty() && B.isStruct()) {
            fieldnames = B.fieldNames();
        }
        return ArrayOf::emptyStruct(fieldnames, dimsC);
    }
    void* ptr = ArrayOf::allocateArrayOf(commonType, dimsC.elementCount());
    return ArrayOf(commonType, dimsC, ptr);
}
//=============================================================================
static ArrayOf
HorzCatClass(const ArrayOf& A, const ArrayOf& B)
{
    if (A.dataClass() != B.dataClass()) {
        Error("Nelson:operators:classMismatch", "Same class type expected.");
    }
    if (A.classType() != B.classType()) {
        Error("Nelson:operators:classMismatch", "Same class type expected.");
    }
    stringVector fieldnamesA = A.fieldNames();
    stringVector fieldnamesB = B.fieldNames();
    if (fieldnamesA.size() != fieldnamesB.size()) {
        Error(L"Nelson:operators:fieldnamesMustMatch", L"Fieldnames in structs must match.");
    }
    for (size_t k = 0; k < fieldnamesA.size(); k++) {
        if (fieldnamesA[k] != fieldnamesB[k]) {
            Error(L"Nelson:operators:fieldnamesMustMatch", L"Fieldnames in structs must match.");
        }
    }
    NelsonType classType = A.dataClass();
    ArrayOf res = HorzCatFields(A, B, classType, fieldnamesA);
    res.setClassType(A.classType());
    if (classType == NLS_CLASSDEF_ARRAY) {
        res.setClassdefLayout(A.classdefLayout());
    }
    return res;
}
//=============================================================================
static ArrayOf
HorzCatStruct(const ArrayOf& A, const ArrayOf& B)
{
    stringVector fieldnamesA = A.fieldNames();
    return HorzCatFields(A, matchStructFields(B, fieldnamesA), NLS_STRUCT_ARRAY, fieldnamesA);
}
//=============================================================================
static bool
isZeroByZero(const ArrayOf& value)
{
    return value.is2D() && value.rows() == 0 && value.columns() == 0;
}
//=============================================================================
static bool
isClassArrayType(NelsonType type)
{
    return type == NLS_CLASS_ARRAY || type == NLS_CLASSDEF_ARRAY;
}
//=============================================================================
static void
checkClassCompatibility(const ArrayOf& A, const ArrayOf& B)
{
    if (A.dataClass() != B.dataClass() || A.classType() != B.classType()) {
        Error("Nelson:operators:classMismatch", "Same class type expected.");
    }
    stringVector fieldnamesA = A.fieldNames();
    stringVector fieldnamesB = B.fieldNames();
    if (fieldnamesA.size() != fieldnamesB.size()) {
        Error(L"Nelson:operators:fieldnamesMustMatch", L"Fieldnames in structs must match.");
    }
    for (size_t k = 0; k < fieldnamesA.size(); k++) {
        if (fieldnamesA[k] != fieldnamesB[k]) {
            Error(L"Nelson:operators:fieldnamesMustMatch", L"Fieldnames in structs must match.");
        }
    }
}
//=============================================================================
static ArrayOf
emptyClassArrayLike(const ArrayOf& source, Dimensions& dimensions)
{
    ArrayOf result;
    if (source.dataClass() == NLS_CLASSDEF_ARRAY) {
        result = ArrayOf::emptyClassdef(source.classType(), source.fieldNames(), dimensions);
        result.setClassdefLayout(source.classdefLayout());
    } else {
        result = ArrayOf::emptyClass(source.classType(), source.fieldNames(), dimensions);
    }
    return result;
}
//=============================================================================
ArrayOf
HorzCat(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    if (isClassArrayType(commonType) && (A.isEmpty(false) || B.isEmpty(false))) {
        return HorzCatClassWithEmpty(A, B);
    }
    if (A.isEmpty(false) && B.isEmpty(false)) {
        return HorzCatEmpty(A, B, commonType);
    }
    if (A.isEmpty(false)) {
        checkEmptyOperand(A, B, HORZCAT_AXIS);
        if (A.isCell() && commonType != NLS_STRUCT_ARRAY) {
            if (B.isCell()) {
                ArrayOf C(B);
                return C;
            }
            return ArrayOf::cellArrayFrom(B);
        }
        return concatenateKeptOperand(B, commonType);
    }
    if (B.isEmpty(false)) {
        checkEmptyOperand(B, A, HORZCAT_AXIS);
        if (B.isCell() && commonType != NLS_STRUCT_ARRAY) {
            if (A.isCell()) {
                ArrayOf C(A);
                return C;
            }
            return ArrayOf::cellArrayFrom(A);
        }
        return concatenateKeptOperand(A, commonType);
    }

    checkDimensions(A, B);

    if (commonType == NLS_STRUCT_ARRAY) {
        return HorzCatStruct(A, B);
    }
    if (commonType == NLS_CLASS_ARRAY || commonType == NLS_CLASSDEF_ARRAY) {
        return HorzCatClass(A, B);
    }
    if (A.is2D() && B.is2D()) {
        return HorzCat2DGeneric(A, B, commonType);
    }
    return HorzCatNdArray(A, B, commonType);
}
//=============================================================================
// Three or more empty operands take the size of their concatenation as a
// whole; the pairwise fold only settles their class. Otherwise the pairwise
// fold must not hide an N-D operand that disagrees with an empty one.
ArrayOf
HorzCat(const ArrayOfVector& argIn, NelsonType commonType)
{
    if (argIn.size() > 2) {
        Dimensions dims;
        if (allEmptyOperandsDimensions(argIn, HORZCAT_AXIS, dims)) {
            ArrayOf res = HorzCatOperands(argIn, commonType);
            res.reshape(dims);
            return res;
        }
        checkNdOperandsDimensions(argIn, HORZCAT_AXIS);
    }
    return HorzCatOperands(argIn, commonType);
}
//=============================================================================
template <class T>
static ArrayOf
HorzCatContiguousTemplate(
    const ArrayOfVector& argIn, NelsonType commonType, const Dimensions& dimsC)
{
    // Column-major storage: horizontal concatenation of same-type operands is their
    // data buffers copied one after another.
    void* pRes = ArrayOf::allocateArrayOf(
        commonType, dimsC.rows() * dimsC.columns(), stringVector(), false);
    T* dst = (T*)pRes;
    for (const auto& arg : argIn) {
        ArrayOf a(arg);
        a.promoteType(commonType);
        indexType cnt = a.elementCount();
        std::memcpy(dst, a.dataPointer(), cnt * sizeof(T));
        dst += cnt;
    }
    return ArrayOf(commonType, dimsC, pRes);
}
//=============================================================================
static ArrayOf
HorzCatSlotsContiguous(const ArrayOfVector& argIn, NelsonType commonType, const Dimensions& dimsC,
    indexType slotsPerElement, const stringVector& fieldNames)
{
    // String, cell and struct arrays store ArrayOf slots, not POD data: copy each slot
    // by ArrayOf assignment, not memcpy. One pass keeps [a1, ..., aN] linear (the pairwise
    // fold recopies the accumulated result each step). Slots are element-major (a struct
    // has slotsPerElement fields per element).
    void* pRes
        = ArrayOf::allocateArrayOf(commonType, dimsC.rows() * dimsC.columns(), fieldNames, false);
    ArrayOf* dst = static_cast<ArrayOf*>(pRes);
    indexType k = 0;
    for (const auto& arg : argIn) {
        ArrayOf a(arg);
        const ArrayOf* src = static_cast<const ArrayOf*>(a.dataPointer());
        indexType cnt = a.elementCount() * slotsPerElement;
        for (indexType i = 0; i < cnt; i++) {
            dst[k++] = src[i];
        }
    }
    return ArrayOf(commonType, dimsC, pRes, false, fieldNames);
}
//=============================================================================
static bool
tryHorzCatContiguous(const ArrayOfVector& argIn, NelsonType commonType, ArrayOf& result)
{
    // Fast path for three or more plain real, non-empty, 2-D operands with equal row
    // counts: one pass instead of the pairwise fold, which is O(n^2).
    switch (commonType) {
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
    case NLS_CHAR:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
        break;
    default:
        return false;
    }
    const bool slotType = commonType == NLS_STRING_ARRAY || commonType == NLS_CELL_ARRAY
        || commonType == NLS_STRUCT_ARRAY;
    stringVector structFields;
    if (commonType == NLS_STRUCT_ARRAY) {
        structFields = argIn[0].fieldNames();
    }
    indexType rowsC = argIn[0].rows();
    indexType colsC = 0;
    for (const auto& arg : argIn) {
        if (!arg.is2D() || arg.isComplex() || arg.isSparse() || arg.isEmpty(false)
            || arg.rows() != rowsC) {
            return false;
        }
        if (slotType && arg.dataClass() != commonType) {
            // Only take the fast path when every operand already has the result
            // type; a char operand promoted to string would change its element
            // count, so leave such mixes to the general path.
            return false;
        }
        if (commonType == NLS_STRUCT_ARRAY) {
            // The one-pass copy appends element slots verbatim, so it is only
            // valid when every struct has the same fields in the same order;
            // reordering is left to the general path (matchStructFields).
            stringVector f = arg.fieldNames();
            if (f.size() != structFields.size()) {
                return false;
            }
            for (size_t j = 0; j < f.size(); j++) {
                if (f[j] != structFields[j]) {
                    return false;
                }
            }
        }
        colsC += arg.columns();
    }
    Dimensions dimsC(rowsC, colsC);
    switch (commonType) {
    case NLS_LOGICAL:
        result = HorzCatContiguousTemplate<logical>(argIn, commonType, dimsC);
        break;
    case NLS_UINT8:
        result = HorzCatContiguousTemplate<uint8>(argIn, commonType, dimsC);
        break;
    case NLS_INT8:
        result = HorzCatContiguousTemplate<int8>(argIn, commonType, dimsC);
        break;
    case NLS_UINT16:
        result = HorzCatContiguousTemplate<uint16>(argIn, commonType, dimsC);
        break;
    case NLS_INT16:
        result = HorzCatContiguousTemplate<int16>(argIn, commonType, dimsC);
        break;
    case NLS_UINT32:
        result = HorzCatContiguousTemplate<uint32>(argIn, commonType, dimsC);
        break;
    case NLS_INT32:
        result = HorzCatContiguousTemplate<int32>(argIn, commonType, dimsC);
        break;
    case NLS_UINT64:
        result = HorzCatContiguousTemplate<uint64>(argIn, commonType, dimsC);
        break;
    case NLS_INT64:
        result = HorzCatContiguousTemplate<int64>(argIn, commonType, dimsC);
        break;
    case NLS_SINGLE:
        result = HorzCatContiguousTemplate<single>(argIn, commonType, dimsC);
        break;
    case NLS_DOUBLE:
        result = HorzCatContiguousTemplate<double>(argIn, commonType, dimsC);
        break;
    case NLS_CHAR:
        result = HorzCatContiguousTemplate<charType>(argIn, commonType, dimsC);
        break;
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
        result = HorzCatSlotsContiguous(argIn, commonType, dimsC, 1, stringVector());
        break;
    case NLS_STRUCT_ARRAY:
        result = HorzCatSlotsContiguous(
            argIn, commonType, dimsC, (indexType)structFields.size(), structFields);
        break;
    default:
        return false;
    }
    return true;
}
//=============================================================================
static ArrayOf
HorzCatOperands(const ArrayOfVector& argIn, NelsonType commonType)
{
    if (argIn.size() == 0) {
        Dimensions dims(0, 0);
        return ArrayOf::empty(dims);
    }
    if (argIn.size() == 1) {
        return argIn[0];
    }
    bool isSparse = false;
    bool isComplex = false;
    for (const auto& arg : argIn) {
        isSparse = isSparse || arg.isSparse();
        isComplex = isComplex || arg.isComplex();
    }
    if (isSparse) {
        ArrayOfVector _argIn(argIn);
        if (commonType == NLS_LOGICAL) {
            for (auto& arg : _argIn) {
                arg.promoteType(NLS_LOGICAL);
                arg.makeSparse();
            }
            ArrayOf res = _argIn[0];
            for (size_t k = 1; k < _argIn.size(); k++) {
                res = HorzCatSparseLogical(res, _argIn[k]);
            }
            return res;
        }
        NelsonType sparseType = NLS_DOUBLE;
        if (commonType == NLS_SINGLE) {
            sparseType = isComplex ? NLS_SCOMPLEX : NLS_SINGLE;
        } else {
            sparseType = isComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
        }
        for (auto& arg : _argIn) {
            arg.promoteType(sparseType);
            arg.makeSparse();
        }
        ArrayOf res = _argIn[0];
        for (size_t k = 1; k < _argIn.size(); k++) {
            res = HorzCatSparseDouble(res, _argIn[k]);
        }
        return res;
    }
    if (argIn.size() == 2) {
        return HorzCat(argIn[0], argIn[1], commonType);
    }
    ArrayOf fast;
    if (tryHorzCatContiguous(argIn, commonType, fast)) {
        return fast;
    }
    ArrayOf res = argIn[0];
    for (size_t k = 1; k < argIn.size(); k++) {
        res = HorzCat(res, argIn[k], commonType);
    }
    return res;
}
//=============================================================================
static ArrayOf
HorzCatFields(
    const ArrayOf& A, const ArrayOf& B, NelsonType classType, const stringVector& fieldnames)
{
    ArrayOfMatrix m;
    ArrayOfVector v;
    v.push_back(A);
    v.push_back(B);
    m.push_back(v);
    return ConcatenateNdArray(m, classType, fieldnames);
}
//=============================================================================
static ArrayOf
HorzCatClassWithEmpty(const ArrayOf& A, const ArrayOf& B)
{
    if (isZeroByZero(A)) {
        if (B.isClassType() && A.isClassType() && A.classType() != B.classType()) {
            Error("Nelson:operators:classMismatch", "Same class type expected.");
        }
        return ArrayOf(B);
    }
    if (isZeroByZero(B)) {
        if (A.isClassType() && B.isClassType() && A.classType() != B.classType()) {
            Error("Nelson:operators:classMismatch", "Same class type expected.");
        }
        return ArrayOf(A);
    }
    checkClassCompatibility(A, B);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (A.isEmpty(false) && B.isEmpty(false)) {
        Dimensions dimsC = concatenatedEmptyDimensions(dimsA, dimsB, HORZCAT_AXIS);
        return emptyClassArrayLike(A, dimsC);
    }
    if (A.isEmpty(false)) {
        checkEmptyOperandDimensions(dimsA, dimsB, HORZCAT_AXIS);
        return ArrayOf(B);
    }
    checkEmptyOperandDimensions(dimsB, dimsA, HORZCAT_AXIS);
    return ArrayOf(A);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
