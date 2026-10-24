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
#include <cstring>
#include <vector>
#include "VertCat.hpp"
#include "VertCatSparseDouble.hpp"
#include "VertCatSparseLogical.hpp"
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
static constexpr indexType VERTCAT_AXIS = 0;
//=============================================================================
static ArrayOf
VertCatFields(
    const ArrayOf& A, const ArrayOf& B, NelsonType classType, const stringVector& fieldnames);
static ArrayOf
VertCatClassWithEmpty(const ArrayOf& A, const ArrayOf& B);
static ArrayOf
VertCatOperands(const ArrayOfVector& argIn, NelsonType commonType);
//=============================================================================
template <class T>
ArrayOf
VertCatTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
{
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    void* pRes = ArrayOf::allocateArrayOf(
        A.dataClass(), dimsRes.rows() * dimsRes.columns(), stringVector(), false);
    T* ptrC = (T*)pRes;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        ptrA, dimsA.rows(), dimsA.columns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        ptrB, dimsB.rows(), dimsB.columns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC(
        ptrC, dimsRes.rows(), dimsRes.columns());
    matC << matA, matB;
    return ArrayOf(A.dataClass(), dimsRes, pRes);
}
//=============================================================================
template <class T>
ArrayOf
VertCatComplexTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
{
    void* pRes = ArrayOf::allocateArrayOf(
        A.dataClass(), dimsRes.elementCount() * 2, stringVector(), false);
    T* ptrC = (T*)pRes;
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        Cz, dimsRes.rows(), dimsRes.columns());
    Dimensions dimsA = A.dimensions();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, dimsA.rows(), dimsA.columns());
    Dimensions dimsB = B.dimensions();
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)B.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
        Bz, dimsB.rows(), dimsB.columns());
    matC << matA, matB;
    return ArrayOf(A.dataClass(), dimsRes, pRes);
}
//=============================================================================
static ArrayOf
VertCatNdArray(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    ArrayOfMatrix m;
    ArrayOfVector v;
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    v.push_back(_A);
    m.push_back(v);
    v.clear();
    v.push_back(_B);
    m.push_back(v);
    return ConcatenateNdArray(m, commonType);
}
//=============================================================================
static ArrayOf
VertCat2DGeneric(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    ArrayOf res;
    indexType newColumnsSize = A.columns();
    indexType newRowsSize = A.rows() + B.rows();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    switch (commonType) {
    case NLS_LOGICAL: {
        res = VertCatTemplate<logical>(_A, _B, dimsC);
    } break;
    case NLS_UINT8: {
        res = VertCatTemplate<uint8>(_A, _B, dimsC);
    } break;
    case NLS_INT8: {
        res = VertCatTemplate<int8>(_A, _B, dimsC);
    } break;
    case NLS_UINT16: {
        res = VertCatTemplate<uint16>(_A, _B, dimsC);
    } break;
    case NLS_INT16: {
        res = VertCatTemplate<int16>(_A, _B, dimsC);
    } break;
    case NLS_UINT32: {
        res = VertCatTemplate<uint32>(_A, _B, dimsC);
    } break;
    case NLS_INT32: {
        res = VertCatTemplate<int32>(_A, _B, dimsC);
    } break;
    case NLS_UINT64: {
        res = VertCatTemplate<uint64>(_A, _B, dimsC);
    } break;
    case NLS_INT64: {
        res = VertCatTemplate<int64>(_A, _B, dimsC);
    } break;
    case NLS_SINGLE: {
        res = VertCatTemplate<single>(_A, _B, dimsC);
    } break;
    case NLS_DOUBLE: {
        res = VertCatTemplate<double>(_A, _B, dimsC);
    } break;
    case NLS_SCOMPLEX: {
        res = VertCatComplexTemplate<single>(_A, _B, dimsC);
    } break;
    case NLS_DCOMPLEX: {
        res = VertCatComplexTemplate<double>(_A, _B, dimsC);
    } break;
    case NLS_CHAR: {
        res = VertCatTemplate<charType>(_A, _B, dimsC);
    } break;
    default: {
        res = VertCatNdArray(_A, _B, commonType);
    } break;
    }
    return res;
}
//=============================================================================
static void
checkDimensions(const ArrayOf& A, const ArrayOf& B)
{
    if (A.columns() == B.columns() && A.is2D() && B.is2D()) {
        return;
    }
    checkConcatenationDimensions(A.dimensions(), B.dimensions(), VERTCAT_AXIS);
}
//=============================================================================
static ArrayOf
VertCatEmpty(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    Dimensions dimsC = concatenatedEmptyDimensions(A.dimensions(), B.dimensions(), VERTCAT_AXIS);
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
VertCatClass(const ArrayOf& A, const ArrayOf& B)
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
    ArrayOf res = VertCatFields(A, B, classType, fieldnamesA);
    res.setClassType(A.classType());
    if (classType == NLS_CLASSDEF_ARRAY) {
        res.setClassdefLayout(A.classdefLayout());
    }
    return res;
}
//=============================================================================
static ArrayOf
VertCatStruct(const ArrayOf& A, const ArrayOf& B)
{
    stringVector fieldnamesA = A.fieldNames();
    return VertCatFields(A, matchStructFields(B, fieldnamesA), NLS_STRUCT_ARRAY, fieldnamesA);
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
VertCat(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    if (isClassArrayType(commonType) && (A.isEmpty(false) || B.isEmpty(false))) {
        return VertCatClassWithEmpty(A, B);
    }
    if (A.isEmpty(false) && B.isEmpty(false)) {
        return VertCatEmpty(A, B, commonType);
    }
    if (A.isEmpty(false)) {
        checkEmptyOperand(A, B, VERTCAT_AXIS);
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
        checkEmptyOperand(B, A, VERTCAT_AXIS);
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
        return VertCatStruct(A, B);
    }
    if (commonType == NLS_CLASS_ARRAY || commonType == NLS_CLASSDEF_ARRAY) {
        return VertCatClass(A, B);
    }
    if (A.is2D() && B.is2D()) {
        return VertCat2DGeneric(A, B, commonType);
    }
    return VertCatNdArray(A, B, commonType);
}
//=============================================================================
// Three or more empty operands take the size of their concatenation as a
// whole; the pairwise fold only settles their class. Otherwise the pairwise
// fold must not hide an N-D operand that disagrees with an empty one.
ArrayOf
VertCat(const ArrayOfVector& argIn, NelsonType commonType)
{
    if (argIn.size() > 2) {
        Dimensions dims;
        if (allEmptyOperandsDimensions(argIn, VERTCAT_AXIS, dims)) {
            ArrayOf res = VertCatOperands(argIn, commonType);
            res.reshape(dims);
            return res;
        }
        checkNdOperandsDimensions(argIn, VERTCAT_AXIS);
    }
    return VertCatOperands(argIn, commonType);
}
//=============================================================================
template <class T>
static ArrayOf
VertCatContiguousTemplate(
    const ArrayOfVector& argIn, NelsonType commonType, const Dimensions& dimsC)
{
    // Column-major storage: result column c is the operands' column c stacked. Each
    // operand column is copied into place in one allocation.
    indexType totalRows = dimsC.rows();
    indexType cols = dimsC.columns();
    void* pRes = ArrayOf::allocateArrayOf(commonType, totalRows * cols, stringVector(), false);
    T* dst = (T*)pRes;
    std::vector<ArrayOf> promoted;
    promoted.reserve(argIn.size());
    for (const auto& arg : argIn) {
        ArrayOf a(arg);
        a.promoteType(commonType);
        promoted.push_back(a);
    }
    for (indexType c = 0; c < cols; ++c) {
        T* colDst = dst + c * totalRows;
        indexType rowOff = 0;
        for (const auto& a : promoted) {
            indexType r = a.rows();
            const T* src = (const T*)a.dataPointer();
            std::memcpy(colDst + rowOff, src + c * r, r * sizeof(T));
            rowOff += r;
        }
    }
    return ArrayOf(commonType, dimsC, pRes);
}
//=============================================================================
static ArrayOf
VertCatSlotsContiguous(const ArrayOfVector& argIn, NelsonType commonType, const Dimensions& dimsC,
    indexType slotsPerElement, const stringVector& fieldNames)
{
    // String, cell and struct arrays store ArrayOf slots, not POD data: copy each slot
    // by ArrayOf assignment, not memcpy. Same column-major interleave as the numeric
    // template; a struct element occupies slotsPerElement consecutive slots.
    indexType totalRows = dimsC.rows();
    indexType cols = dimsC.columns();
    void* pRes = ArrayOf::allocateArrayOf(commonType, totalRows * cols, fieldNames, false);
    ArrayOf* dst = static_cast<ArrayOf*>(pRes);
    std::vector<ArrayOf> kept;
    kept.reserve(argIn.size());
    for (const auto& arg : argIn) {
        kept.push_back(ArrayOf(arg));
    }
    for (indexType c = 0; c < cols; ++c) {
        indexType rowOff = 0;
        for (const auto& a : kept) {
            indexType r = a.rows();
            const ArrayOf* src = static_cast<const ArrayOf*>(a.dataPointer());
            for (indexType row = 0; row < r; ++row) {
                indexType dstElem = c * totalRows + rowOff + row;
                indexType srcElem = c * r + row;
                for (indexType j = 0; j < slotsPerElement; ++j) {
                    dst[dstElem * slotsPerElement + j] = src[srcElem * slotsPerElement + j];
                }
            }
            rowOff += r;
        }
    }
    return ArrayOf(commonType, dimsC, pRes, false, fieldNames);
}
//=============================================================================
static bool
tryVertCatContiguous(const ArrayOfVector& argIn, NelsonType commonType, ArrayOf& result)
{
    // Fast path for three or more plain real, non-empty, 2-D operands with equal column
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
    indexType colsC = argIn[0].columns();
    indexType rowsC = 0;
    for (const auto& arg : argIn) {
        if (!arg.is2D() || arg.isComplex() || arg.isSparse() || arg.isEmpty(false)
            || arg.columns() != colsC) {
            return false;
        }
        if (slotType && arg.dataClass() != commonType) {
            return false;
        }
        if (commonType == NLS_STRUCT_ARRAY) {
            // One-pass copy appends element slots verbatim: only valid when every
            // struct has the same fields in the same order (reordering is left to
            // the general path).
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
        rowsC += arg.rows();
    }
    Dimensions dimsC(rowsC, colsC);
    switch (commonType) {
    case NLS_LOGICAL:
        result = VertCatContiguousTemplate<logical>(argIn, commonType, dimsC);
        break;
    case NLS_UINT8:
        result = VertCatContiguousTemplate<uint8>(argIn, commonType, dimsC);
        break;
    case NLS_INT8:
        result = VertCatContiguousTemplate<int8>(argIn, commonType, dimsC);
        break;
    case NLS_UINT16:
        result = VertCatContiguousTemplate<uint16>(argIn, commonType, dimsC);
        break;
    case NLS_INT16:
        result = VertCatContiguousTemplate<int16>(argIn, commonType, dimsC);
        break;
    case NLS_UINT32:
        result = VertCatContiguousTemplate<uint32>(argIn, commonType, dimsC);
        break;
    case NLS_INT32:
        result = VertCatContiguousTemplate<int32>(argIn, commonType, dimsC);
        break;
    case NLS_UINT64:
        result = VertCatContiguousTemplate<uint64>(argIn, commonType, dimsC);
        break;
    case NLS_INT64:
        result = VertCatContiguousTemplate<int64>(argIn, commonType, dimsC);
        break;
    case NLS_SINGLE:
        result = VertCatContiguousTemplate<single>(argIn, commonType, dimsC);
        break;
    case NLS_DOUBLE:
        result = VertCatContiguousTemplate<double>(argIn, commonType, dimsC);
        break;
    case NLS_CHAR:
        result = VertCatContiguousTemplate<charType>(argIn, commonType, dimsC);
        break;
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
        result = VertCatSlotsContiguous(argIn, commonType, dimsC, 1, stringVector());
        break;
    case NLS_STRUCT_ARRAY:
        result = VertCatSlotsContiguous(
            argIn, commonType, dimsC, (indexType)structFields.size(), structFields);
        break;
    default:
        return false;
    }
    return true;
}
//=============================================================================
static ArrayOf
VertCatOperands(const ArrayOfVector& argIn, NelsonType commonType)
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
                res = VertCatSparseLogical(res, _argIn[k]);
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
            res = VertCatSparseDouble(res, _argIn[k]);
        }
        return res;
    }
    if (argIn.size() == 2) {
        return VertCat(argIn[0], argIn[1], commonType);
    }
    ArrayOf fast;
    if (tryVertCatContiguous(argIn, commonType, fast)) {
        return fast;
    }
    ArrayOf res = argIn[0];
    for (size_t k = 1; k < argIn.size(); k++) {
        res = VertCat(res, argIn[k], commonType);
    }
    return res;
}
//=============================================================================
static ArrayOf
VertCatFields(
    const ArrayOf& A, const ArrayOf& B, NelsonType classType, const stringVector& fieldnames)
{
    ArrayOfMatrix m;
    ArrayOfVector v;
    v.push_back(A);
    m.push_back(v);
    v.clear();
    v.push_back(B);
    m.push_back(v);
    return ConcatenateNdArray(m, classType, fieldnames);
}
//=============================================================================
static ArrayOf
VertCatClassWithEmpty(const ArrayOf& A, const ArrayOf& B)
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
        Dimensions dimsC = concatenatedEmptyDimensions(dimsA, dimsB, VERTCAT_AXIS);
        return emptyClassArrayLike(A, dimsC);
    }
    if (A.isEmpty(false)) {
        checkEmptyOperandDimensions(dimsA, dimsB, VERTCAT_AXIS);
        return ArrayOf(B);
    }
    checkEmptyOperandDimensions(dimsB, dimsA, VERTCAT_AXIS);
    return ArrayOf(A);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
