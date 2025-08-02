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
#include "HorzCat.hpp"
#include "MatrixCheck.hpp"
#include "ConcatenateNdArray.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
HorzCatTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
{
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    void* pRes = ArrayOf::allocateArrayOf(
        A.getDataClass(), dimsRes.getRows() * dimsRes.getColumns(), stringVector(), false);
    T* ptrC = (T*)pRes;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        ptrA, dimsA.getRows(), dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        ptrB, dimsB.getRows(), dimsB.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC(
        ptrC, dimsRes.getRows(), dimsRes.getColumns());
    matC << matA, matB;
    return ArrayOf(A.getDataClass(), dimsRes, pRes);
}
//=============================================================================
template <class T>
ArrayOf
HorzCatComplexTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
{
    void* pRes = ArrayOf::allocateArrayOf(
        A.getDataClass(), dimsRes.getElementCount() * 2, stringVector(), false);
    T* ptrC = (T*)pRes;
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        Cz, dimsRes.getRows(), dimsRes.getColumns());
    Dimensions dimsA = A.getDimensions();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, dimsA.getRows(), dimsA.getColumns());
    Dimensions dimsB = B.getDimensions();
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)B.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
        Bz, dimsB.getRows(), dimsB.getColumns());
    matC << matA, matB;
    return ArrayOf(A.getDataClass(), dimsRes, pRes);
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
    indexType newColumnsSize = A.getColumns() + B.getColumns();
    indexType newRowsSize = A.getRows();
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
    if (A.getRows() != B.getRows()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (dimsA.getLength() != dimsB.getLength()) {
            Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
        }
        for (indexType k = 0; k < dimsA.getLength(); k++) {
            if (k != 1) {
                if (dimsA.getDimensionLength(k) != dimsB.getDimensionLength(k)) {
                    Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
                }
            }
        }
    }
}
//=============================================================================
static ArrayOf
HorzCatEmpty(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{
    Dimensions dimsC;
    indexType m = 0;
    indexType n = 0;
    if (B.getRows() != A.getRows() || (!A.is2D() || !B.is2D())) {
        dimsC = Dimensions(m, n);
    } else {
        m = 0;
        n = A.getColumns() + B.getColumns();
        dimsC = Dimensions(m, n);
    }
    void* ptr = ArrayOf::allocateArrayOf(commonType, dimsC.getElementCount());
    return ArrayOf(commonType, dimsC, ptr);
}
//=============================================================================
static ArrayOf
HorzCatClass(const ArrayOf& A, const ArrayOf& B)
{
    if (A.getClassType() != B.getClassType()) {
        Error(_("Same class type expected."));
    }
    stringVector fieldnamesA = A.getFieldNames();
    indexType newColumnsSize = A.getColumns() + B.getColumns();
    indexType newRowsSize = A.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    void* ptrC = ArrayOf::allocateArrayOf(NLS_CLASS_ARRAY, newSize, fieldnamesA, false);
    auto* elements = static_cast<ArrayOf*>(ptrC);
    ArrayOf res = ArrayOf(NLS_CLASS_ARRAY, dimsC, elements, false, fieldnamesA);
    for (auto& k : fieldnamesA) {
        ArrayOfVector fieldsA = A.getFieldAsList(k);
        ArrayOfVector fieldsB = B.getFieldAsList(k);
        ArrayOfVector fieldsC(fieldsA);
        fieldsC += fieldsB;
        res.setFieldAsList(k, fieldsC);
    }

    res.setClassType(A.getClassType());
    return res;
}
//=============================================================================
static ArrayOf
HorzCatStruct(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    stringVector fieldnamesA = A.getFieldNames();
    stringVector fieldnamesB = B.getFieldNames();
    if (fieldnamesA.size() != fieldnamesB.size()) {
        Error(ERROR_FIELDNAMES_MUST_MATCH);
    }
    for (size_t k = 0; k < fieldnamesA.size(); k++) {
        if (fieldnamesA[k] != fieldnamesB[k]) {
            Error(ERROR_FIELDNAMES_MUST_MATCH);
        }
    }
    indexType newColumnsSize = A.getColumns() + B.getColumns();
    indexType newRowsSize = A.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    void* ptrC = ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, newSize, fieldnamesA, false);
    auto* elements = static_cast<ArrayOf*>(ptrC);
    res = ArrayOf(NLS_STRUCT_ARRAY, dimsC, elements, false, fieldnamesA);
    for (auto& k : fieldnamesA) {
        ArrayOfVector fieldsA = A.getFieldAsList(k);
        ArrayOfVector fieldsB = B.getFieldAsList(k);
        ArrayOfVector fieldsC(fieldsA);
        fieldsC += fieldsB;
        res.setFieldAsList(k, fieldsC);
    }
    return res;
}
//=============================================================================
ArrayOf
HorzCat(const ArrayOf& A, const ArrayOf& B, NelsonType commonType)
{

    if (A.isEmpty(false) && B.isEmpty(false)) {
        return HorzCatEmpty(A, B, commonType);
    }
    if (A.isEmpty(false)) {
        if (A.isCell()) {
            if (B.isCell()) {
                ArrayOf C(B);
                return C;
            }
            return ArrayOf::toCell(B);
        }
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        if (B.isCell()) {
            if (A.isCell()) {
                ArrayOf C(A);
                return C;
            }
            return ArrayOf::toCell(A);
        }
        ArrayOf C(A);
        return C;
    }

    checkDimensions(A, B);

    if (commonType == NLS_STRUCT_ARRAY) {
        return HorzCatStruct(A, B);
    }
    if (commonType == NLS_CLASS_ARRAY) {
        return HorzCatClass(A, B);
    }
    if (A.is2D() && B.is2D()) {
        return HorzCat2DGeneric(A, B, commonType);
    }
    return HorzCatNdArray(A, B, commonType);
}
//=============================================================================
ArrayOf
HorzCat(const ArrayOfVector& argIn, NelsonType commonType)
{
    if (argIn.size() == 0) {
        Dimensions dims(0, 0);
        return ArrayOf::emptyConstructor(dims);
    }
    if (argIn.size() == 1) {
        return argIn[0];
    }
    if (argIn.size() == 2) {
        return HorzCat(argIn[0], argIn[1], commonType);
    }
    ArrayOf res = argIn[0];
    for (size_t k = 1; k < argIn.size(); k++) {
        res = HorzCat(res, argIn[k], commonType);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
