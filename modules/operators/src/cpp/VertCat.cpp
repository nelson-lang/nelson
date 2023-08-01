//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "VertCat.hpp"
#include "ClassName.hpp"
#include "MatrixCheck.hpp"
#include "FindConcatenateClass.hpp"
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
VertCatTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
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
VertCatComplexTemplate(const ArrayOf& A, const ArrayOf& B, Dimensions& dimsRes)
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
ArrayOf
VertCat(ArrayOf& A, ArrayOf& B, bool mustRaiseError, bool& bSuccess)
{
    bSuccess = false;
    if (A.isSparse() || B.isSparse()) {
        if (mustRaiseError) {
            std::string overload = ClassName(A) + "_vertcat_" + ClassName(B);
            Error(_("function") + " " + overload + " " + _("undefined."));
        } else {
            bSuccess = false;
            return {};
        }
    }

    if (A.isEmpty(false) && B.isEmpty(false)) {
        bool needOverload;
        NelsonType classCommon = FindConcatenateClass(A, B, needOverload);
        if (A.isStringArray() || B.isStringArray()) {
            classCommon = NLS_STRING_ARRAY;
        } else if (A.isCell() || B.isCell()) {
            classCommon = NLS_CELL_ARRAY;
        } else {
            if (A.isStruct() && B.isStruct()) {
                classCommon = A.getDataClass();
            }
            if (A.isIntegerType()) {
                classCommon = A.getDataClass();
            } else {
                classCommon = FindConcatenateClass(A, B, needOverload);
            }
        }
        void* ptr = nullptr;
        Dimensions dimsC;
        if (B.getColumns() != A.getColumns()) {
            indexType m = 0;
            indexType n = 0;
            dimsC = Dimensions(m, n);
            ptr = ArrayOf::allocateArrayOf(classCommon, dimsC.getElementCount());
        } else {
            indexType m = A.getRows() + B.getRows();
            indexType n = 0;
            dimsC = Dimensions(m, n);
            ptr = ArrayOf::allocateArrayOf(classCommon, dimsC.getElementCount());
        }
        bSuccess = true;
        return ArrayOf(classCommon, dimsC, ptr);
    }
    if (A.isEmpty(false)) {
        bSuccess = true;
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
        bSuccess = true;
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
    bool needOverload;
    NelsonType classCommon = FindConcatenateClass(A, B, needOverload);
    if (A.isStringArray() || B.isStringArray()) {
        classCommon = NLS_STRING_ARRAY;
    } else if (A.isCell() || B.isCell()) {
        classCommon = NLS_CELL_ARRAY;
    } else {
        if (A.isStruct() && B.isStruct()) {
            classCommon = A.getDataClass();
        }
        if (A.isIntegerType()) {
            classCommon = A.getDataClass();
        } else {
            classCommon = FindConcatenateClass(A, B, needOverload);
        }
    }
    try {
        if (classCommon == NLS_STRING_ARRAY) {
            if (!A.isStringArray()) {
                bool needToOverload;
                A = ArrayOf::toStringArray(A, needToOverload);
                if (needToOverload) {
                    Error(_W("Cannot promote to string array."));
                }
            }
            if (!B.isStringArray()) {
                bool needToOverload;
                B = ArrayOf::toStringArray(B, needToOverload);
                if (needToOverload) {
                    Error(_W("Cannot promote to string array."));
                }
            }
        } else if (classCommon == NLS_CELL_ARRAY) {
            if (!A.isCell()) {
                A = ArrayOf::toCell(A);
            }
            if (!B.isCell()) {
                B = ArrayOf::toCell(B);
            }
        } else {
            if (classCommon != NLS_STRUCT_ARRAY) {
                A.promoteType(classCommon);
                B.promoteType(classCommon);
            }
        }
    } catch (const Exception&) {
        if (mustRaiseError) {
            throw;
        }
        bSuccess = false;
        return {};
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getColumns() != dimsB.getColumns()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    if (dimsA.getLength() != dimsB.getLength()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    for (indexType k = 0; k < dimsA.getLength(); k++) {
        if (k != 0) {
            if (dimsA.getDimensionLength(k) != dimsB.getDimensionLength(k)) {
                Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
            }
        }
    }
    ArrayOf res;
    if (classCommon == NLS_CLASS_ARRAY) {
        if (A.getClassType() != B.getClassType()) {
            Error(_("Same class type expected."));
        }
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
        indexType newColumnsSize = dimsA.getColumns();
        indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
        indexType newSize = newColumnsSize * newRowsSize;
        Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
        void* ptrC = ArrayOf::allocateArrayOf(NLS_CLASS_ARRAY, newSize, fieldnamesA, false);
        auto* elements = static_cast<ArrayOf*>(ptrC);
        res = ArrayOf(NLS_CLASS_ARRAY, dimsC, elements, false, fieldnamesA);
        for (auto& k : fieldnamesA) {
            ArrayOfVector fieldsA = A.getFieldAsList(k);
            ArrayOfVector fieldsB = B.getFieldAsList(k);
            ArrayOfVector fieldsC(fieldsA);
            fieldsC += fieldsB;
            res.setFieldAsList(k, fieldsC);
        }
        bSuccess = true;
        res.setClassType(A.getClassType());
        return res;
    }
    if (classCommon == NLS_STRUCT_ARRAY) {
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
        indexType newColumnsSize = dimsA.getColumns();
        indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
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
        bSuccess = true;
        return res;
    }
    if (A.is2D() && B.is2D()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        indexType newColumnsSize = dimsA.getColumns();
        indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
        Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);

        switch (A.getDataClass()) {
        case NLS_LOGICAL: {
            res = VertCatTemplate<logical>(A, B, dimsC);
        } break;
        case NLS_UINT8: {
            res = VertCatTemplate<uint8>(A, B, dimsC);
        } break;
        case NLS_INT8: {
            res = VertCatTemplate<int8>(A, B, dimsC);
        } break;
        case NLS_UINT16: {
            res = VertCatTemplate<uint16>(A, B, dimsC);
        } break;
        case NLS_INT16: {
            res = VertCatTemplate<int16>(A, B, dimsC);
        } break;
        case NLS_UINT32: {
            res = VertCatTemplate<uint32>(A, B, dimsC);
        } break;
        case NLS_INT32: {
            res = VertCatTemplate<int32>(A, B, dimsC);
        } break;
        case NLS_UINT64: {
            res = VertCatTemplate<uint64>(A, B, dimsC);
        } break;
        case NLS_INT64: {
            res = VertCatTemplate<int64>(A, B, dimsC);
        } break;
        case NLS_SINGLE: {
            res = VertCatTemplate<single>(A, B, dimsC);
        } break;
        case NLS_DOUBLE: {
            res = VertCatTemplate<double>(A, B, dimsC);
        } break;
        case NLS_SCOMPLEX: {
            res = VertCatComplexTemplate<single>(A, B, dimsC);
        } break;
        case NLS_DCOMPLEX: {
            res = VertCatComplexTemplate<double>(A, B, dimsC);
        } break;
        case NLS_CHAR: {
            res = VertCatTemplate<charType>(A, B, dimsC);
        } break;
        default: {
            ArrayOfMatrix m;
            ArrayOfVector v;
            v.push_back(A);
            m.push_back(v);
            v.clear();
            v.push_back(B);
            m.push_back(v);
            res = ConcatenateNdArray(m, classCommon);
        } break;
        }
    } else {
        ArrayOfMatrix m;
        ArrayOfVector v;
        v.push_back(A);
        m.push_back(v);
        v.clear();
        v.push_back(B);
        m.push_back(v);
        res = ConcatenateNdArray(m, classCommon);
    }
    bSuccess = true;
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
