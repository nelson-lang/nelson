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
#include "mat2strBuiltin.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "MatrixToString.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <Eigen/Sparse>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static ArrayOf
doubleColumnVectorFromVector(const std::vector<T>& values)
{
    Dimensions dims(values.size(), 1);
    auto* data = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size());
    for (indexType k = 0; k < (indexType)values.size(); ++k) {
        data[k] = (double)values[k];
    }
    return ArrayOf(NLS_DOUBLE, dims, data);
}
//=============================================================================
template <class T>
static ArrayOf
typedColumnVectorFromVector(const std::vector<T>& values, NelsonType className)
{
    Dimensions dims(values.size(), 1);
    auto* data = (T*)ArrayOf::allocateArrayOf(className, values.size());
    for (indexType k = 0; k < (indexType)values.size(); ++k) {
        data[k] = values[k];
    }
    return ArrayOf(className, dims, data);
}
//=============================================================================
static ArrayOf
complexColumnVectorFromVector(const std::vector<doublecomplex>& values)
{
    Dimensions dims(values.size(), 1);
    auto* data = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, values.size());
    auto* complexData = reinterpret_cast<doublecomplex*>(data);
    for (indexType k = 0; k < (indexType)values.size(); ++k) {
        complexData[k] = values[k];
    }
    return ArrayOf(NLS_DCOMPLEX, dims, data);
}
//=============================================================================
template <class T>
static void
sparseCoordinatesAndValues(const ArrayOf& A, std::vector<indexType>& rows,
    std::vector<indexType>& columns, std::vector<T>& values)
{
    const auto* spmat
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    rows.reserve(spmat->nonZeros());
    columns.reserve(spmat->nonZeros());
    values.reserve(spmat->nonZeros());
    for (indexType k = 0; k < (indexType)spmat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmat, k); it;
            ++it) {
            rows.push_back((indexType)it.row() + 1);
            columns.push_back((indexType)it.col() + 1);
            values.push_back(it.value());
        }
    }
}
//=============================================================================
static std::wstring
sparseMatrixToString(const ArrayOf& A, indexType precision, bool withClass)
{
    std::vector<indexType> rows;
    std::vector<indexType> columns;
    std::wstring valuesString;
    indexType nzmax = 0;
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        std::vector<logical> values;
        sparseCoordinatesAndValues<logical>(A, rows, columns, values);
        nzmax = values.size();
        valuesString = MatrixToString(
            typedColumnVectorFromVector<logical>(values, NLS_LOGICAL), precision, withClass);
    } break;
    case NLS_DOUBLE: {
        std::vector<double> values;
        sparseCoordinatesAndValues<double>(A, rows, columns, values);
        nzmax = values.size();
        valuesString = MatrixToString(
            typedColumnVectorFromVector<double>(values, NLS_DOUBLE), precision, withClass);
    } break;
    case NLS_DCOMPLEX: {
        std::vector<doublecomplex> values;
        sparseCoordinatesAndValues<doublecomplex>(A, rows, columns, values);
        nzmax = values.size();
        valuesString = MatrixToString(complexColumnVectorFromVector(values), precision, withClass);
    } break;
    default:
        Error(L"Nelson:mat2str:typeNotSupported", L"Type not supported.");
    }
    ArrayOf rowsVector = doubleColumnVectorFromVector<indexType>(rows);
    ArrayOf columnsVector = doubleColumnVectorFromVector<indexType>(columns);
    if (A.elementCount() == 0) {
        return L"sparse([], [], [], " + std::to_wstring(A.rows()) + L", "
            + std::to_wstring(A.columns()) + L")";
    }
    if (A.rows() == 1) {
        // a sparse row keeps row-shaped index and value vectors
        Dimensions rowDims(1, static_cast<indexType>(nzmax));
        rowsVector.reshape(rowDims);
        columnsVector.reshape(rowDims);
        StringHelpers::replace_all(valuesString, L";", L" ");
        StringHelpers::replace_all(valuesString, L"zeros(0,1)", L"zeros(1,0)");
    }
    std::wstring rowsString = MatrixToString(rowsVector, 15, false);
    std::wstring columnsString = MatrixToString(columnsVector, 15, false);
    return L"sparse(" + rowsString + L", " + columnsString + L", " + valuesString + L", "
        + std::to_wstring(A.rows()) + L", " + std::to_wstring(A.columns()) + L")";
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::mat2strBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    // mat2str(A [, precision] [, 'class']): the two options come in any order
    indexType defaultPrecision = 15;
    bool withClass = false;
    for (size_t k = 1; k < argIn.size(); ++k) {
        const ArrayOf& param = argIn[k];
        if (param.isRowVectorCharacterArray() || (param.isStringArray() && param.isScalar())) {
            if (StringHelpers::to_lower_copy(param.asWideString()) != L"class") {
                Error(L"Nelson:string:classArgumentExpected", L"'%s' expected.", L"class");
            }
            withClass = true;
        } else if (param.isNumeric() && !param.isScalar()) {
            Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
        } else if (param.isNumeric() && !param.isComplex()) {
            double precision = param.asDoubleScalar();
            if (!std::isfinite(precision) || precision < 1 || precision != std::floor(precision)) {
                Error(L"Nelson:string:secondInputArgumentRealPositive",
                    L"Second input argument must be a real positive integers.");
            }
            defaultPrecision = static_cast<indexType>(precision);
        } else {
            Error(L"Nelson:string:secondInputArgumentRealPositive",
                L"Second input argument must be a real positive integers.");
        }
    }
    ArrayOf A = argIn[0];
    bool canBeConvert = A.isNumeric() || A.isLogical() || A.isCharacterArray() || A.isStringArray();
    if (!canBeConvert) {
        Error(L"Nelson:string:inputMatrixNumericArrayCharacter",
            L"Input matrix must be a numeric array, character array, or string array.");
    }
    if (A.isSparse()) {
        retval << ArrayOf::characterArray(sparseMatrixToString(A, defaultPrecision, withClass));
        return retval;
    }
    std::wstring res = MatrixToString(A, defaultPrecision, withClass);
    retval << ArrayOf::characterArray(res);
    return retval;
}
//=============================================================================
