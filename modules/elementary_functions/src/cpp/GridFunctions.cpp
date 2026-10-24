//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Sparse>
#include <algorithm>
#include <cstring>
#include <vector>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GridFunctions.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isGridInputSupported(const ArrayOf& value);
static ArrayOf
columnVector(ArrayOf value);
static Dimensions
gridDimensions(const std::vector<indexType>& lengths);
static ArrayOf
emptyGridLike(const ArrayOf& value, const Dimensions& dimensions);
static ArrayOf
makeGrid(const ArrayOf& value, const Dimensions& dimensions, indexType axis);
static ArrayOf
makeGrid2D(const ArrayOf& value, const Dimensions& dimensions, indexType axis);
template <class T>
static ArrayOf
makeDenseGrid(const ArrayOf& value, const Dimensions& dimensions, indexType axis);
template <class T>
static ArrayOf
makeDenseGrid2D(const ArrayOf& value, const Dimensions& dimensions, indexType axis);
template <class T>
static ArrayOf
makeSparseInputGrid(const ArrayOf& value, const Dimensions& dimensions, indexType axis);
template <class T>
static ArrayOf
makeSparseInputGrid2D(const ArrayOf& value, const Dimensions& dimensions, indexType axis);
static ArrayOfVector
meshgrid2D(int nLhs, const ArrayOfVector& argIn);
static ArrayOfVector
meshgrid3D(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
Meshgrid(int nLhs, const ArrayOfVector& argIn)
{
    bool useTwoDimensionalGrid = (argIn.size() == 1 && nLhs < 3) || argIn.size() == 2;
    if (useTwoDimensionalGrid) {
        return meshgrid2D(nLhs, argIn);
    }
    return meshgrid3D(nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Ndgrid(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() == 1 && nLhs < 2) {
        ArrayOfVector retval;
        retval << columnVector(argIn[0]);
        return retval;
    }

    size_t outputCount = std::max<size_t>(static_cast<size_t>(std::max(nLhs, 1)), argIn.size());
    std::vector<indexType> lengths(outputCount);
    for (size_t k = 0; k < outputCount; ++k) {
        size_t inputIndex = argIn.size() == 1 ? 0 : k;
        lengths[k] = argIn[inputIndex].elementCount();
    }
    Dimensions dimensions = gridDimensions(lengths);
    ArrayOfVector retval;
    for (size_t k = 0; k < static_cast<size_t>(std::max(nLhs, 1)); ++k) {
        size_t inputIndex = argIn.size() == 1 ? 0 : k;
        retval << makeGrid(argIn[inputIndex], dimensions, static_cast<indexType>(k));
    }
    return retval;
}
//=============================================================================
static bool
isGridInputSupported(const ArrayOf& value)
{
    return value.isNumeric() || value.isLogical() || value.isCharacterArray();
}
//=============================================================================
static ArrayOf
columnVector(ArrayOf value)
{
    if (!isGridInputSupported(value)) {
        Error(L"Nelson:grid:typeNotSupported", L"Type not supported.");
    }
    Dimensions dimensions(value.elementCount(), 1);
    value.reshape(dimensions);
    return value;
}
//=============================================================================
static Dimensions
gridDimensions(const std::vector<indexType>& lengths)
{
    Dimensions dimensions(static_cast<indexType>(lengths.size()));
    for (indexType k = 0; k < static_cast<indexType>(lengths.size()); ++k) {
        dimensions[k] = lengths[k];
    }
    dimensions.simplify();
    return dimensions;
}
//=============================================================================
static ArrayOf
emptyGridLike(const ArrayOf& value, const Dimensions& dimensions)
{
    return ArrayOf(value.dataClass(), dimensions, nullptr, false);
}
//=============================================================================
static ArrayOf
makeGrid(const ArrayOf& value, const Dimensions& dimensions, indexType axis)
{
    if (!isGridInputSupported(value)) {
        Error(L"Nelson:grid:typeNotSupported", L"Type not supported.");
    }
    if (dimensions.elementCount() == 0) {
        return emptyGridLike(value, dimensions);
    }
    if (dimensions.is2D()) {
        return makeGrid2D(value, dimensions, axis);
    }
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        return value.isSparse() ? makeSparseInputGrid<logical>(value, dimensions, axis)
                                : makeDenseGrid<logical>(value, dimensions, axis);
    case NLS_UINT8:
        return makeDenseGrid<uint8>(value, dimensions, axis);
    case NLS_INT8:
        return makeDenseGrid<int8>(value, dimensions, axis);
    case NLS_UINT16:
        return makeDenseGrid<uint16>(value, dimensions, axis);
    case NLS_INT16:
        return makeDenseGrid<int16>(value, dimensions, axis);
    case NLS_UINT32:
        return makeDenseGrid<uint32>(value, dimensions, axis);
    case NLS_INT32:
        return makeDenseGrid<int32>(value, dimensions, axis);
    case NLS_UINT64:
        return makeDenseGrid<uint64>(value, dimensions, axis);
    case NLS_INT64:
        return makeDenseGrid<int64>(value, dimensions, axis);
    case NLS_SINGLE:
        return value.isSparse() ? makeSparseInputGrid<single>(value, dimensions, axis)
                                : makeDenseGrid<single>(value, dimensions, axis);
    case NLS_DOUBLE:
        return value.isSparse() ? makeSparseInputGrid<double>(value, dimensions, axis)
                                : makeDenseGrid<double>(value, dimensions, axis);
    case NLS_SCOMPLEX:
        return value.isSparse() ? makeSparseInputGrid<singlecomplex>(value, dimensions, axis)
                                : makeDenseGrid<singlecomplex>(value, dimensions, axis);
    case NLS_DCOMPLEX:
        return value.isSparse() ? makeSparseInputGrid<doublecomplex>(value, dimensions, axis)
                                : makeDenseGrid<doublecomplex>(value, dimensions, axis);
    case NLS_CHAR:
        return makeDenseGrid<charType>(value, dimensions, axis);
    default:
        Error(L"Nelson:grid:typeNotSupported", L"Type not supported.");
    }
    return {};
}
//=============================================================================
static ArrayOf
makeGrid2D(const ArrayOf& value, const Dimensions& dimensions, indexType axis)
{
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        return value.isSparse() ? makeSparseInputGrid2D<logical>(value, dimensions, axis)
                                : makeDenseGrid2D<logical>(value, dimensions, axis);
    case NLS_UINT8:
        return makeDenseGrid2D<uint8>(value, dimensions, axis);
    case NLS_INT8:
        return makeDenseGrid2D<int8>(value, dimensions, axis);
    case NLS_UINT16:
        return makeDenseGrid2D<uint16>(value, dimensions, axis);
    case NLS_INT16:
        return makeDenseGrid2D<int16>(value, dimensions, axis);
    case NLS_UINT32:
        return makeDenseGrid2D<uint32>(value, dimensions, axis);
    case NLS_INT32:
        return makeDenseGrid2D<int32>(value, dimensions, axis);
    case NLS_UINT64:
        return makeDenseGrid2D<uint64>(value, dimensions, axis);
    case NLS_INT64:
        return makeDenseGrid2D<int64>(value, dimensions, axis);
    case NLS_SINGLE:
        return value.isSparse() ? makeSparseInputGrid2D<single>(value, dimensions, axis)
                                : makeDenseGrid2D<single>(value, dimensions, axis);
    case NLS_DOUBLE:
        return value.isSparse() ? makeSparseInputGrid2D<double>(value, dimensions, axis)
                                : makeDenseGrid2D<double>(value, dimensions, axis);
    case NLS_SCOMPLEX:
        return value.isSparse() ? makeSparseInputGrid2D<singlecomplex>(value, dimensions, axis)
                                : makeDenseGrid2D<singlecomplex>(value, dimensions, axis);
    case NLS_DCOMPLEX:
        return value.isSparse() ? makeSparseInputGrid2D<doublecomplex>(value, dimensions, axis)
                                : makeDenseGrid2D<doublecomplex>(value, dimensions, axis);
    case NLS_CHAR:
        return makeDenseGrid2D<charType>(value, dimensions, axis);
    default:
        Error(L"Nelson:grid:typeNotSupported", L"Type not supported.");
    }
    return {};
}
//=============================================================================
template <class T>
static ArrayOf
makeDenseGrid(const ArrayOf& value, const Dimensions& dimensions, indexType axis)
{
    T* destination
        = static_cast<T*>(ArrayOf::allocateArrayOf(value.dataClass(), dimensions.elementCount()));
    const T* source = static_cast<const T*>(value.dataPointer());
    indexType stride = 1;
    for (indexType k = 0; k < axis; ++k) {
        stride *= dimensions.dimensionLength(k);
    }
    ompIndexType elementCount = static_cast<ompIndexType>(dimensions.elementCount());
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; ++k) {
        indexType coordinate = (static_cast<indexType>(k) / stride) % value.elementCount();
        destination[k] = source[coordinate];
    }
    return ArrayOf(value.dataClass(), dimensions, destination, false);
}
//=============================================================================
template <class T>
static ArrayOf
makeDenseGrid2D(const ArrayOf& value, const Dimensions& dimensions, indexType axis)
{
    T* destination
        = static_cast<T*>(ArrayOf::allocateArrayOf(value.dataClass(), dimensions.elementCount()));
    const T* source = static_cast<const T*>(value.dataPointer());
    indexType rows = dimensions.rows();
    indexType columns = dimensions.columns();
    if (axis == 0) {
        size_t columnBytes = static_cast<size_t>(rows) * sizeof(T);
        OMP_PARALLEL_FOR_LOOP(columns)
        for (ompIndexType column = 0; column < static_cast<ompIndexType>(columns); ++column) {
            std::memcpy(destination + static_cast<size_t>(column) * rows, source, columnBytes);
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(columns)
        for (ompIndexType column = 0; column < static_cast<ompIndexType>(columns); ++column) {
            T valueAtColumn = source[column];
            T* columnStart = destination + static_cast<size_t>(column) * rows;
            for (indexType row = 0; row < rows; ++row) {
                columnStart[row] = valueAtColumn;
            }
        }
    }
    return ArrayOf(value.dataClass(), dimensions, destination, false);
}
//=============================================================================
template <class T>
static ArrayOf
makeSparseInputGrid(const ArrayOf& value, const Dimensions& dimensions, indexType axis)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    T* destination
        = static_cast<T*>(ArrayOf::allocateArrayOf(value.dataClass(), dimensions.elementCount()));
    const auto* source = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    indexType stride = 1;
    for (indexType k = 0; k < axis; ++k) {
        stride *= dimensions.dimensionLength(k);
    }
    signedIndexType rows = static_cast<signedIndexType>(value.rows());
    ompIndexType elementCount = static_cast<ompIndexType>(dimensions.elementCount());
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; ++k) {
        indexType coordinate = (static_cast<indexType>(k) / stride) % value.elementCount();
        signedIndexType row = static_cast<signedIndexType>(coordinate % value.rows());
        signedIndexType column = static_cast<signedIndexType>(coordinate) / rows;
        destination[k] = source->coeff(row, column);
    }
    return ArrayOf(value.dataClass(), dimensions, destination, false);
}
//=============================================================================
template <class T>
static ArrayOf
makeSparseInputGrid2D(const ArrayOf& value, const Dimensions& dimensions, indexType axis)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    T* destination
        = static_cast<T*>(ArrayOf::allocateArrayOf(value.dataClass(), dimensions.elementCount()));
    const auto* source = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    indexType rows = dimensions.rows();
    indexType columns = dimensions.columns();
    signedIndexType sourceRows = static_cast<signedIndexType>(value.rows());
    OMP_PARALLEL_FOR_LOOP(columns)
    for (ompIndexType column = 0; column < static_cast<ompIndexType>(columns); ++column) {
        T* columnStart = destination + static_cast<size_t>(column) * rows;
        for (indexType row = 0; row < rows; ++row) {
            indexType coordinate = axis == 0 ? row : static_cast<indexType>(column);
            signedIndexType sourceRow = static_cast<signedIndexType>(coordinate % value.rows());
            signedIndexType sourceColumn = static_cast<signedIndexType>(coordinate) / sourceRows;
            columnStart[row] = source->coeff(sourceRow, sourceColumn);
        }
    }
    return ArrayOf(value.dataClass(), dimensions, destination, false);
}
//=============================================================================
static ArrayOfVector
meshgrid2D(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf x = argIn[0];
    ArrayOf y = argIn.size() == 1 ? argIn[0] : argIn[1];
    ArrayOfVector retval;
    if (x.isEmpty() || y.isEmpty()) {
        Dimensions emptyDimensions(0, 0);
        retval << emptyGridLike(x, emptyDimensions);
        if (nLhs > 1) {
            retval << emptyGridLike(y, emptyDimensions);
        }
        return retval;
    }
    Dimensions dimensions(y.elementCount(), x.elementCount());
    retval << makeGrid(x, dimensions, 1);
    if (nLhs > 1) {
        retval << makeGrid(y, dimensions, 0);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
meshgrid3D(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf x = argIn[0];
    ArrayOf y = argIn.size() == 1 ? argIn[0] : argIn[1];
    ArrayOf z = argIn.size() == 1 ? argIn[0] : argIn[2];
    ArrayOfVector retval;
    int outputCount = std::max(nLhs, 1);
    if (x.isEmpty() || y.isEmpty() || z.isEmpty()) {
        Dimensions emptyDimensions(0, 0);
        retval << emptyGridLike(x, emptyDimensions);
        if (outputCount > 1) {
            retval << emptyGridLike(y, emptyDimensions);
        }
        if (outputCount > 2) {
            retval << emptyGridLike(z, emptyDimensions);
        }
        return retval;
    }
    std::vector<indexType> lengths = { y.elementCount(), x.elementCount(), z.elementCount() };
    Dimensions dimensions = gridDimensions(lengths);
    retval << makeGrid(x, dimensions, 1);
    if (outputCount > 1) {
        retval << makeGrid(y, dimensions, 0);
    }
    if (outputCount > 2) {
        retval << makeGrid(z, dimensions, 2);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
