//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ArrayOf.hpp"
#include <cstdio>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <utility>
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
require(bool condition, const char* message)
{
    if (!condition) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
template <class T>
static void
checkScalar(const ArrayOf& value, NelsonType type, T expected, const Dimensions& shape)
{
    require(
        value.dataClass() == type && value.isScalar() && !value.isSparse(), "Scalar class changed");
    require(std::memcmp(value.dataPointer(), &expected, sizeof(T)) == 0, "Scalar bits changed");
    require(value.elementSize() == sizeof(T) && value.byteSize() == sizeof(T),
        "Scalar byte size changed");
    require(value.dimensions().equals(shape) && value.nDims() == shape.length(),
        "Scalar dimensions changed");
    require(value.isLogical() == (type == NLS_LOGICAL) && !value.isDoubleClass()
            && value.isSingleClass() == (type == NLS_SINGLE),
        "Scalar class predicates changed");
    require(value.isNdArrayLogical() == (type == NLS_LOGICAL && !shape.is2D()),
        "Logical shape predicate changed");
    require(value.isSingleType() == (type == NLS_SINGLE && shape.is2D())
            && value.isNdArraySingleType() == (type == NLS_SINGLE && !shape.is2D()),
        "Single shape predicate changed");
    require(value.is2D() == shape.is2D() && value.isVector() == shape.isVector()
            && value.isRowVector() == shape.isRowVector()
            && value.isColumnVector() == shape.isColumnVector(),
        "Scalar shape predicates changed");
    require(value.nnz() == (expected != 0) && value.nzmax() == 1, "Scalar counts changed");
    require(value.isNumeric() == (type != NLS_LOGICAL) && value.isReal() && value.allReal(),
        "Scalar numeric predicates changed");
}
//=============================================================================
template <class T>
static void
checkTransfers(NelsonType type, T expected)
{
    const Dimensions scalar(1, 1);
    for (int iteration = 0; iteration < 10000; ++iteration) {
        T* raw = new T[1];
        ArrayOf source(type, scalar, raw);
        *raw = expected;
        require(source.dataPointer() == raw, "Construction replaced an uninitialized buffer");
        ArrayOf moved(std::move(source));
        ArrayOf value;
        value = std::move(moved);
        require(value.dataPointer() == raw, "Moving replaced the buffer");
        ArrayOf copy(value);
        require(copy.referenceCount() == 1, "Scalar copy retained a shared Data block");
        *static_cast<T*>(value.readWriteDataPointer()) = T(0);
        checkScalar(copy, type, expected, scalar);
        value.changeInPlaceDimensions(scalar);
        require(value.dataPointer() == raw, "Materialization replaced the buffer");
        ArrayOf materializedMove(std::move(value));
        require(
            materializedMove.dataPointer() == raw, "Moving materialized storage lost the buffer");
        *raw = expected;
        checkScalar(materializedMove, type, expected, scalar);
        ArrayOf copiedMaterialized(materializedMove);
        require(copiedMaterialized.referenceCount() == 1, "Materialized copy was not compacted");
        T* replacement = new T[1];
        copiedMaterialized.setDataPointer(replacement);
        *replacement = expected;
        require(copiedMaterialized.dataPointer() == replacement, "Replacement buffer changed");
        checkScalar(copiedMaterialized, type, expected, scalar);
    }
}
//=============================================================================
template <class T>
static void
checkShapes(NelsonType type, T expected)
{
    for (indexType rank : { indexType(1), indexType(2), indexType(3), indexType(maxDims) }) {
        Dimensions shape(rank);
        for (indexType k = 0; k < rank; ++k) {
            shape[k] = 1;
        }
        ArrayOf value(type, shape, static_cast<const void*>(&expected), sizeof(T));
        checkScalar(value, type, expected, shape);
        value.name("scalar");
        ArrayOf copied(value);
        require(copied.referenceCount() == 1 && copied.name() == "scalar", "Copy lost its name");
        checkScalar(copied, type, expected, shape);
        value.changeInPlaceDimensions(shape);
        ArrayOf materializedCopy(value);
        checkScalar(materializedCopy, type, expected, shape);
        ArrayOf moved(std::move(value));
        checkScalar(moved, type, expected, shape);
        ArrayOf assigned;
        assigned = std::move(moved);
        checkScalar(assigned, type, expected, shape);
        T elements[2] = {};
        assigned.copyElements(0, elements, 1, 1);
        require(
            std::memcmp(elements + 1, &expected, sizeof(T)) == 0, "Element copying changed bits");
        ArrayOf extracted = assigned.valueAtIndex(0);
        checkScalar(extracted, type, expected, Dimensions(1, 1));
    }
}
//=============================================================================
template <class T>
static void
checkFallbacks(NelsonType type, T expected)
{
    ArrayOf metadata(type, Dimensions(1, 1), new T[1] { expected }, false, { "field" });
    ArrayOf copy(metadata);
    require(copy.fieldNames() == stringVector { "field" }, "Metadata was discarded");
    ArrayOf placeholder(type, Dimensions(1, 1), static_cast<void*>(nullptr));
    require(placeholder.dataPointer() == nullptr, "Null placeholder was read");
    ArrayOf value(type);
    value.vectorResize(1);
    const void* reserved = value.dataPointer();
    ArrayOf moved(std::move(value));
    require(moved.dataPointer() == reserved, "Moving reserved storage replaced the buffer");
    moved.vectorResize(2);
    require(moved.dataPointer() == reserved && moved.elementCount() == 2,
        "Moving discarded reserved vector capacity");
}
//=============================================================================
static void
checkPromotionCapabilities(NelsonType type)
{
    ArrayOf compact
        = type == NLS_LOGICAL ? ArrayOf::logicalScalar(true) : ArrayOf::singleScalar(1.f);
    ArrayOf reference(type, Dimensions(1, 2), ArrayOf::allocateArrayOf(type, 2, {}, true));
    for (NelsonType destination : { NLS_DOUBLE, NLS_SINGLE, NLS_LOGICAL, NLS_DCOMPLEX, NLS_SCOMPLEX,
             NLS_INT8, NLS_INT64, NLS_UINT64, NLS_CHAR, NLS_CELL_ARRAY, NLS_STRUCT_ARRAY,
             NLS_STRING_ARRAY, NLS_MISSING_ARRAY, NLS_HANDLE, NLS_UNKNOWN }) {
        require(compact.canBePromotedTo(destination) == reference.canBePromotedTo(destination),
            "Compact scalar changed the promotion capability");
    }
}
//=============================================================================
static void
checkLogical()
{
    for (logical expected : { logical(0), logical(1), logical(2), logical(255) }) {
        checkShapes(NLS_LOGICAL, expected);
    }
    checkTransfers(NLS_LOGICAL, logical(1));
    checkFallbacks(NLS_LOGICAL, logical(1));
    checkPromotionCapabilities(NLS_LOGICAL);
    ArrayOf value = ArrayOf::logicalScalar(true);
    require(value.asLogicalScalar() == 1 && !value.isIntegerValue() && !value.isPositive(),
        "Logical scalar conversion or predicates changed");
    value.makeSparse();
    require(value.isSparseLogicalType(), "Logical sparse conversion changed class");
    value.makeDense();
    require(value.isLogical() && value.asLogicalScalar() == 1 && !value.isSparse(),
        "Logical dense conversion changed class");
    ArrayOf copy(value);
    require(copy.referenceCount() == 1, "Dense logical copy retained a shared Data block");
}
//=============================================================================
static void
checkSingle()
{
    for (single expected : { 0.f, -0.f, 3.5f, -2.f, std::numeric_limits<single>::infinity(),
             -std::numeric_limits<single>::infinity(), std::numeric_limits<single>::quiet_NaN(),
             std::numeric_limits<single>::denorm_min(), std::numeric_limits<single>::max() }) {
        checkShapes(NLS_SINGLE, expected);
        ArrayOf value = ArrayOf::singleScalar(expected);
        const single actual = value.asSingleScalar();
        require(
            std::memcmp(&actual, &expected, sizeof(single)) == 0, "Single extraction changed bits");
        require(value.isPositive() == !(expected < 0), "Single sign predicate changed");
    }
    checkTransfers(NLS_SINGLE, 3.5f);
    checkFallbacks(NLS_SINGLE, 3.5f);
    checkPromotionCapabilities(NLS_SINGLE);
    checkScalar(ArrayOf::singleMatrix2d(1, 1), NLS_SINGLE, 0.f, Dimensions(1, 1));
    checkScalar(ArrayOf::singleRowVector(1), NLS_SINGLE, 0.f, Dimensions(1, 1));
    checkScalar(
        ArrayOf::singleRowVector(std::vector<single> { -0.f }), NLS_SINGLE, -0.f, Dimensions(1, 1));
    require(ArrayOf::singleRowVector(0).isEmpty(), "Empty single factory changed");
    require(ArrayOf::singleMatrix2d(1, 2).elementCount() == 2, "Single matrix factory changed");
    ArrayOf value = ArrayOf::singleScalar(3);
    require(
        value.isIntegerValue() && value.asScalarIndex() == 3, "Single integer conversion changed");
    require(value.asIndexVector() == std::vector<indexType> { 3 }, "Single index vector changed");
    value = ArrayOf::singleScalar(3.5f);
    require(!value.isIntegerValue(), "Fractional single was classified as an integer");
    value.promoteType(NLS_DOUBLE);
    require(value.dataClass() == NLS_DOUBLE && value.asDoubleScalar() == 3.5,
        "Single to double promotion changed");
    value.promoteType(NLS_LOGICAL);
    require(
        value.isLogical() && value.asLogicalScalar() == 1, "Single to logical promotion changed");
    value.promoteType(NLS_SINGLE);
    checkScalar(value, NLS_SINGLE, 1.f, Dimensions(1, 1));
}
//=============================================================================
template <class T>
static void
checkMatrixCopies(NelsonType type, T expected)
{
    ArrayOf matrix(type, Dimensions(2, 2), new T[4] { expected, 0, 0, expected });
    matrix.name("matrix");
    for (int iteration = 0; iteration < 1000; ++iteration) {
        ArrayOf copy(matrix);
        require(copy.dataPointer() == matrix.dataPointer() && copy.referenceCount() == 2,
            "Matrix copy lost shared storage");
        require(copy.dataClass() == type && copy.name() == "matrix"
                && copy.dimensions().equals(Dimensions(2, 2)),
            "Matrix copy lost its properties");
        *static_cast<T*>(copy.readWriteDataPointer()) = 0;
        require(*static_cast<const T*>(matrix.dataPointer()) == expected,
            "Matrix write changed its source");
        copy = ArrayOf::logicalScalar(true);
        copy = matrix;
        require(copy.dataPointer() == matrix.dataPointer() && copy.dataClass() == type,
            "Scalar to matrix assignment changed storage or class");
        copy = ArrayOf::singleScalar(-2.f);
        copy.name("replacement");
        copy.setValue(matrix);
        require(copy.dataPointer() == matrix.dataPointer() && copy.dataClass() == type,
            "Replacing a scalar with a matrix changed storage or class");
        ArrayOf moved(std::move(copy));
        copy = matrix;
        require(copy.dataPointer() == matrix.dataPointer() && moved.name() == "replacement",
            "Reusing a moved value lost matrix storage or name");
        copy = ArrayOf::doubleScalar(3.5);
        require(copy.asDoubleScalar() == 3.5, "Matrix to scalar assignment changed value");
    }
}
//=============================================================================
int
main()
{
    try {
        checkLogical();
        checkSingle();
        checkMatrixCopies(NLS_DOUBLE, 3.5);
        checkMatrixCopies(NLS_SINGLE, 3.5f);
        checkMatrixCopies(NLS_LOGICAL, logical(255));
        std::printf("REAL_SCALAR_STORAGE_OK size=%zu\n", sizeof(ArrayOf));
        return 0;
    } catch (const std::exception& error) {
        std::fprintf(stderr, "%s\n", error.what());
        return 1;
    }
}
//=============================================================================
