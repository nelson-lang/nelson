//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ArrayOf.hpp"
#include <cmath>
#include <cstdio>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <utility>
#include <vector>
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
static void
checkScalar(const ArrayOf& value, double expected, const Dimensions& shape)
{
    const double actual = value.asDoubleScalar();
    require(std::memcmp(&actual, &expected, sizeof(double)) == 0, "Scalar bits changed");
    require(value.dataClass() == NLS_DOUBLE && value.isScalar() && !value.isSparse(),
        "Scalar class changed");
    require(value.dimensions().equals(shape) && value.nDims() == shape.length(),
        "Scalar dimensions changed");
    require(value.rows() == 1 && value.columns() == 1 && value.elementCount() == 1,
        "Scalar lengths changed");
    require(value.is2D() == shape.is2D() && value.isSquare() == shape.isSquare()
            && value.isVector() == shape.isVector() && value.isRowVector() == shape.isRowVector()
            && value.isColumnVector() == shape.isColumnVector(),
        "Scalar shape predicates changed");
    require(value.isDoubleType() == shape.is2D() && value.isNdArrayDoubleType() == !shape.is2D(),
        "Double shape class changed");
    require(value.nnz() == (expected != 0) && value.nzmax() == 1
            && value.isPositive() == !(expected < 0),
        "Scalar properties changed");
}
//=============================================================================
static void
checkOwnedTransfers()
{
    for (int iteration = 0; iteration < 10000; ++iteration) {
        auto* raw = new double[1];
        ArrayOf value(NLS_DOUBLE, Dimensions(1, 1), raw);
        *raw = 3.5;
        require(value.dataPointer() == raw && value.asDoubleScalar() == 3.5,
            "Owned buffer was read or replaced before initialization");
        ArrayOf moved(std::move(value));
        ArrayOf assigned;
        assigned = std::move(moved);
        require(assigned.dataPointer() == raw, "Moving replaced an owned buffer");
        ArrayOf copy(assigned);
        *static_cast<double*>(assigned.readWriteDataPointer()) = 8.;
        require(copy.asDoubleScalar() == 3.5 && assigned.asDoubleScalar() == 8.,
            "Copied scalar changed after a write");
        require(copy.referenceCount() == 1, "Copied scalar retained shared Data storage");
        ArrayOfVector indexes;
        indexes.push_back(ArrayOf::doubleScalar(1));
        indexes.push_back(ArrayOf::doubleScalar(1));
        require(assigned.nDimSubset(indexes).asDoubleScalar() == 8., "Scalar extraction changed");
        require(assigned.dataPointer() == raw, "Materialization replaced an owned buffer");
        ArrayOf materializedMove(std::move(assigned));
        require(materializedMove.dataPointer() == raw,
            "Moving materialized storage replaced an owned buffer");
        *raw = 9.;
        require(
            materializedMove.asDoubleScalar() == 9., "Transferred buffer stopped being writable");
    }
}
//=============================================================================
static void
checkShapesAndCopies()
{
    for (indexType rank : { indexType(1), indexType(2), indexType(3), indexType(maxDims) }) {
        Dimensions shape(rank);
        for (indexType k = 0; k < rank; ++k) {
            shape[k] = 1;
        }
        for (double expected : { 0., -0., 3.5, -2., std::numeric_limits<double>::infinity(),
                 -std::numeric_limits<double>::infinity(),
                 std::numeric_limits<double>::quiet_NaN() }) {
            ArrayOf copied(NLS_DOUBLE, shape, static_cast<const void*>(&expected), sizeof(double));
            checkScalar(copied, expected, shape);
            auto* raw = new double[1];
            ArrayOf owned(NLS_DOUBLE, shape, raw);
            *raw = expected;
            owned.name("scalar");
            checkScalar(owned, expected, shape);
            owned.changeInPlaceDimensions(shape);
            ArrayOf copy(owned);
            require(copy.referenceCount() == 1 && copy.name() == "scalar",
                "Materialized scalar copy lost its value semantics or name");
            checkScalar(copy, expected, shape);
            ArrayOf moved(std::move(owned));
            require(moved.referenceCount() == 1 && moved.name() == "scalar",
                "Materialized scalar move lost its value semantics or name");
            checkScalar(moved, expected, shape);
            ArrayOf assigned;
            assigned = copy;
            *static_cast<double*>(assigned.readWriteDataPointer()) = 19.;
            checkScalar(copy, expected, shape);
            copied = std::move(moved);
            checkScalar(copied, expected, shape);
        }
    }
}
//=============================================================================
static void
checkVectorCapacity()
{
    ArrayOf value;
    value.vectorResize(1);
    auto* raw = static_cast<double*>(value.readWriteDataPointer());
    *raw = 3.5;
    ArrayOf moved(std::move(value));
    require(moved.dataPointer() == raw, "Moving discarded a vector's reserved buffer");
    moved.vectorResize(2);
    require(moved.dataPointer() == raw && moved.asDoubleScalar(true) == 3.5,
        "Growing a moved vector discarded reserved capacity");
}
//=============================================================================
static void
checkScalarIndexes()
{
    for (ArrayOf index :
        { ArrayOf::doubleScalar(3), ArrayOf::singleScalar(3), ArrayOf::int64Scalar(3) }) {
        require(index.asIndexVector() == std::vector<indexType> { 3 },
            "Scalar index conversion changed");
        indexType* pointer = index.indexPointer();
        const indexType value = pointer[0];
        delete[] pointer;
        require(value == 3, "Scalar index pointer conversion changed");
    }
    ArrayOf index = ArrayOf::doubleScalar(1);
    index.makeSparse();
    index.toOrdinalType();
    require(index.isIntegerType() && index.asDoubleScalar() == 1,
        "Sparse scalar ordinal conversion changed");
}
//=============================================================================
static void
checkFactoriesAndConversions()
{
    const Dimensions scalar(1, 1);
    checkScalar(ArrayOf::doubleRowVector(std::vector<double> { -0. }), -0., scalar);
    checkScalar(ArrayOf::doubleRowVector(1), 0., scalar);
    checkScalar(ArrayOf::doubleMatrix2d(1, 1), 0., scalar);
    require(ArrayOf::doubleRowVector(0).isEmpty(), "Empty factory changed");
    require(ArrayOf::doubleMatrix2d(1, 2).elementCount() == 2, "Matrix factory changed");
    for (ArrayOf value :
        { ArrayOf::singleScalar(3.5), ArrayOf::int64Scalar(-7), ArrayOf::logicalScalar(true) }) {
        value.promoteType(NLS_DOUBLE);
        const double expected = value.asDoubleScalar();
        ArrayOf copy = value;
        require(value.referenceCount() == 1 && copy.referenceCount() == 1,
            "Conversion retained shared Data storage");
        auto* replacement = new double[1];
        *replacement = -0.;
        value.setDataPointer(replacement);
        require(value.dataPointer() == replacement, "Replacement buffer address changed");
        checkScalar(value, -0., scalar);
        checkScalar(copy, expected, scalar);
    }
    ArrayOf value = ArrayOf::doubleScalar(3.5);
    value.makeSparse();
    require(value.isSparse(), "Sparse scalar became dense");
    value.makeDense();
    checkScalar(value, 3.5, scalar);
    ArrayOf copy = value;
    require(copy.referenceCount() == 1, "Dense conversion retained shared Data storage");
    ArrayOf metadata(NLS_DOUBLE, scalar, new double[1] { 7. }, false, { "field" });
    ArrayOf metadataCopy(metadata);
    require(metadataCopy.fieldNames() == stringVector { "field" }, "Metadata was discarded");
    ArrayOf placeholder(NLS_DOUBLE, scalar, static_cast<void*>(nullptr));
    require(placeholder.dataPointer() == nullptr, "Null placeholder was initialized");
}
//=============================================================================
int
main()
{
    try {
        checkOwnedTransfers();
        checkShapesAndCopies();
        checkVectorCapacity();
        checkScalarIndexes();
        checkFactoriesAndConversions();
        std::printf("DOUBLE_SCALAR_STORAGE_OK size=%zu\n", sizeof(ArrayOf));
        return 0;
    } catch (const std::exception& error) {
        std::fprintf(stderr, "%s\n", error.what());
        return 1;
    }
}
//=============================================================================
