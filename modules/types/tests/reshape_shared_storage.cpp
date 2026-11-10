//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ArrayOf.hpp"
#include <cstdio>
#include <stdexcept>
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
static ArrayOf
doubleRange(indexType rows, indexType columns)
{
    const indexType count = rows * columns;
    auto* values = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, count));
    for (indexType k = 0; k < count; ++k) {
        values[k] = static_cast<double>(k + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(rows, columns), values);
}
//=============================================================================
static bool
holdsRange(const ArrayOf& value, indexType count)
{
    const auto* values = static_cast<const double*>(value.dataPointer());
    for (indexType k = 0; k < count; ++k) {
        if (values[k] != static_cast<double>(k + 1)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static void
checkSharedReshape()
{
    ArrayOf original = doubleRange(3, 4);
    ArrayOf reshaped = original;
    Dimensions shape(4, 3);
    reshaped.reshape(shape);
    require(reshaped.dataPointer() == original.dataPointer(), "Reshape copied shared data");
    require(original.dimensions().equals(Dimensions(3, 4)), "Reshape changed the original shape");
    require(reshaped.dimensions().equals(shape), "Reshape lost the new shape");
    require(original.referenceCount() == 2, "Original lost the borrowing reference");
    require(reshaped.referenceCount() > 2, "Borrowed block reported a single owner");
    auto* written = static_cast<double*>(reshaped.readWriteDataPointer());
    require(written != original.dataPointer(), "Write went into the original data");
    written[0] = -1.;
    require(holdsRange(original, 12), "Original values changed");
    require(original.referenceCount() == 1, "Detached copy kept the original shared");
}
//=============================================================================
static void
checkOriginalWrite()
{
    ArrayOf original = doubleRange(2, 6);
    ArrayOf reshaped = original;
    Dimensions shape(3, 4);
    reshaped.reshape(shape);
    auto* written = static_cast<double*>(original.readWriteDataPointer());
    written[5] = 0.;
    require(holdsRange(reshaped, 12), "Reshaped values changed");
    require(reshaped.dimensions().equals(shape), "Reshaped shape changed");
}
//=============================================================================
static void
checkLastHolderAdopts()
{
    ArrayOf original = doubleRange(4, 5);
    ArrayOf reshaped = original;
    Dimensions shape(5, 4);
    reshaped.reshape(shape);
    const void* shared = reshaped.dataPointer();
    original = ArrayOf();
    require(reshaped.referenceCount() == 1, "Last holder was reported as shared");
    require(reshaped.readWriteDataPointer() == shared, "Last holder copied its data");
    require(holdsRange(reshaped, 20), "Adopted values changed");
}
//=============================================================================
static void
checkChainedReshape()
{
    ArrayOf original = doubleRange(4, 6);
    ArrayOf first = original;
    Dimensions shapeFirst(6, 4);
    first.reshape(shapeFirst);
    ArrayOf second = first;
    Dimensions shapeSecond(2, 12);
    second.reshape(shapeSecond);
    require(second.dataPointer() == original.dataPointer(), "Chained reshape copied data");
    require(first.dimensions().equals(shapeFirst), "Chained reshape changed its source");
    const void* shared = second.dataPointer();
    original = ArrayOf();
    first = ArrayOf();
    require(second.referenceCount() == 1, "Chain end was reported as shared");
    require(second.readWriteDataPointer() == shared, "Chain end copied its data");
}
//=============================================================================
static void
checkGrowth()
{
    ArrayOf original = doubleRange(1, 6);
    ArrayOf reshaped = original;
    Dimensions column(6, 1);
    reshaped.reshape(column);
    reshaped.vectorResize(8);
    require(reshaped.dimensions().equals(Dimensions(8, 1)), "Growth lost the shape");
    require(reshaped.dataPointer() != original.dataPointer(), "Growth wrote shared data");
    require(original.dimensions().equals(Dimensions(1, 6)) && holdsRange(original, 6),
        "Growth changed the original");
}
//=============================================================================
static void
checkCells()
{
    auto* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, 4));
    for (indexType k = 0; k < 4; ++k) {
        elements[k] = ArrayOf::doubleScalar(static_cast<double>(k));
    }
    ArrayOf cell(NLS_CELL_ARRAY, Dimensions(1, 4), elements);
    ArrayOf reshaped = cell;
    Dimensions shape(2, 2);
    reshaped.reshape(shape);
    require(reshaped.dataPointer() == cell.dataPointer(), "Cell reshape copied data");
    auto* written = static_cast<ArrayOf*>(reshaped.readWriteDataPointer());
    written[1] = ArrayOf::doubleScalar(42.);
    const auto* kept = static_cast<const ArrayOf*>(cell.dataPointer());
    require(kept[1].asDoubleScalar() == 1., "Cell element changed in the original");
}
//=============================================================================
static void
checkSparse()
{
    ArrayOf dense = doubleRange(3, 4);
    auto* values = static_cast<double*>(dense.readWriteDataPointer());
    values[1] = 0.;
    values[6] = 0.;
    values[11] = 0.;
    ArrayOf expected = dense;
    Dimensions shape(2, 6);
    expected.reshape(shape);
    ArrayOf sparse = dense;
    sparse.makeSparse();
    sparse.reshape(shape);
    sparse.makeDense();
    require(sparse.dimensions().equals(shape), "Sparse reshape lost the shape");
    const auto* got = static_cast<const double*>(sparse.dataPointer());
    const auto* want = static_cast<const double*>(expected.dataPointer());
    for (indexType k = 0; k < 12; ++k) {
        require(got[k] == want[k], "Sparse reshape moved a value");
    }
}
//=============================================================================
int
main()
{
    try {
        checkSharedReshape();
        checkOriginalWrite();
        checkLastHolderAdopts();
        checkChainedReshape();
        checkGrowth();
        checkCells();
        checkSparse();
        std::printf("RESHAPE_SHARED_STORAGE_OK\n");
        return 0;
    } catch (const std::exception& error) {
        std::fprintf(stderr, "%s\n", error.what());
        return 1;
    }
}
//=============================================================================
