//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cell2matBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ConcatenateDimensions.hpp"
#include "FindCommonConcatenateType.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct Cell2MatLayout
{
    indexType rank = 2;
    Dimensions outputDims;
    std::vector<std::vector<indexType>> offsets;
};
//=============================================================================
bool
isObjectValue(const ArrayOf& value)
{
    return value.dataClass() >= NLS_CLASS_ARRAY;
}
//=============================================================================
bool
isSupportedScalarContent(const ArrayOf& value)
{
    return value.isNumeric() || value.isCharacterArray() || value.isLogical() || value.isStruct()
        || value.isStringArray();
}
//=============================================================================
bool
isSkippedEmpty(const ArrayOf& value)
{
    // a 0x0 element adds nothing, like [] in a concatenation
    return value.isEmpty() && value.is2D() && value.rows() == 0 && value.columns() == 0;
}
//=============================================================================
// Returns true when the element needs the concatenation conversion path:
// a class other than the first element's, a sparse value or a skipped empty.
bool
checkElementType(const ArrayOf& value, NelsonType dataClass, const stringVector& fieldNames)
{
    if (value.isCell() || isObjectValue(value)) {
        Error(L"Nelson:data_structures:allElementsCellArraySame",
            L"All elements of cell array must have same type.");
    }
    if (value.dataClass() != dataClass) {
        if (dataClass == NLS_STRUCT_ARRAY || value.isStruct()) {
            Error(L"Nelson:data_structures:allElementsCellArraySame",
                L"All elements of cell array must have same type.");
        }
        return true;
    }
    if (dataClass == NLS_STRUCT_ARRAY && value.fieldNames() != fieldNames) {
        Error(L"Nelson:data_structures:allStructuresSameFieldnames",
            L"All structures must have same fieldnames");
    }
    return value.isSparse() || isSkippedEmpty(value);
}
//=============================================================================
// Elements of mixed classes, sparse or 0x0 are converted as a concatenation
// would: 0x0 elements are left out and the others take the common class.
NelsonType
normalizeElements(std::vector<ArrayOf>& elements, bool& anySparse)
{
    ArrayOfVector values;
    values.reserve(elements.size());
    for (const auto& element : elements) {
        if (!isSkippedEmpty(element)) {
            values.push_back(element);
        }
    }
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    std::string typeName;
    if (values.empty()) {
        // only 0x0 elements: an empty result of their class
        return elements[0].dataClass();
    }
    if (!FindCommonConcatenateType(values, commonType, isSparse, isComplex, typeName)) {
        Error(L"Nelson:data_structures:allElementsCellArraySame",
            L"All elements of cell array must have same type.");
    }
    if (isComplex && commonType == NLS_DOUBLE) {
        commonType = NLS_DCOMPLEX;
    }
    if (isComplex && commonType == NLS_SINGLE) {
        commonType = NLS_SCOMPLEX;
    }
    anySparse = isSparse;
    for (auto& element : elements) {
        if (element.isSparse()) {
            element.makeDense();
        }
        if (element.dataClass() != commonType) {
            element.promoteType(commonType);
        }
    }
    return commonType;
}
//=============================================================================
std::vector<indexType>
linearSubscripts(indexType linearIndexValue, const Dimensions& dims, indexType rank)
{
    std::vector<indexType> subscripts(static_cast<size_t>(rank), 0);
    indexType remainder = linearIndexValue;
    for (indexType d = 0; d < rank; ++d) {
        indexType length = dims.dimensionLength(d);
        subscripts[static_cast<size_t>(d)] = remainder % length;
        remainder /= length;
    }
    return subscripts;
}
//=============================================================================
indexType
linearIndex(const Dimensions& dims, const std::vector<indexType>& subscripts, indexType rank)
{
    indexType index = 0;
    indexType stride = 1;
    for (indexType d = 0; d < rank; ++d) {
        index += subscripts[static_cast<size_t>(d)] * stride;
        stride *= dims.dimensionLength(d);
    }
    return index;
}
//=============================================================================
indexType
maxElementRank(const ArrayOf* cells, indexType count)
{
    indexType rank = 2;
    for (indexType k = 0; k < count; ++k) {
        rank = std::max(rank, cells[k].dimensions().length());
    }
    return rank;
}
//=============================================================================
bool
allElementsAreScalar(const ArrayOf* cells, indexType count)
{
    for (indexType k = 0; k < count; ++k) {
        if (!cells[k].isScalar()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
ArrayOf
cell2matScalarElements(ArrayOf* cells, const Dimensions& cellDims, NelsonType dataClass,
    const stringVector& fieldNames)
{
    void* destination
        = ArrayOf::allocateArrayOf(dataClass, cellDims.elementCount(), fieldNames, false);
    for (indexType k = 0; k < cellDims.elementCount(); ++k) {
        cells[k].copyElements(0, destination, k, 1);
    }
    return ArrayOf(dataClass, cellDims, destination, false, fieldNames);
}
//=============================================================================
std::vector<std::vector<indexType>>
makeSegmentLengths(
    const ArrayOf* cells, const Dimensions& cellDims, indexType cellRank, indexType outputRank)
{
    std::vector<std::vector<indexType>> lengths(static_cast<size_t>(outputRank));
    for (indexType d = 0; d < outputRank; ++d) {
        indexType count = d < cellRank ? cellDims.dimensionLength(d) : 1;
        lengths[static_cast<size_t>(d)].assign(static_cast<size_t>(count), INDEX_TYPE_MAX);
    }
    for (indexType k = 0; k < cellDims.elementCount(); ++k) {
        if (isSkippedEmpty(cells[k])) {
            continue;
        }
        std::vector<indexType> cellSub = linearSubscripts(k, cellDims, cellRank);
        Dimensions valueDims = cells[k].dimensions();
        for (indexType d = 0; d < outputRank; ++d) {
            indexType slot = d < cellRank ? cellSub[static_cast<size_t>(d)] : 0;
            indexType valueLength = valueDims.dimensionLength(d);
            indexType& stored = lengths[static_cast<size_t>(d)][static_cast<size_t>(slot)];
            if (stored == INDEX_TYPE_MAX) {
                stored = valueLength;
            } else if (stored != valueLength) {
                raiseConcatenationDimensionMismatch();
            }
        }
    }
    return lengths;
}
//=============================================================================
Cell2MatLayout
makeLayout(const ArrayOf* cells, const Dimensions& cellDims, indexType outputRank)
{
    indexType cellRank = cellDims.length();
    auto lengths = makeSegmentLengths(cells, cellDims, cellRank, outputRank);
    Cell2MatLayout layout;
    layout.rank = outputRank;
    layout.outputDims = Dimensions(outputRank);
    layout.offsets.resize(static_cast<size_t>(outputRank));
    for (indexType d = 0; d < outputRank; ++d) {
        indexType total = 0;
        auto& dimLengths = lengths[static_cast<size_t>(d)];
        auto& dimOffsets = layout.offsets[static_cast<size_t>(d)];
        dimOffsets.resize(dimLengths.size());
        for (size_t k = 0; k < dimLengths.size(); ++k) {
            dimOffsets[k] = total;
            if (dimLengths[k] != INDEX_TYPE_MAX) {
                total += dimLengths[k];
            }
        }
        layout.outputDims[d] = total;
    }
    return layout;
}
//=============================================================================
std::vector<indexType>
blockBaseSubscripts(
    const Cell2MatLayout& layout, const std::vector<indexType>& cellSub, indexType cellRank)
{
    std::vector<indexType> base(static_cast<size_t>(layout.rank), 0);
    for (indexType d = 0; d < cellRank; ++d) {
        base[static_cast<size_t>(d)]
            = layout.offsets[static_cast<size_t>(d)][static_cast<size_t>(cellSub[d])];
    }
    return base;
}
//=============================================================================
void
copyBlock(ArrayOf& value, void* destination, const Cell2MatLayout& layout,
    const std::vector<indexType>& base)
{
    Dimensions valueDims = value.dimensions();
    indexType rowCount = valueDims.dimensionLength(0);
    if (rowCount == 0 || value.elementCount() == 0) {
        return;
    }
    indexType sliceCount = 1;
    for (indexType d = 1; d < layout.rank; ++d) {
        sliceCount *= valueDims.dimensionLength(d);
    }
    std::vector<indexType> srcSub(static_cast<size_t>(layout.rank), 0);
    std::vector<indexType> dstSub(static_cast<size_t>(layout.rank), 0);
    for (indexType linear = 0; linear < sliceCount; ++linear) {
        indexType remainder = linear;
        dstSub[0] = base[0];
        for (indexType d = 1; d < layout.rank; ++d) {
            indexType length = valueDims.dimensionLength(d);
            indexType subscript = remainder % length;
            srcSub[static_cast<size_t>(d)] = subscript;
            dstSub[static_cast<size_t>(d)] = base[static_cast<size_t>(d)] + subscript;
            remainder /= length;
        }
        indexType srcIndex = linearIndex(valueDims, srcSub, layout.rank);
        indexType dstIndex = linearIndex(layout.outputDims, dstSub, layout.rank);
        value.copyElements(srcIndex, destination, dstIndex, rowCount);
    }
}
//=============================================================================
ArrayOf
cell2matDense(ArrayOf* cells, const Dimensions& cellDims, NelsonType dataClass,
    const stringVector& fieldNames)
{
    indexType cellRank = cellDims.length();
    indexType outputRank = std::max(cellRank, maxElementRank(cells, cellDims.elementCount()));
    Cell2MatLayout layout = makeLayout(cells, cellDims, outputRank);
    void* destination
        = ArrayOf::allocateArrayOf(dataClass, layout.outputDims.elementCount(), fieldNames, false);
    for (indexType k = 0; k < cellDims.elementCount(); ++k) {
        std::vector<indexType> cellSub = linearSubscripts(k, cellDims, cellRank);
        std::vector<indexType> base = blockBaseSubscripts(layout, cellSub, cellRank);
        copyBlock(cells[k], destination, layout, base);
    }
    return ArrayOf(dataClass, layout.outputDims, destination, false, fieldNames);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cell2matBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    const ArrayOf& input = argIn[0];
    if (input.isEmpty()) {
        return { ArrayOf::empty(Dimensions(0, 0)) };
    }
    if (!input.isCell()) {
        Error(L"Nelson:data_structures:firstInputArgumentCellArray",
            L"First input argument must be a cell array.");
    }
    auto* cells = const_cast<ArrayOf*>(static_cast<const ArrayOf*>(input.dataPointer()));
    const ArrayOf& first = cells[0];
    if (input.isScalar() && isSupportedScalarContent(first)) {
        return { first };
    }
    if (first.isCell() || isObjectValue(first)) {
        Error(L"Nelson:data_structures:cellCellsObjectSupported",
            L"Cell with cells or object not supported.");
    }
    NelsonType dataClass = first.dataClass();
    stringVector fieldNames = first.fieldNames();
    bool needsConversion = false;
    for (indexType k = 0; k < input.elementCount(); ++k) {
        needsConversion = checkElementType(cells[k], dataClass, fieldNames) || needsConversion;
    }
    if (needsConversion) {
        std::vector<ArrayOf> elements(cells, cells + input.elementCount());
        bool anySparse = false;
        dataClass = normalizeElements(elements, anySparse);
        ArrayOf result = cell2matDense(elements.data(), input.dimensions(), dataClass, fieldNames);
        if (anySparse) {
            result.makeSparse();
        }
        return { result };
    }
    if (allElementsAreScalar(cells, input.elementCount())) {
        return { cell2matScalarElements(cells, input.dimensions(), dataClass, fieldNames) };
    }
    return { cell2matDense(cells, input.dimensions(), dataClass, fieldNames) };
}
//=============================================================================
