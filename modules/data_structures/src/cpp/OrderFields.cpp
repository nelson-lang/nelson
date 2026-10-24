//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "OrderFields.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct FieldOrderEntry
{
    std::string name;
    indexType index;
};
//=============================================================================
static std::vector<indexType>
makePermutationFromSortedNames(const stringVector& fieldNames);
static std::vector<indexType>
makePermutationFromNames(const stringVector& currentNames, const stringVector& requestedNames);
static std::vector<indexType>
makePermutationFromNumeric(ArrayOf permutationValue, size_t fieldCount);
static stringVector
namesFromPermutation(const stringVector& currentNames, const std::vector<indexType>& permutation);
static ArrayOf
permutationToColumnVector(const std::vector<indexType>& permutation);
static ArrayOf
reorderStruct(const ArrayOf& input, const stringVector& orderedNames,
    const std::vector<indexType>& permutation);
static void
appendOutputs(ArrayOfVector& retval, const ArrayOf& reordered,
    const std::vector<indexType>& permutation, int nLhs);
//=============================================================================
ArrayOfVector
OrderFields(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf input = argIn[0];
    if (!input.isStruct()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"struct");
    }

    stringVector currentNames = input.fieldNames();
    std::vector<indexType> permutation;
    stringVector orderedNames;

    if (argIn.size() == 1) {
        permutation = makePermutationFromSortedNames(currentNames);
        orderedNames = namesFromPermutation(currentNames, permutation);
        appendOutputs(retval, reorderStruct(input, orderedNames, permutation), permutation, nLhs);
        return retval;
    }

    ArrayOf order = argIn[1];
    if (order.isCell() || order.isStringArray()) {
        orderedNames = order.asUtf8StringVector();
        permutation = makePermutationFromNames(currentNames, orderedNames);
        appendOutputs(retval, reorderStruct(input, orderedNames, permutation), permutation, nLhs);
        return retval;
    }

    if (order.isStruct()) {
        orderedNames = order.fieldNames();
        permutation = makePermutationFromNames(currentNames, orderedNames);
        appendOutputs(retval, reorderStruct(input, orderedNames, permutation), permutation, nLhs);
        return retval;
    }

    if (order.isNumeric()) {
        permutation = makePermutationFromNumeric(order, currentNames.size());
        orderedNames = namesFromPermutation(currentNames, permutation);
        appendOutputs(retval, reorderStruct(input, orderedNames, permutation), permutation, nLhs);
        return retval;
    }

    Error("Nelson:orderfields:invalidArgument",
        "Second argument must be a struct, a string, a cell array of character vectors, or a "
        "permutation vector.");
    return retval;
}
//=============================================================================
static std::vector<indexType>
makePermutationFromSortedNames(const stringVector& fieldNames)
{
    std::vector<FieldOrderEntry> entries;
    entries.reserve(fieldNames.size());
    for (indexType k = 0; k < static_cast<indexType>(fieldNames.size()); ++k) {
        entries.push_back({ fieldNames[k], k });
    }
    std::sort(entries.begin(), entries.end(),
        [](const FieldOrderEntry& left, const FieldOrderEntry& right) {
            return left.name < right.name;
        });

    std::vector<indexType> permutation;
    permutation.reserve(entries.size());
    for (const auto& entry : entries) {
        permutation.push_back(entry.index);
    }
    return permutation;
}
//=============================================================================
static std::vector<indexType>
makePermutationFromNames(const stringVector& currentNames, const stringVector& requestedNames)
{
    if (currentNames.size() != requestedNames.size()) {
        Error("Nelson:data_structures:textSecondInputMatchFieldnames",
            "Text in second input must match fieldnames of the struct.");
    }

    std::unordered_map<std::string, indexType> nameToIndex;
    nameToIndex.reserve(currentNames.size());
    for (indexType k = 0; k < static_cast<indexType>(currentNames.size()); ++k) {
        nameToIndex[currentNames[k]] = k;
    }

    std::vector<bool> seen(currentNames.size(), false);
    std::vector<indexType> permutation;
    permutation.reserve(requestedNames.size());
    for (const auto& name : requestedNames) {
        auto it = nameToIndex.find(name);
        if (it == nameToIndex.end()) {
            Error("Nelson:data_structures:textSecondInputMatchFieldnames",
                "Text in second input must match fieldnames of the struct.");
        }
        indexType idx = it->second;
        if (seen[idx]) {
            Error("Nelson:data_structures:textSecondInputMatchFieldnames",
                "Text in second input must match fieldnames of the struct.");
        }
        seen[idx] = true;
        permutation.push_back(idx);
    }
    return permutation;
}
//=============================================================================
static std::vector<indexType>
makePermutationFromNumeric(ArrayOf permutationValue, size_t fieldCount)
{
    bool isEmptyPermutation = permutationValue.isEmpty();
    if (!permutationValue.isNumeric() || permutationValue.isComplex() || permutationValue.isSparse()
        || (!permutationValue.isVector() && !isEmptyPermutation)) {
        Error("Nelson:orderfields:invalidArgument",
            "Second argument must be a struct, a string, a cell array of character vectors, or a "
            "permutation vector.");
    }

    std::vector<indexType> indices = permutationValue.asIndexVector();
    if (indices.size() != fieldCount) {
        Error("Nelson:data_structures:permutationVectorLengthMatchNumber",
            "Permutation vector length must match number of fields in the struct.");
    }

    std::vector<bool> seen(fieldCount, false);
    std::vector<indexType> permutation;
    permutation.reserve(indices.size());
    for (indexType value : indices) {
        if (value < 1 || value > static_cast<indexType>(fieldCount)) {
            Error("Nelson:data_structures:permutationVectorContainValidField",
                "Permutation vector must contain valid field indices.");
        }
        indexType zeroBased = value - 1;
        if (seen[zeroBased]) {
            Error("Nelson:data_structures:duplicatedFieldDetected", "Duplicated field detected.");
        }
        seen[zeroBased] = true;
        permutation.push_back(zeroBased);
    }
    return permutation;
}
//=============================================================================
static stringVector
namesFromPermutation(const stringVector& currentNames, const std::vector<indexType>& permutation)
{
    stringVector orderedNames;
    orderedNames.reserve(permutation.size());
    for (indexType idx : permutation) {
        orderedNames.push_back(currentNames[idx]);
    }
    return orderedNames;
}
//=============================================================================
static ArrayOf
permutationToColumnVector(const std::vector<indexType>& permutation)
{
    ArrayOf result = ArrayOf::doubleMatrix2d(static_cast<indexType>(permutation.size()), 1);
    double* values = static_cast<double*>(result.readWriteDataPointer());
    for (indexType k = 0; k < static_cast<indexType>(permutation.size()); ++k) {
        values[k] = static_cast<double>(permutation[k] + 1);
    }
    return result;
}
//=============================================================================
static ArrayOf
reorderStruct(const ArrayOf& input, const stringVector& orderedNames,
    const std::vector<indexType>& permutation)
{
    if (orderedNames.empty()) {
        return input;
    }

    Dimensions dims = input.dimensions();
    if (dims.elementCount() == 0) {
        return ArrayOf::emptyStruct(orderedNames, dims);
    }

    ArrayOf* data = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), orderedNames, false));
    ArrayOf result = ArrayOf(NLS_STRUCT_ARRAY, dims, data, false, orderedNames);
    stringVector currentNames = input.fieldNames();
    for (indexType outIndex = 0; outIndex < static_cast<indexType>(orderedNames.size());
        ++outIndex) {
        ArrayOfVector values = input.fieldAsList(currentNames[permutation[outIndex]]);
        result.setFieldAsList(orderedNames[outIndex], values);
    }
    return result;
}
//=============================================================================
static void
appendOutputs(ArrayOfVector& retval, const ArrayOf& reordered,
    const std::vector<indexType>& permutation, int nLhs)
{
    retval << reordered;
    if (nLhs > 1) {
        retval << permutationToColumnVector(permutation);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
