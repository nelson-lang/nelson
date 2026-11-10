//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <cstring>
#include "PartialFileAccess.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
indexType
PartialFileBlock::last(size_t k) const
{
    return start[k] + stride[k] * (count[k] - 1);
}
//=============================================================================
indexType
PartialFileBlock::elementCount() const
{
    indexType n = 1;
    for (indexType c : count) {
        n *= c;
    }
    return n;
}
//=============================================================================
Dimensions
PartialFileBlock::resultDimensions() const
{
    Dimensions dims;
    for (size_t k = 0; k < count.size(); k++) {
        dims[k] = count[k];
    }
    dims.simplify();
    return dims;
}
//=============================================================================
void
PartialFileBackend::beforeWholeAccess(const std::string& name)
{
}
//=============================================================================
namespace {
    //=============================================================================
    void
    raiseNotInFile(const PartialFileBackend& backend, const std::string& name, bool fileFound)
    {
        if (!fileFound) {
            Error("Nelson:io:fileNotFound", "Variable '%s' cannot be read: file '%s' not found.",
                name, backend.fileText());
        }
        Error("Nelson:io:variableNotFound", "No variable '%s' in '%s'.", name, backend.fileText());
    }
    //=============================================================================
    PartialFileVariable
    describeExisting(PartialFileBackend& backend, const std::string& name)
    {
        PartialFileVariable variable = backend.describe(name);
        if (!variable.exists) {
            raiseNotInFile(backend, name, variable.fileExists);
        }
        return variable;
    }
    //=============================================================================
    // class of the dense values of the variable, NLS_UNKNOWN when it has none
    NelsonType
    denseType(const PartialFileVariable& variable)
    {
        if (variable.isSparse) {
            return NLS_UNKNOWN;
        }
        switch (variable.type) {
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
        case NLS_SINGLE:
        case NLS_SCOMPLEX:
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_INT64:
        case NLS_UINT64:
        case NLS_LOGICAL:
        case NLS_CHAR:
            return variable.type;
        default:
            return NLS_UNKNOWN;
        }
    }
    //=============================================================================
    // dimensions seen by n subscripts: trailing dimensions fold into the last one
    Dimensions
    dimensionsForSubscripts(const Dimensions& dims, size_t n)
    {
        Dimensions result;
        for (size_t k = 0; k < n; k++) {
            if (k + 1 < n) {
                result[k] = k < dims.length() ? dims.at(k, false) : 1;
            } else {
                indexType product = 1;
                for (size_t j = k; j < dims.length(); j++) {
                    product *= dims.at(j, false);
                }
                result[k] = product;
            }
        }
        return result;
    }
    //=============================================================================
    void
    checkStep(const std::string& name, const double* values, indexType count)
    {
        const double step = count > 1 ? values[1] - values[0] : 1.0;
        bool regular = step > 0;
        for (indexType j = 1; j < count && regular; j++) {
            regular = values[j] == values[0] + step * static_cast<double>(j);
        }
        if (!regular) {
            Error("Nelson:io:irregularIndex", "Indices of '%s' must increase with a constant step.",
                name);
        }
        if (values[0] <= 0 || std::floor(values[0]) != values[0] || std::floor(step) != step) {
            Error("Nelson:io:invalidIndex", "Indices must be positive integers.");
        }
    }
    //=============================================================================
    // subscripts of obj.name(...) as strided blocks (dims: variable seen by the subscripts)
    PartialFileBlock
    parseSubscripts(const std::string& name, const ArrayOfVector& indices, bool variableExists,
        const Dimensions& dims)
    {
        PartialFileBlock block;
        const size_t n = indices.size();
        block.start.resize(n);
        block.stride.resize(n);
        block.count.resize(n);
        for (size_t k = 0; k < n; k++) {
            const ArrayOf& index = indices[k];
            if (index.isRowVectorCharacterArray() && index.asUtf8String() == ":") {
                if (!variableExists) {
                    Error("Nelson:io:colonOnNewVariable",
                        "':' cannot size a dimension of a new variable.");
                }
                if (dims.at(k, false) == 0) {
                    Error("Nelson:io:colonOnEmptyVariable",
                        "':' cannot index the empty variable '%s'.", name);
                }
                block.start[k] = 1;
                block.stride[k] = 1;
                block.count[k] = dims.at(k, false);
                continue;
            }
            if (!index.isNumeric() || index.isSparse() || index.isComplex()) {
                Error("Nelson:io:nonNumericIndex", "Indices of '%s' must be numeric, not %s.", name,
                    ClassName(index));
            }
            if (index.isEmpty()) {
                Error("Nelson:io:emptyIndex", "Indices of '%s' must not be empty.", name);
            }
            ArrayOf asDouble(index);
            asDouble.promoteType(NLS_DOUBLE);
            const auto* values = static_cast<const double*>(asDouble.dataPointer());
            const indexType count = asDouble.elementCount();
            checkStep(name, values, count);
            block.start[k] = static_cast<indexType>(values[0]);
            block.stride[k] = count > 1 ? static_cast<indexType>(values[1] - values[0]) : 1;
            block.count[k] = count;
        }
        return block;
    }
    //=============================================================================
    void
    checkSubscriptCount(const std::string& name, size_t n, const PartialFileVariable& variable)
    {
        if (n >= 2) {
            return;
        }
        if (variable.exists) {
            Error("Nelson:io:linearIndex",
                "'%s' needs one index per dimension (%d): linear indexing is not supported.", name,
                static_cast<int>(std::max<size_t>(variable.dims.length(), 2)));
        }
        Error("Nelson:io:linearIndexNewVariable",
            "New variable '%s' needs at least two indices: linear indexing is not supported.",
            name);
    }
    //=============================================================================
    ArrayOf
    rangeIndex(indexType start, indexType stride, indexType count)
    {
        auto* values = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, count, stringVector(), false));
        for (indexType k = 0; k < count; k++) {
            values[k] = static_cast<double>(start + stride * k);
        }
        return ArrayOf(NLS_DOUBLE, Dimensions(1, count), values);
    }
    //=============================================================================
    ArrayOfVector
    blockIndices(const PartialFileBlock& block)
    {
        ArrayOfVector indices;
        indices.reserve(block.count.size());
        for (size_t k = 0; k < block.count.size(); k++) {
            indices.push_back(rangeIndex(block.start[k], block.stride[k], block.count[k]));
        }
        return indices;
    }
    //=============================================================================
    bool
    isBuiltinIndexable(const ArrayOf& value)
    {
        return !value.isClassType() && !value.isHandle() && !value.isFunctionHandle()
            && !value.isStringArray();
    }
    //=============================================================================
    void
    raiseWholeVariableOnly(const std::string& name, const ArrayOf& value)
    {
        Error("Nelson:io:wholeVariableOnly", "'%s' (%s) can only be read or written whole.", name,
            ClassName(value));
    }
    //=============================================================================
    bool
    isInBounds(const PartialFileBlock& block, const Dimensions& seen)
    {
        for (size_t k = 0; k < block.count.size(); k++) {
            if (block.last(k) > seen.at(k, false)) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    isEmptyBracketValue(const ArrayOf& value)
    {
        return value.isDoubleClass() && !value.isSparse() && value.isEmpty()
            && value.dimensions().length() == 2 && value.dimensions()[0] == 0
            && value.dimensions()[1] == 0;
    }
    //=============================================================================
    // the right side converts to the class of the variable in the file
    bool
    isAssignableClass(const PartialFileVariable& variable, const ArrayOf& value)
    {
        switch (variable.type) {
        case NLS_DOUBLE:
        case NLS_SINGLE:
        case NLS_DCOMPLEX:
        case NLS_SCOMPLEX:
            return (value.isDoubleClass() || value.isSingleClass()) && !value.isSparse()
                && (variable.type == NLS_DCOMPLEX || variable.type == NLS_SCOMPLEX
                    || !value.isComplex());
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_INT64:
        case NLS_UINT64:
            return value.isIntegerType();
        case NLS_LOGICAL:
            return value.isLogical();
        case NLS_CHAR:
            return value.isCharacterArray();
        case NLS_CELL_ARRAY:
            return value.isCell();
        case NLS_STRUCT_ARRAY:
            return value.isStruct();
        default:
            return false;
        }
    }
    //=============================================================================
    // the right side with one value per selected position, in the class of the variable
    ArrayOf
    blockData(const ArrayOf& value, NelsonType type, const PartialFileBlock& block)
    {
        const indexType count = block.elementCount();
        ArrayOf converted(value);
        converted.ensureSingleOwner();
        converted.promoteType(type);
        if (converted.elementCount() == count) {
            return converted;
        }
        const size_t elementSize = converted.elementSize();
        auto* data
            = static_cast<char*>(ArrayOf::allocateArrayOf(type, count, stringVector(), false));
        const auto* source = static_cast<const char*>(converted.dataPointer());
        for (indexType k = 0; k < count; k++) {
            std::memcpy(data + k * elementSize, source, elementSize);
        }
        return ArrayOf(type, block.resultDimensions(), data);
    }
    //=============================================================================
    ArrayOf
    emptyLike(const ArrayOf& value)
    {
        ArrayOf base = ArrayOf::empty(0, 0);
        if (value.isStruct()) {
            base.promoteType(NLS_STRUCT_ARRAY, value.fieldNames());
        } else {
            base.promoteType(value.dataClass());
        }
        return base;
    }
    //=============================================================================
    void
    checkWritableInto(PartialFileBackend& backend, const std::string& name,
        const PartialFileVariable& variable, const ArrayOf& value, const PartialFileBlock& block,
        size_t n)
    {
        if (variable.isSparse) {
            Error("Nelson:io:sparsePartialWrite",
                "Part of a sparse variable cannot be written: write the whole variable.");
        }
        if (!isAssignableClass(variable, value)) {
            if (variable.type == NLS_UNKNOWN) {
                ArrayOf whole = backend.readWhole(name);
                if (!isBuiltinIndexable(whole)) {
                    raiseWholeVariableOnly(name, whole);
                }
            }
            Error("Nelson:io:incompatibleClass",
                "The value cannot be converted to the class of the variable in the file.");
        }
        const size_t rank = variable.dims.length();
        for (size_t k = rank; k < n; k++) {
            if (block.last(k) > 1) {
                Error("Nelson:io:dimensionCountMismatch",
                    "'%s' has %d dimensions in the file but %d indices were given.", name,
                    static_cast<int>(rank), static_cast<int>(n));
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
PartialFileSubscripts(const ArrayOf& subscripts)
{
    if (!subscripts.isCell()) {
        Error("Nelson:data_structures:cellExpected", "cell expected.");
    }
    const auto* elements = static_cast<const ArrayOf*>(subscripts.dataPointer());
    ArrayOfVector list;
    list.reserve(subscripts.elementCount());
    for (indexType k = 0; k < subscripts.elementCount(); k++) {
        list.push_back(elements[k]);
    }
    return list;
}
//=============================================================================
ArrayOf
PartialFileVariableSize(PartialFileBackend& backend, const std::string& name)
{
    PartialFileVariable variable = describeExisting(backend, name);
    const indexType rank = variable.dims.length();
    auto* values
        = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rank, stringVector(), false));
    for (indexType k = 0; k < rank; k++) {
        values[k] = static_cast<double>(variable.dims[k]);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, rank), values);
}
//=============================================================================
ArrayOf
PartialFileRead(PartialFileBackend& backend, const std::string& name, const ArrayOfVector* indices)
{
    PartialFileVariable variable = describeExisting(backend, name);
    if (indices == nullptr) {
        return backend.readWhole(name);
    }
    const size_t n = indices->size();
    checkSubscriptCount(name, n, variable);
    const Dimensions seen = dimensionsForSubscripts(variable.dims, n);
    PartialFileBlock block = parseSubscripts(name, *indices, true, seen);
    const bool builtinClass = variable.type != NLS_UNKNOWN;
    ArrayOf whole;
    if (!builtinClass) {
        whole = backend.readWhole(name);
        if (!isBuiltinIndexable(whole)) {
            raiseWholeVariableOnly(name, whole);
        }
    }
    if (!isInBounds(block, seen)) {
        Error("Nelson:badsubscript", "Index exceeds array bounds.");
    }
    ArrayOf result;
    if (builtinClass && n >= variable.dims.length()
        && backend.readBlock(name, variable, block, result)) {
        return result;
    }
    backend.beforeWholeAccess(name);
    if (builtinClass) {
        whole = backend.readWhole(name);
    }
    ArrayOfVector subscripts = blockIndices(block);
    return whole.nDimSubset(subscripts);
}
//=============================================================================
void
PartialFileWrite(PartialFileBackend& backend, const std::string& name, const ArrayOfVector* indices,
    const ArrayOf& value)
{
    if (indices == nullptr) {
        backend.writeWhole(name, value);
        return;
    }
    if (isEmptyBracketValue(value)) {
        // obj.name(indices) = [] deletes in memory, then the variable is written back
        describeExisting(backend, name);
        ArrayOf whole = backend.readWhole(name);
        ArrayOfVector subscripts(*indices);
        whole.deleteNDimSubset(subscripts);
        backend.writeWhole(name, whole);
        return;
    }
    PartialFileVariable variable = backend.describe(name);
    const size_t n = indices->size();
    checkSubscriptCount(name, n, variable);
    const Dimensions seen
        = variable.exists ? dimensionsForSubscripts(variable.dims, n) : Dimensions();
    PartialFileBlock block = parseSubscripts(name, *indices, variable.exists, seen);
    if (variable.exists) {
        checkWritableInto(backend, name, variable, value, block, n);
    }
    if (value.elementCount() != 1 && value.elementCount() != block.elementCount()) {
        Error("Nelson:dimagree:sizeMismatchAssignmentAI1I2",
            "Size mismatch in assignment A(I1,I2,...,In) = B.");
    }
    const NelsonType type = denseType(variable);
    if (variable.exists && type != NLS_UNKNOWN && n >= variable.dims.length()
        && !variable.dims.isEmpty(false) && isInBounds(block, seen)
        && backend.writeBlock(name, variable, block, blockData(value, type, block))) {
        return;
    }
    if (variable.exists) {
        backend.beforeWholeAccess(name);
    }
    ArrayOf base = variable.exists ? backend.readWhole(name) : emptyLike(value);
    ArrayOf converted(value);
    if (variable.exists && type != NLS_UNKNOWN) {
        converted.ensureSingleOwner();
        converted.promoteType(type);
    }
    ArrayOfVector subscripts = blockIndices(block);
    base.setNDimSubset(subscripts, converted);
    backend.writeWhole(name, base);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
