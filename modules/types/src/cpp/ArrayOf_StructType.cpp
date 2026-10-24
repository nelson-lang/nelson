//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <utility>
#include "StringHelpers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    hasFieldName(const ArrayOf& value, const std::string& fieldName)
    {
        const stringVector fieldNames = value.fieldNames();
        return std::find(fieldNames.begin(), fieldNames.end(), fieldName) != fieldNames.end();
    }
    //=============================================================================
    bool
    supportsFieldAssignment(NelsonType dataClass)
    {
        return dataClass == NLS_STRUCT_ARRAY || dataClass == NLS_CLASS_ARRAY
            || dataClass == NLS_CLASSDEF_ARRAY || dataClass == NLS_FUNCTION_HANDLE;
    }
    //=============================================================================
    void
    raiseNonExistentField(const ArrayOf& value, const std::string& fieldName);
    //=============================================================================
} // namespace
//=============================================================================
bool
ArrayOf::isStruct() const
{
    return (this->dataClass() == NLS_STRUCT_ARRAY);
}
//=============================================================================
stringVector
ArrayOf::fieldNames() const
{
    if (dp) {
        return dp->fieldNames();
    }
    return {};
}
//=============================================================================
ArrayOf
ArrayOf::structScalar(const wstringVector& fNames, const ArrayOfVector& values)
{
    stringVector fieldnames;
    fieldnames.reserve(fNames.size());
    for (const auto& name : fNames) {
        fieldnames.push_back(wstring_to_utf8(name));
    }
    return structScalar(fieldnames, values);
}
//=============================================================================
ArrayOf
ArrayOf::structScalar(const stringVector& fNames, const ArrayOfVector& values)
{
    Dimensions dims;
    ArrayOf* qp = nullptr;
    try {
        if (fNames.size() != values.size()) {
            Error(L"Nelson:types:fieldNamesValuesCountMismatch",
                L"Number of field names must match number of values in %s constructor.",
                L"structure");
        }
        Dimensions dims(1, 1);
        qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fNames, false);
        indexType length = dims.elementCount();
        indexType offset = 0;
        for (indexType j = 0; j < length; j++) {
            for (indexType i = 0; i < (indexType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                qp[offset] = rval;
                offset++;
            }
        }
        return ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fNames);
    } catch (const NelsonException&) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::structArray(const stringVector& fNames, const ArrayOfVector& values)
{
    const ArrayOf* rptr;
    Dimensions dims;
    indexType i, j;
    ArrayOf* qp = nullptr;
    try {
        if (fNames.size() != values.size()) {
            Error(L"Nelson:types:fieldNamesValuesCountMismatch",
                L"Number of field names must match number of values in %s constructor.",
                L"structure");
        }
        bool nonSingularFound = false;
        for (i = 0; i < (indexType)values.size(); i++) {
            ArrayOf value = values[i];
            if (value.dataClass() == NLS_CELL_ARRAY || value.dataClass() == NLS_STRING_ARRAY) {
                if (!value.isScalar()) {
                    if (!nonSingularFound) {
                        nonSingularFound = true;
                        dims = value.dimensions();
                    } else if (!dims.equals(value.dimensions())) {
                        Error(L"Nelson:types:structConstructionDimensionsMismatch",
                            L"Dimensions of non-scalar entries must agree in structure "
                            L"construction.");
                    }
                }
            }
        }
        if (!nonSingularFound) {
            dims.reset();
            dims[0] = 1;
            dims[1] = 1;
        }
        qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fNames, false);
        indexType length = dims.elementCount();
        indexType offset = 0;
        for (j = 0; j < length; j++) {
            for (i = 0; i < (indexType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                if (rval.dataClass() == NLS_CELL_ARRAY) {
                    rptr = static_cast<const ArrayOf*>(rval.dataPointer());
                    if (rval.isScalar()) {
                        qp[offset] = rptr[0];
                    } else {
                        qp[offset] = rptr[j];
                    }
                } else {
                    qp[offset] = rval;
                }
                offset++;
            }
        }
        return ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fNames);
    } catch (const NelsonException&) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::structArray(const wstringVector& fNames, const ArrayOfVector& values)
{
    stringVector fNamesUtf8;
    for (const auto& fName : fNames) {
        fNamesUtf8.push_back(wstring_to_utf8(fName));
    }
    return ArrayOf::structArray(fNamesUtf8, values);
}
//=============================================================================
bool
ArrayOf::isField(const std::string& fieldName) const
{
    if (!this->isStruct() && !this->isClassType()) {
        return false;
    }
    const auto& fieldnames = fieldNames();
    return std::find(fieldnames.begin(), fieldnames.end(), fieldName) != fieldnames.end();
}
//=============================================================================
ArrayOf
ArrayOf::field(const std::string& fieldName) const
{
    if (!isScalar()) {
        Error(L"Nelson:types:structFieldExpected:cannotDereferenceAFieldAMulti",
            L"Cannot dereference a field of a multi-element structure array.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.", L"field");
    }
    int64 ndx = fieldIndex(fieldName);
    if (ndx < 0) {
        raiseNonExistentField(*this, fieldName);
    }
    const ArrayOf* qp = (const ArrayOf*)dp->data();
    return qp[ndx];
}
//=============================================================================
ArrayOfVector
ArrayOf::fieldAsList(const std::string& fieldName) const
{
    if (!(isStruct() || isClassType())) {
        Error(L"Nelson:types:structFieldExpected:attemptApplyFieldIndexingNonStructure",
            L"Attempt to apply field-indexing to non structure-array object.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"fieldAsList");
    }
    const ArrayOf* qp = (const ArrayOf*)dp->data();
    indexType N = elementCount();
    indexType fieldCount = static_cast<indexType>(dp->fieldNames().size());
    int64 ndx = fieldIndex(fieldName);
    if (ndx < 0) {
        raiseNonExistentField(*this, fieldName);
    }
    ArrayOfVector m;
    m.reserve(static_cast<size_t>(N));
    for (indexType i = 0; i < N; i++) {
        m.push_back(qp[i * fieldCount + ndx]);
    }
    return m;
}
//=============================================================================
void
ArrayOf::setField(const std::string& fieldName, ArrayOf& data)
{
    if (isClassdefType() && !hasFieldName(*this, fieldName)) {
        Error("Nelson:types:fieldNotFound", "No such property: %s", fieldName);
    }
    if (isEmpty()) {
        stringVector newNames(fieldNames());
        if (!hasFieldName(*this, fieldName)) {
            newNames.push_back(fieldName);
        }
        NelsonType targetType = isClassType() ? dp->dataClass : NLS_STRUCT_ARRAY;
        promoteType(targetType, newNames);
        Dimensions a;
        a[0] = 1;
        a[1] = 1;
        resize(a);
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"setField");
    }
    if (!supportsFieldAssignment(dataClass())) {
        Error(L"Nelson:types:structFieldExpected",
            L"Cannot apply A.field_name = B to non struct-array object A.");
    }
    if (!isScalar()) {
        Error(L"Nelson:types:structFieldExpected:cannotApplyAFieldNameB",
            L"Cannot apply A.field_name = B to multi-element structure array A.");
    }
    double field_ndx = (double)fieldIndex(fieldName);
    if (field_ndx == -1) {
        field_ndx = (double)insertFieldName(fieldName);
    }
    ArrayOf* sp = (ArrayOf*)readWriteDataPointer();
    sp[(indexType)field_ndx] = data;
    dp->dimensions_.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
void
ArrayOf::setFieldAsList(const std::string& fieldName, ArrayOfVector& data)
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"setFieldAsList");
    }
    if (isClassdefType() && !hasFieldName(*this, fieldName)) {
        Error("Nelson:types:fieldNotFound", "No such property: %s", fieldName);
    }
    if (isEmpty()) {
        stringVector names(fieldNames());
        if (!hasFieldName(*this, fieldName)) {
            names.push_back(fieldName);
        }
        NelsonType targetType = isClassType() ? dp->dataClass : NLS_STRUCT_ARRAY;
        promoteType(targetType, names);
        Dimensions a;
        a[0] = 1;
        a[1] = 1;
        resize(a);
    }
    if (!this->isStruct() && !this->isClassType()) {
        Error(L"Nelson:types:structFieldExpected",
            L"Cannot apply A.field_name = B to non struct-array object A.");
    }
    if ((int)data.size() < elementCount()) {
        if (data.size() == 1) {
            // A single value assigned to a field of a non-scalar structure: the
            // structure must be scalar for A.field = B, using the established convention.
            Error(L"Nelson:types:scalarStructureRequired",
                L"Scalar structure required for this assignment.");
        }
        Error(L"Nelson:types:notEnoughRhsValues",
            L"Not enough right hand side values to satisfy left hand side expression.");
    }
    indexType indexLength = elementCount();
    int64 field_ndx = fieldIndex(fieldName);
    if (field_ndx == -1) {
        field_ndx = insertFieldName(fieldName);
    }
    indexType fieldCount = static_cast<indexType>(dp->fieldNames().size());
    ArrayOf* qp = (ArrayOf*)readWriteDataPointer();
    // Index data directly: popping the front of the underlying std::vector on
    // every element is O(n) per pop, making this loop quadratic.
    for (indexType i = 0; i < indexLength; i++) {
        qp[i * fieldCount + field_ndx] = data[i];
        qp[i * fieldCount + field_ndx].name("");
    }
    dp->dimensions_.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
indexType
ArrayOf::insertFieldName(const std::string& fieldName)
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"insertFieldName");
    }
    if (dp->dataClass == NLS_CLASSDEF_ARRAY) {
        Error("Nelson:types:fieldNotFound", "No such property: %s", fieldName);
    }
    // Fast path: a uniquely owned scalar structure holds exactly one element, so a new field
    // is one extra slot at the end with no re-striding. A doubling buffer avoids reallocating
    // on every add, turning an O(nFields^2) build into O(nFields).
    if (elementCount() == 1 && dp->numberOfOwners() <= 1) {
        const indexType oldCount = static_cast<indexType>(dp->fieldNames_.size());
        if (oldCount >= dp->allocatedFieldCount) {
            indexType newCapacity = dp->allocatedFieldCount < 4 ? 4 : dp->allocatedFieldCount * 2;
            if (newCapacity <= oldCount) {
                newCapacity = oldCount + 1;
            }
            auto* oldStorage = static_cast<ArrayOf*>(dp->cp);
            auto* newStorage = new_with_exception<ArrayOf>(newCapacity, false);
            for (indexType j = 0; j < oldCount; j++) {
                newStorage[j] = std::move(oldStorage[j]);
            }
            delete[] oldStorage;
            dp->cp = newStorage;
            dp->allocatedFieldCount = newCapacity;
        }
        // match the empty double of a freshly allocated field
        static_cast<ArrayOf*>(dp->cp)[oldCount] = ArrayOf(NLS_DOUBLE);
        dp->fieldNames_.push_back(fieldName);
        dp->noteAppendedField(fieldName, oldCount);
        return oldCount;
    }
    stringVector names(fieldNames());
    names.push_back(fieldName);
    NelsonType dataClass = dp->dataClass;
    const ArrayOf* qp = (const ArrayOf*)dp->data();
    ArrayOf* rp = (ArrayOf*)allocateArrayOf(dp->dataClass, elementCount(), names, false);
    std::string classtype = dp->classTypeName();
    indexType fN = names.size();
    indexType oldFieldCount = fN - 1;
    if (oldFieldCount > 0) {
        // The storage is element-major (element e, field j at index e*fieldCount+j), so the
        // stride changes from oldFieldCount to fN.
        ompIndexType elemCount = (ompIndexType)elementCount();
        OMP_PARALLEL_FOR_LOOP(elemCount)
        for (ompIndexType e = 0; e < elemCount; e++) {
            for (indexType j = 0; j < oldFieldCount; j++) {
                rp[e * fN + j] = qp[e * oldFieldCount + j];
            }
        }
    }
    dp = dp->putData(dataClass, dp->dimensions_, rp, false, names);
    dp->setClassTypeName(classtype);
    return (fN - 1);
}
//=============================================================================
int64
ArrayOf::fieldIndexFromList(const std::string& fName, const stringVector& fieldNames) const
{
    bool foundName = false;
    uint64 i = 0;
    while (i < (uint64)fieldNames.size() && !foundName) {
#ifdef NLS_INDEX_TYPE_64
        foundName = (fieldNames[i] == fName);
#else
        foundName = (fieldNames[(indexType)i] == fName);
#endif
        if (!foundName) {
            i++;
        }
    }
    if (foundName) {
        return i;
    }
    return -1;
}
//=============================================================================
int64
ArrayOf::fieldIndex(const std::string& fName) const
{
    if (dp == nullptr) {
        return -1;
    }
    return dp->fieldIndexFromName(fName);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStruct()
{
    stringVector fieldnames;
    fieldnames.clear();
    ArrayOfVector fieldvalues;
    fieldvalues.clear();
    return ArrayOf::structArray(fieldnames, fieldvalues);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStruct(const stringVector& fNames, Dimensions& dim)
{
    if (dim.elementCount() != 0) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    ArrayOf* qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dim.elementCount(), fNames, false);
    return ArrayOf(NLS_STRUCT_ARRAY, dim, qp, false, fNames);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStruct(const wstringVector& fNames, Dimensions& dim)
{
    stringVector fs;
    fs.reserve(fNames.size());
    for (const auto& fName : fNames) {
        fs.push_back(wstring_to_utf8(fName));
    }
    return ArrayOf::emptyStruct(fs, dim);
}
//=============================================================================
bool
ArrayOf::haveValidFieldNames(const stringVector& fieldnames)
{
    if (fieldnames.empty()) {
        return true;
    }
    for (const auto& fieldname : fieldnames) {
        if (fieldname.size() == 0) {
            return false;
        }
        if (StringHelpers::contains(fieldname, "\n")) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
ArrayOf::haveUniqueFieldNames(const stringVector& fieldnames)
{
    if (fieldnames.size() > 1) {
        stringVector copyVector(fieldnames);
        std::sort(copyVector.begin(), copyVector.end());
        copyVector.erase(std::unique(copyVector.begin(), copyVector.end()), copyVector.end());
        return fieldnames.size() == copyVector.size();
    }
    return true;
}
//=============================================================================
bool
ArrayOf::renameFieldnames(const stringVector& newFieldnames)
{
    if (dp->dataClass == NLS_CLASSDEF_ARRAY) {
        return false;
    }
    if (dp->fieldNames_.size() != newFieldnames.size()) {
        return false;
    }
    dp->fieldNames_ = newFieldnames;
    dp->invalidateFieldIndexCache();
    return true;
}
//=============================================================================
namespace {
    //=============================================================================
    // The Properties of a table or timetable (VariableNames, VariableTypes and
    // RowNames or RowTimes) read with an unknown name, as in T.Properties.Foo.
    void
    raiseNonExistentField(const ArrayOf& value, const std::string& fieldName)
    {
        if (value.isStruct() && hasFieldName(value, "VariableNames")
            && hasFieldName(value, "VariableTypes")
            && (hasFieldName(value, "RowNames") || hasFieldName(value, "RowTimes"))) {
            Error("Nelson:table:unknownProperty", "Unknown table property: %s.", fieldName);
        }
        Error("Nelson:nonExistentField", "Reference to non-existent field %s", fieldName);
    }
    //=============================================================================
} // namespace
//=============================================================================
} // namespace Nelson
//=============================================================================
