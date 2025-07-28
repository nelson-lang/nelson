//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isStruct() const
{
    return (this->getDataClass() == NLS_STRUCT_ARRAY);
}
//=============================================================================
stringVector
ArrayOf::getFieldNames() const
{
    if (dp) {
        return dp->fieldNames;
    }
    return {};
}
//=============================================================================
ArrayOf
ArrayOf::structScalarConstructor(const wstringVector& fNames, const ArrayOfVector& values)
{
    stringVector fieldnames;
    fieldnames.reserve(fNames.size());
    for (const auto& name : fNames) {
        fieldnames.push_back(wstring_to_utf8(name));
    }
    return structScalarConstructor(fieldnames, values);
}
//=============================================================================
ArrayOf
ArrayOf::structScalarConstructor(const stringVector& fNames, const ArrayOfVector& values)
{
    const ArrayOf* rptr;
    Dimensions dims;
    ArrayOf* qp = nullptr;
    try {
        if (fNames.size() != values.size()) {
            Error(
                _W("Number of field names must match number of values in structure constructor."));
        }
        Dimensions dims(1, 1);
        qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fNames, false);
        /**
         * Work through the values, and copy the values back one at a time.
         */
        indexType length = dims.getElementCount();
        indexType offset = 0;
        for (indexType j = 0; j < length; j++) {
            for (indexType i = 0; i < (indexType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                rptr = (const ArrayOf*)rval.dp->getData();
                qp[offset] = rval;
                offset++;
            }
        }
        return ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fNames);
    } catch (const Exception&) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::structConstructor(const stringVector& fNames, const ArrayOfVector& values)
{
    const ArrayOf* rptr;
    Dimensions dims;
    indexType i, j;
    ArrayOf* qp = nullptr;
    try {
        if (fNames.size() != values.size()) {
            Error(
                _W("Number of field names must match number of values in structure constructor."));
        }
        /**
         * First, we have to make sure that each entry of "values" have
         *  1.  cell arrays of the same size,
         *  2.  single element cell arrays,
         *  3.  single values.
         */
        bool nonSingularFound = false;
        for (i = 0; i < (indexType)values.size(); i++) {
            /**
             * Check the type of the entry.  If its a non-cell array, then
             * then ignore this entry.
             */
            ArrayOf value = values[i];
            if (value.dp->dataClass == NLS_CELL_ARRAY || value.dp->dataClass == NLS_STRING_ARRAY) {
                /**
                 * This is a cell-array, so look for non-scalar cell-arrays.
                 */
                if (!value.isScalar()) {
                    if (!nonSingularFound) {
                        nonSingularFound = true;
                        dims = value.dp->dimensions;
                    } else if (!dims.equals(value.dp->dimensions)) {
                        Error(_W("ArrayOf dimensions of non-scalar entries must agree in "
                                 "structure construction."));
                    }
                }
            }
        }
        /**
         * At this point we can construct the dimensions of the output.
         */
        if (!nonSingularFound) {
            dims.reset();
            dims[0] = 1;
            dims[1] = 1;
        }
        /**
         * The dimensions of the object have been identified.  Set the
         * dimensions of the object and the field names.  Then allocate
         * the space.
         */
        qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fNames, false);
        /**
         * Work through the values, and copy the values back one at a time.
         */
        indexType length = dims.getElementCount();
        indexType offset = 0;
        for (j = 0; j < length; j++)
            for (i = 0; i < (indexType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                rptr = (const ArrayOf*)rval.dp->getData();
                if (rval.dp->dataClass == NLS_CELL_ARRAY) {
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
        return ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fNames);
    } catch (const Exception&) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::structConstructor(const wstringVector& fNames, const ArrayOfVector& values)
{
    stringVector fNamesUtf8;
    for (const auto& fName : fNames) {
        fNamesUtf8.push_back(wstring_to_utf8(fName));
    }
    return ArrayOf::structConstructor(fNamesUtf8, values);
}
//=============================================================================
bool
ArrayOf::isField(const std::string& fieldName) const
{
    if (!this->isStruct() && !this->isClassType()) {
        return false;
    }
    stringVector fieldnames = getFieldNames();
    for (const auto& name : fieldnames) {
        if (name == fieldName) {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::getField(const std::string& fieldName) const
{
    // First make sure that we are a scalar value.
    if (!isScalar()) {
        Error(_W("Cannot dereference a field of a multi-element structure array."));
    }
    if (isSparse()) {
        Error(_W("getField not supported for sparse arrays."));
    }
    // Then, find the field index.
    int64 ndx = getFieldIndex(fieldName);
    if (ndx < 0) {
        Error(_("Reference to non-existent field") + " " + fieldName);
    }
    // Cast real part into a data pointer, and return a copy of what the caller asked for!
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    return qp[ndx];
}
//=============================================================================
ArrayOfVector
ArrayOf::getFieldAsList(const std::string& fieldName) const
{
    if (!(isStruct() || isClassType())) {
        Error(_W("Attempt to apply field-indexing to non structure-array object."));
    }
    if (isSparse()) {
        Error(_W("getFieldAsList not supported for sparse arrays."));
    }
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    indexType N = getElementCount();
    indexType fieldCount = dp->fieldNames.size();
    int64 ndx = getFieldIndex(fieldName);
    if (ndx < 0) {
        Error(_("Reference to non-existent field") + " " + fieldName);
    }
    ArrayOfVector m;
    for (indexType i = 0; i < N; i++) {
        m.push_back(qp[i * fieldCount + ndx]);
    }
    return m;
}
//=============================================================================
/**
 * Set the contents of a field in a structure.
 */
void
ArrayOf::setField(const std::string& fieldName, ArrayOf& data)
{
    if (isEmpty()) {
        stringVector newNames(dp->fieldNames);
        newNames.push_back(fieldName);
        promoteType(NLS_STRUCT_ARRAY, newNames);
        Dimensions a;
        a[0] = 1;
        a[1] = 1;
        resize(a);
    }
    if (isSparse()) {
        Error(_W("setField not supported for sparse arrays."));
    }
    if (dp->dataClass != NLS_STRUCT_ARRAY && dp->dataClass != NLS_CLASS_ARRAY
        && dp->dataClass != NLS_FUNCTION_HANDLE) {
        Error(ERROR_ASSIGN_TO_NON_STRUCT);
    }
    if (!isScalar()) {
        Error(_W("Cannot apply A.field_name = B to multi-element structure array A."));
    }
    double field_ndx = (double)getFieldIndex(fieldName);
    if (field_ndx == -1) {
        field_ndx = (double)insertFieldName(fieldName);
    }
    ArrayOf* sp = (ArrayOf*)getReadWriteDataPointer();
    sp[(indexType)field_ndx] = data;
    dp->dimensions.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * Set the contents of a field in a structure.
 */
void
ArrayOf::setFieldAsList(const std::string& fieldName, ArrayOfVector& data)
{
    if (isSparse()) {
        Error(_W("setFieldAsList not supported for sparse arrays."));
    }
    if (isEmpty()) {
        bool bFieldAlreadyExist = false;
        for (auto& k : dp->fieldNames) {
            if (fieldName == k) {
                bFieldAlreadyExist = true;
                break;
            }
        }
        stringVector names(dp->fieldNames);
        if (!bFieldAlreadyExist) {
            names.push_back(fieldName);
        }
        promoteType(NLS_STRUCT_ARRAY, names);
        Dimensions a;
        a[0] = 1;
        a[1] = 1;
        resize(a);
    }
    if (!this->isStruct() && !this->isClassType()) {
        Error(ERROR_ASSIGN_TO_NON_STRUCT);
    }
    if ((int)data.size() < getElementCount()) {
        Error(_W("Not enough right hand values to satisfy left hand side expression."));
    }
    indexType indexLength = getElementCount();
    int64 field_ndx = getFieldIndex(fieldName);
    if (field_ndx == -1) {
        field_ndx = insertFieldName(fieldName);
    }
    indexType fieldCount = dp->fieldNames.size();
    ArrayOf* qp = (ArrayOf*)getReadWriteDataPointer();
    for (indexType i = 0; i < indexLength; i++) {
        qp[i * fieldCount + field_ndx] = data.front();
        qp[i * fieldCount + field_ndx].name("");
        data.pop_front();
    }
    dp->dimensions.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * Add another fieldname to our structure array.
 */
indexType
ArrayOf::insertFieldName(const std::string& fieldName)
{
    if (isSparse()) {
        Error(_W("insertFieldName not supported for sparse arrays."));
    }
    stringVector names(dp->fieldNames);
    names.push_back(fieldName);
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    ArrayOf* rp = (ArrayOf*)allocateArrayOf(dp->dataClass, getElementCount(), names, false);
    std::string classtype = dp->getClassTypeName();
    indexType fN = names.size();
    if (fN > 1) {
        ompIndexType nN = (ompIndexType)fN - 1;
        OMP_PARALLEL_FOR_LOOP(nN)
        for (ompIndexType i = 0; i < nN; i++) {
            rp[i] = qp[i];
        }
    }
    dp = dp->putData(NLS_STRUCT_ARRAY, dp->dimensions, rp, false, names);
    dp->setClassTypeName(classtype);
    return (fN - 1);
}
//=============================================================================
/**
 * Compute the ordinal index for a given field name from a
 * structure when the list of field names is given.
 */
int64
ArrayOf::getFieldIndexFromList(const std::string& fName, const stringVector& fieldNames) const
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
/**
 * Compute the ordinal index for a given field name from a
 * structure using the current set of field names.
 */
int64
ArrayOf::getFieldIndex(const std::string& fName) const
{
    return getFieldIndexFromList(fName, dp->fieldNames);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStructWithoutFields()
{
    stringVector fieldnames;
    fieldnames.clear();
    ArrayOfVector fieldvalues;
    fieldvalues.clear();
    return ArrayOf::structConstructor(fieldnames, fieldvalues);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStructConstructor(const stringVector& fNames, Dimensions& dim)
{
    if (dim.getElementCount() != 0) {
        Error(_W("Invalid dimensions."));
    }
    ArrayOf* qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dim.getElementCount(), fNames, false);
    return ArrayOf(NLS_STRUCT_ARRAY, dim, qp, false, fNames);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStructConstructor(const wstringVector& fNames, Dimensions& dim)
{
    stringVector fs;
    fs.reserve(fNames.size());
    for (const auto& fName : fNames) {
        fs.push_back(wstring_to_utf8(fName));
    }
    return ArrayOf::emptyStructConstructor(fs, dim);
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
    if (dp->fieldNames.size() != newFieldnames.size()) {
        return false;
    }
    dp->fieldNames = newFieldnames;
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
