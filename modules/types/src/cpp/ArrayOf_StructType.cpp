//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "Exception.hpp"
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
bool
ArrayOf::isClassStruct() const
{
    if (this->isStruct()) {
        return (getStructType() != NLS_STRUCT_ARRAY_STR);
    }
    return false;
}
//=============================================================================
stringVector
ArrayOf::getFieldNames() const
{
    if (dp) {
        return dp->fieldNames;
    } else {
        return stringVector();
    }
}
//=============================================================================
std::string
ArrayOf::getStructType() const
{
    if (dp->dataClass != NLS_STRUCT_ARRAY) {
        Error(ERROR_TYPE_STRUCT_EXPECTED);
    }
    return dp->getStructTypeName();
}
//=============================================================================
void
ArrayOf::setStructType(std::wstring structname)
{
    ArrayOf::setStructType(wstring_to_utf8(structname));
}
//=============================================================================
void
ArrayOf::setStructType(std::string structname)
{
    if (this->getDataClass() != NLS_STRUCT_ARRAY) {
        Error(ERROR_TYPE_STRUCT_EXPECTED);
    }
    dp->setStructTypeName(structname);
}
//=============================================================================
ArrayOf
ArrayOf::structScalarConstructor(stringVector fNames, ArrayOfVector& values)
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
        qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fNames);
        /**
         * Work through the values, and copy the values back one at a time.
         */
        indexType length = dims.getElementCount();
        indexType offset = 0;
        for (sizeType j = 0; j < length; j++) {
            for (sizeType i = 0; i < (sizeType)fNames.size(); i++) {
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
ArrayOf::structConstructor(stringVector fNames, ArrayOfVector& values)
{
    const ArrayOf* rptr;
    Dimensions dims;
    sizeType i, j;
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
        for (i = 0; i < (sizeType)values.size(); i++) {
            /**
             * Check the type of the entry.  If its a non-cell array, then
             * then ignore this entry.
             */
            if (values[i].dp->dataClass == NLS_CELL_ARRAY
                || values[i].dp->dataClass == NLS_STRING_ARRAY) {
                /**
                 * This is a cell-array, so look for non-scalar cell-arrays.
                 */
                if (!values[i].isScalar()) {
                    if (!nonSingularFound) {
                        nonSingularFound = true;
                        dims = values[i].dp->dimensions;
                    } else if (!dims.equals(values[i].dp->dimensions)) {
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
        qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fNames);
        /**
         * Work through the values, and copy the values back one at a time.
         */
        indexType length = dims.getElementCount();
        indexType offset = 0;
        for (j = 0; j < length; j++)
            for (i = 0; i < (sizeType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                rptr = (const ArrayOf*)rval.dp->getData();
                if (rval.dp->dataClass == NLS_CELL_ARRAY
                    || rval.dp->dataClass == NLS_STRING_ARRAY) {
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
ArrayOf::structConstructor(wstringVector fNames, ArrayOfVector& values)
{
    stringVector fNamesUtf8;
    for (size_t k = 0; k < fNames.size(); k++) {
        fNamesUtf8.push_back(wstring_to_utf8(fNames[k]));
    }
    return ArrayOf::structConstructor(fNamesUtf8, values);
}
//=============================================================================
ArrayOf
ArrayOf::getField(std::string fieldName)
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
ArrayOf::getFieldAsList(std::string fieldName)
{
    if (!isStruct()) {
        Error(_W("Attempt to apply field-indexing to non structure-array object."));
    }
    if (isSparse()) {
        Error(_W("getFieldAsList not supported for sparse arrays."));
    }
    ArrayOfVector m;
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    indexType N = getLength();
    indexType fieldCount = dp->fieldNames.size();
    int64 ndx = getFieldIndex(fieldName);
    if (ndx < 0) {
        Error(_("Reference to non-existent field") + " " + fieldName);
    }
    indexType i = 0;
    for (i = 0; i < N; i++) {
        m.push_back(qp[i * fieldCount + ndx]);
    }
    return m;
}
//=============================================================================
/**
 * Set the contents of a field in a structure.
 */
void
ArrayOf::setField(std::string fieldName, ArrayOf& data)
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
    if (dp->dataClass != NLS_STRUCT_ARRAY) {
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
}
//=============================================================================
/**
 * Set the contents of a field in a structure.
 */
void
ArrayOf::setFieldAsList(std::string fieldName, ArrayOfVector& data)
{
    if (isSparse()) {
        Error(_W("setFieldAsList not supported for sparse arrays."));
    }
    if (isEmpty()) {
        bool bFieldAlreadyExist = false;
        for (size_t k = 0; k < dp->fieldNames.size(); k++) {
            if (fieldName == dp->fieldNames[k]) {
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
        //       dp = dp->putData(NLS_STRUCT_ARRAY,dp->getDimensions(),NULL,names);
        //       return;
    }
    if (!this->isStruct()) {
        Error(ERROR_ASSIGN_TO_NON_STRUCT);
    }
    if ((int)data.size() < getLength()) {
        Error(_W("Not enough right hand values to satisfy left hand side expression."));
    }
    indexType indexLength = getLength();
    int64 field_ndx = getFieldIndex(fieldName);
    if (field_ndx == -1) {
        field_ndx = insertFieldName(fieldName);
    }
    indexType fieldCount = dp->fieldNames.size();
    ArrayOf* qp = (ArrayOf*)getReadWriteDataPointer();
    for (indexType i = 0; i < indexLength; i++) {
        qp[i * fieldCount + field_ndx] = data.front();
        //      data.pop_front();
        data.erase(data.begin());
    }
    dp->dimensions.simplify();
}
//=============================================================================
/**
 * Add another fieldname to our structure array.
 */
indexType
ArrayOf::insertFieldName(std::string fieldName)
{
    if (isSparse()) {
        Error(_W("insertFieldName not supported for sparse arrays."));
    }
    stringVector names(dp->fieldNames);
    names.push_back(fieldName);
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    ArrayOf* rp = (ArrayOf*)allocateArrayOf(dp->dataClass, getLength(), names);
    std::string classtype = dp->getStructTypeName();
    indexType fN = names.size();
    for (indexType i = 0; i < fN - 1; i++) {
        rp[i] = qp[i];
    }
    dp = dp->putData(NLS_STRUCT_ARRAY, dp->dimensions, rp, false, names);
    dp->setStructTypeName(classtype);
    return (fN - 1);
}
//=============================================================================
/**
 * Compute the ordinal index for a given field name from a
 * structure when the list of field names is given.
 */
int64
ArrayOf::getFieldIndexFromList(std::string fName, const stringVector& fieldNames)
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
    } else {
        return -1;
    }
}
//=============================================================================
/**
 * Compute the ordinal index for a given field name from a
 * structure using the current set of field names.
 */
int64
ArrayOf::getFieldIndex(std::string fName)
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
ArrayOf::emptyStructConstructor(stringVector fNames, Dimensions& dim)
{
    if (dim.getElementCount() != 0) {
        Error(_W("Invalid dimensions."));
    }
    ArrayOf* qp = (ArrayOf*)allocateArrayOf(NLS_STRUCT_ARRAY, dim.getElementCount(), fNames);
    return ArrayOf(NLS_STRUCT_ARRAY, dim, qp, false, fNames);
}
//=============================================================================
ArrayOf
ArrayOf::emptyStructConstructor(wstringVector fNames, Dimensions& dim)
{
    stringVector fs;
    fs.reserve(fNames.size());
    for (size_t k = 0; k < fNames.size(); k++) {
        fs.push_back(wstring_to_utf8(fNames[k]));
    }
    return ArrayOf::emptyStructConstructor(fs, dim);
}
//=============================================================================
bool
ArrayOf::haveValidFieldNames(stringVector fieldnames)
{
    if (fieldnames.empty()) {
        return true;
    }
    for (size_t k = 0; k < fieldnames.size(); ++k) {
        if (fieldnames[k].size() == 0) {
            return false;
        }
        if (boost::algorithm::contains(fieldnames[k], "\n")) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
ArrayOf::haveUniqueFieldNames(stringVector fieldnames)
{
    stringVector copyVector(fieldnames);
    if (fieldnames.size() > 1) {
        std::sort(copyVector.begin(), copyVector.end());
        copyVector.erase(std::unique(copyVector.begin(), copyVector.end()), copyVector.end());
        return fieldnames.size() == copyVector.size();
    }
    return true;
}
//=============================================================================
}
//=============================================================================
