//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4724)
#endif
//=============================================================================
#include <cstring>
#include "lapack_eigen.hpp"
#include <Eigen/src/misc/lapacke.h>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <limits>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "IEEEFP.hpp"
#include "SparseDynamicFunctions.hpp"
#include "SparseType.hpp"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
#ifdef _MSC_VER
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static int objectBalance;
//=============================================================================
ArrayOfVector
scalarArrayOfToArrayOfVector(ArrayOf a)
{
    ArrayOfVector retval(1);
    retval << a;
    return retval;
}
//=============================================================================
std::wstring
ArrayOf::wname() const
{
    return utf8_to_wstring(_name);
}
//=============================================================================

std::string
ArrayOf::name() const
{
    return _name;
}
//=============================================================================
void
ArrayOf::name(const std::string& name)
{
    _name = name;
}
//=============================================================================
void
ArrayOf::copyObject(const ArrayOf& copy)
{
    this->_name = copy._name;
    if (copy.dp) {
        dp = copy.dp->getCopy();
    } else {
        dp = nullptr;
    }
}
//=============================================================================
inline void
ArrayOf::deleteContents(void)
{
    if (dp) {
        if (dp->deleteCopy() <= 1) {
            delete dp;
        }
        dp = nullptr;
    }
}
//=============================================================================
bool*
ArrayOf::getBinaryMap(indexType maxD)
{
    bool* map = new_with_exception<bool>(maxD, true);
    indexType N = getElementCount();
    constIndexPtr rp = (constIndexPtr)dp->getData();
    for (indexType i = 0; i < N; i++) {
        indexType n = (rp[i] - 1);
#ifdef NLS_INDEX_TYPE_64
        if (n >= maxD)
#else
        if (n < 0 || n >= maxD)
#endif
        {
            delete[] map;
            map = nullptr;
            Error(_W("Matrix index is out of range."));
        }
        if (map) {
            map[n] = true;
        }
    }
    return map;
}
//=============================================================================
indexType
ArrayOf::getMaxAsIndex()
{
    indexType maxval;
    constIndexPtr rp = (constIndexPtr)dp->getData();
    indexType K = getElementCount();
    maxval = rp[0];
    for (indexType k = 1; k < K; k++) {
        if (rp[k] > maxval) {
            maxval = rp[k];
        }
    }
    if (maxval <= 0) {
        Error(_W("Illegal zero or negative index"));
    }
    return maxval;
}
//=============================================================================
void
ArrayOf::toOrdinalType()
{
    if (isSparse()) {
        makeDense();
    }
    switch (dp->dataClass) {
    case NLS_LOGICAL: {
        // We make a first pass through the array, and count the number of
        // non-zero entries.
        const logical* rp = (const logical*)dp->getData();
        int indexCount = 0;
        indexType len = getElementCount();
        for (ompIndexType i = 0; i < (ompIndexType)len; i++)
            if (rp[i] != 0) {
                indexCount++;
            }
        // Allocate space to hold the new type.
        indexType* lp = new_with_exception<indexType>(indexCount, false);
        indexType* qp = lp;
        for (ompIndexType i = 0; i < (ompIndexType)len; i++)
            if (rp[i] != 0) {
                *qp++ = (indexType)(i + 1);
            }
        // Reset our data pointer to the new vector.
        Dimensions dimensions;
        if (isRowVector()) {
            dimensions[1] = indexCount;
            dimensions[0] = 1;
        } else {
            dimensions[1] = 1;
            dimensions[0] = indexCount;
        }
        // Change the class to an NLS_UINT32.
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dimensions, lp);
#else
        dp = dp->putData(NLS_UINT32, dimensions, lp);
#endif
    } break;
    case NLS_CHAR: {
        Error(_W("Cannot convert string data types to indices."));
    } break;
    case NLS_DCOMPLEX: {
        Warning(_W("Imaginary part of complex index ignored.\n"));
        // We convert complex values into real values
        const double* rp = (const double*)dp->getData();
        indexType len = getElementCount();
        indexType ndx = 0;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (indexType i = 0; i < len; i++) {
            ndx = (indexType)rp[i << 1];
            if ((double)ndx != rp[i << 1]) {
                Error(_W("index must either be real positive integers or logicals."));
            }
            if (ndx <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_SCOMPLEX: {
        Warning("Imaginary part of complex index ignored.\n");
        // We convert complex values into real values
        const single* rp = (const single*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (indexType i = 0; i < len; i++) {
            ndx = (indexType)rp[i << 1];
            if ((double)ndx != rp[i << 1]) {
                Error(_W("index must either be real positive integers or logicals."));
            }
            if (ndx <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_DOUBLE: {
        const double* rp = (const double*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (indexType i = 0; i < len; i++) {
            ndx = (indexType)rp[i];
            if ((double)ndx != rp[i]) {
                Error(_W("index must either be real positive integers or logicals."));
            }
            if (ndx <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_SINGLE: {
        const single* rp = (const single*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (indexType i = 0; i < len; i++) {
            ndx = (indexType)rp[i];
            if ((double)ndx != rp[i]) {
                Error(_W("index must either be real positive integers or logicals."));
            }
            if (ndx <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_INT64: {
        const int64* rp = (const int64*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (indexType i = 0; i < len; i++) {
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            ndx = (indexType)rp[i];
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_UINT64: {
        const uint64* rp = (const uint64*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            if (rp[i] > std::numeric_limits<indexType>::max()) {
                Error(_W("Too big index encountered."));
            }
            ndx = (indexType)rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_INT32: {
        const int32* rp = (const int32*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            ndx = rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_UINT32: {
        const uint32* rp = (const uint32*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            ndx = rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_INT16: {
        const int16* rp = (const int16*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            ndx = rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_UINT16: {
        const uint16* rp = (const uint16*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            ndx = rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_INT8: {
        const int8* rp = (const int8*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            ndx = rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_UINT8: {
        const uint8* rp = (const uint8*)dp->getData();
        indexType len = getElementCount();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len, false);
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            ndx = rp[i];
            if (rp[i] <= 0) {
                Error(_W("Zero or negative index encountered."));
            }
            lp[i] = ndx;
        }
#ifdef NLS_INDEX_TYPE_64
        dp = dp->putData(NLS_UINT64, dp->getDimensions(), lp);
#else
        dp = dp->putData(NLS_UINT32, dp->getDimensions(), lp);
#endif
    } break;
    case NLS_GO_HANDLE: {
        Error(_W("Cannot convert handle arrays to indices."));
    } break;
    case NLS_HANDLE: {
        Error(_W("Cannot convert handle arrays to indices."));
    } break;
    case NLS_CELL_ARRAY: {
        Error(_W("Cannot convert cell arrays to indices."));
    } break;
    case NLS_STRING_ARRAY: {
        Error(_W("Cannot convert string arrays to indices."));
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(_W("Cannot convert structure arrays to indices."));
    } break;
    default: {
        Error(_W("Cannot convert unknown type to indices."));
    } break;
    }
}
//=============================================================================
ArrayOf::ArrayOf()
{
    Dimensions dims(0, 0);
    dp = nullptr;
}
//=============================================================================
/**
 * Create a variable with the specified contents.
 */
ArrayOf::ArrayOf(
    Class type, const Dimensions& dims, void* data, bool sparse, const stringVector& fnames)
{
    dp = new Data(type, dims, data, sparse, fnames);
}
//=============================================================================
ArrayOf::ArrayOf(Class type) { dp = new Data(type, Dimensions(0, 0), nullptr); }
//=============================================================================
/**
 * Destructor - free the data object.
 */
ArrayOf::~ArrayOf() { deleteContents(); }
//=============================================================================
void
ArrayOf::operator=(const ArrayOf& copy)
{
    if (this == &copy) {
        return;
    }
    this->_name = copy._name;
    deleteContents();
    if (copy.dp) {
        dp = copy.dp->getCopy();
    } else {
        dp = nullptr;
    }
}
//=============================================================================
indexType
ArrayOf::getReferenceCount() const
{
    if (dp) {
        return dp->numberOfOwners();
    } else {
        return 0;
    }
}
//=============================================================================
indexType
ArrayOf::getRows() const
{
    if (dp) {
        return dp->getRows();
    } else {
        return 0;
    }
}
//=============================================================================
indexType
ArrayOf::getColumns() const
{
    if (dp) {
        return dp->getColumns();
    } else {
        return 0;
    }
}
//=============================================================================
indexType
ArrayOf::nDims() const
{
    if (dp) {
        return dp->dimensions.getLength();
    } else {
        return 0;
    }
}
//=============================================================================
indexType
ArrayOf::getElementCount() const
{
    if (dp) {
        return dp->getElementCount();
    } else {
        return 0;
    }
}
//=============================================================================
Dimensions
ArrayOf::getDimensions() const
{
    if (dp) {
        return dp->dimensions;
    } else {
        return Dimensions();
    }
}
//=============================================================================
indexType
ArrayOf::getDimensionLength(int t) const
{
    if (dp) {
        return dp->dimensions[t];
    } else {
        return 0;
    }
}
//=============================================================================
const void*
ArrayOf::getDataPointer() const
{
    if (isSparse()) {
        Error(_W("operation does not support sparse matrix arguments."));
    }
    if (dp) {
        return dp->getData();
    } else {
        return nullptr;
    }
}
//=============================================================================
void
ArrayOf::ensureSingleOwner()
{
    if (dp->numberOfOwners() > 1) {
        if (!dp->sparse) {
            std::string currentStructType = dp->getStructTypeName();
            void* np = allocateArrayOf(dp->dataClass, getElementCount(), dp->fieldNames, false);
            if (isEmpty()) {
                Dimensions dim = dp->getDimensions();
                dp = dp->putData(dp->dataClass, dim, np, dp->sparse, dp->fieldNames);
            } else {
                copyElements(0, np, 0, getElementCount());
                dp = dp->putData(dp->dataClass, dp->dimensions, np, dp->sparse, dp->fieldNames);
            }
            dp->setStructTypeName(currentStructType);
        } else {
            dp = dp->putData(dp->dataClass, dp->dimensions,
                CopySparseMatrixDynamicFunction(
                    dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData()),
                dp->sparse, dp->fieldNames);
        }
    }
}
//=============================================================================
void*
ArrayOf::getReadWriteDataPointer()
{
    if (isSparse()) {
        Warning(_W("Warning: sparse matrix converted to full for operation."));
        makeDense();
    }
    ensureSingleOwner();
    return dp->getWriteableData();
}
//=============================================================================
void
ArrayOf::setDataPointer(void* rp)
{
    dp = dp->putData(dp->dataClass, dp->dimensions, rp, dp->sparse, dp->fieldNames);
}
//=============================================================================
void
ArrayOf::resize(Dimensions& a)
{
    Dimensions newSize;
    // Make a copy of the current dimension vector, and
    // compute the new dimension size.
    newSize = dp->dimensions;
    newSize.expandToCover(a);
    // Check to see if the dimensions are unchanged.
    if (newSize.equals(dp->dimensions)) {
        return;
    }
    // Check to see if the total number of elements is unchanged.
    if (newSize.getElementCount() == getElementCount()) {
        ensureSingleOwner();
        dp->dimensions = newSize;
        dp->refreshDimensionCache();
        return;
    }
    if (isSparse()) {
        Error(_W("Cannot resize sparse arrays."));
    }
    // Allocate space for our new size.
    void* dst_data
        = allocateArrayOf(dp->dataClass, newSize.getElementCount(), dp->fieldNames, true);
    if (!isEmpty()) {
        // Initialize a pointer to zero.
        Dimensions curPos(dp->dimensions.getLength());
        // Because we copy & convert data a column at a time, we retrieve
        // the number of rows in each column.
        indexType rowCount = dp->dimensions[0];
        // Track our offset into the original data.
        indexType srcIndex = 0;
        while (curPos.inside(dp->dimensions)) {
            // Get the destination index for the current source position.
            indexType dstIndex = newSize.mapPoint(curPos);
            // Copy the data from our original data structure to the
            // new data structure, starting from the source index
            // srcIndex, and moving to dstIndex.
            copyElements(srcIndex, dst_data, dstIndex, rowCount);
            // Update the column number (as we have just copied an
            // entire column).
            curPos.incrementModulo(dp->dimensions, 1);
            // Advance the source data pointer so that it points to the
            // start of the next column.
            srcIndex += rowCount;
        }
    }
    dp = dp->putData(dp->dataClass, newSize, dst_data, dp->sparse, dp->fieldNames);
}
//=============================================================================
void
ArrayOf::vectorResize(indexType max_index)
{
    if (max_index > getElementCount()) {
        Dimensions newDim;
        if (isEmpty() || dp->isScalar()) {
            newDim.reset();
            newDim[0] = 1;
            newDim[1] = max_index;
        } else if (dp->isVector()) {
            newDim = dp->dimensions;
            if (dp->dimensions[0] != 1) {
                newDim[0] = max_index;
            } else {
                newDim[1] = max_index;
            }
        } else {
            // First reshape it
            Dimensions tDim(2);
            tDim[0] = 1;
            tDim[1] = getElementCount();
            reshape(tDim);
            newDim.reset();
            newDim[0] = 1;
            newDim[1] = max_index;
        }
        resize(newDim);
    }
}
//=============================================================================
/**
 * Reshape an array.  This is only legal if the number of
 * elements remains the same after reshaping.
 */
void
ArrayOf::reshape(Dimensions& a, bool checkValidDimension)
{
    if (isClassStruct()) {
        Error(_W("Reshape operation not allowed for overloaded type."));
    }
    if (isFunctionHandle()) {
        Error(_W("Reshape operation not allowed for 'function_handle' type."));
    }
    if (checkValidDimension) {
        if (a.getElementCount() != getElementCount()) {
            Error(_W("Reshape operation cannot change the number of elements in array."));
        }
    }
    if (isSparse()) {
        if (a.is2D() || a.isVector() || a.isScalar()) {
            void* reshapedSparseMatrix = ReshapeSparseMatrixDynamicFunction(
                dp->dataClass, dp->dimensions[0], dp->dimensions[1], a[0], a[1], dp->getData());
            dp = dp->putData(dp->dataClass, a, reshapedSparseMatrix, true);
            dp->dimensions = a;
            dp->refreshDimensionCache();
        } else {
            Error(_W("Reshape operation not allowed with N Dimensions sparse arrays."));
        }
    } else {
        ensureSingleOwner();
        dp->dimensions = a;
        dp->refreshDimensionCache();
    }
}
//=============================================================================
void
ArrayOf::changeInPlaceDimensions(const Dimensions& a)
{
    if (isClassStruct()) {
        Error(_W("changeDimensions operation not allowed for overloaded type."));
    }
    if (isFunctionHandle()) {
        Error(_W("changeDimensions operation not allowed for 'function_handle' type."));
    }
    if (a.getElementCount() != getElementCount()) {
        Error(_W("changeDimensions operation cannot change the number of elements in array."));
    }
    dp->dimensions = a;
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * Get our data class (of type Class).
 */
Class
ArrayOf::getDataClass() const
{
    return (dp == nullptr) ? NLS_DOUBLE : dp->dataClass;
}
//=============================================================================
/**
 * Calculate the size of each element in this array.
 */
indexType
ArrayOf::getElementSize() const
{
    if (dp == nullptr) {
        Error(_W("Invalid data class."));
    }
    switch (dp->dataClass) {
    case NLS_GO_HANDLE:
        return sizeof(nelson_handle);
    case NLS_HANDLE:
        return sizeof(nelson_handle);
    case NLS_STRING_ARRAY:
        return sizeof(ArrayOf);
    case NLS_CELL_ARRAY:
        return sizeof(ArrayOf);
    case NLS_STRUCT_ARRAY:
        return (sizeof(ArrayOf) * dp->fieldNames.size());
    case NLS_LOGICAL:
        return sizeof(logical);
    case NLS_UINT8:
        return sizeof(uint8);
    case NLS_INT8:
        return sizeof(int8);
    case NLS_UINT16:
        return sizeof(uint16);
    case NLS_INT16:
        return sizeof(int16);
    case NLS_UINT32:
        return sizeof(uint32);
    case NLS_INT32:
        return sizeof(int32);
    case NLS_UINT64:
        return sizeof(uint64);
    case NLS_INT64:
        return sizeof(int64);
    case NLS_SINGLE:
        return sizeof(single);
    case NLS_DOUBLE:
        return sizeof(double);
    case NLS_SCOMPLEX:
        return sizeof(single) * 2;
    case NLS_DCOMPLEX:
        return sizeof(double) * 2;
    case NLS_CHAR:
        return sizeof(charType);
    default: { } break; }
    return 0;
}
//=============================================================================
/**
 * Calculate the total number of bytes required to store this array.
 */
indexType
ArrayOf::getByteSize() const
{
    if (isSparse()) {
        Error(_W("Byte size calculation not supported for sparse arrays."));
    }
    return getElementSize() * getElementCount();
}
//=============================================================================
/**
 * Returns true if we are positive.
 */
template <class T>
bool
isTPositive(const void* data, indexType len)
{
    const T* qp = (const T*)data;
    for (indexType i = 0; i < len; ++i) {
        if (qp[i] < 0) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
ArrayOf::isPositive() const
{
    if (isSparse()) {
        switch (dp->dataClass) {
        case NLS_DCOMPLEX:
            return false;
        case NLS_DOUBLE: {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)dp->getData();
            return isTPositive<double>(spMat->valuePtr(), spMat->nonZeros());
        } break;
        default:
            return false;
        }
    }

    switch (dp->dataClass) {
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
        return true;
    case NLS_SINGLE:
        return isTPositive<single>(dp->getData(), getElementCount());
    case NLS_DOUBLE:
        return isTPositive<double>(dp->getData(), getElementCount());
    case NLS_INT8:
        return isTPositive<int8>(dp->getData(), getElementCount());
    case NLS_INT16:
        return isTPositive<int16>(dp->getData(), getElementCount());
    case NLS_INT32:
        return isTPositive<int32>(dp->getData(), getElementCount());
    case NLS_INT64:
        return isTPositive<int64>(dp->getData(), getElementCount());
    case NLS_NOT_TYPED:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_LOGICAL:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
    default: {
        return false;
    } break;
    }
    return false;
}
//=============================================================================
#define caseMacroReal(caseLabel, type)                                                             \
    case caseLabel:                                                                                \
        retval = (*((const type*)x_dp) == *((const type*)y_dp));                                   \
        break;
//=============================================================================
#define caseMacroComplex(caseLabel, type)                                                          \
    case caseLabel:                                                                                \
        retval = (((const type*)x_dp)[0] == ((const type*)y_dp)[0])                                \
            && (((const type*)x_dp)[1] == ((const type*)y_dp)[1]);                                 \
        break;
//=============================================================================
bool
ArrayOf::testCaseMatchScalar(ArrayOf x) const
{
    if (isSparse()) {
        Error(_W("isPositive not supported for sparse arrays."));
    }
    // Now we have to compare ourselves to the argument.  Check for the
    // case that we are a string type
    if (isRowVectorCharacterArray()) {
        // If x is not a string, we cannot match
        if (!x.isRowVectorCharacterArray()) {
            return false;
        }
        // if x is a string do a string, string compare.
        std::wstring s1 = getContentAsWideString();
        std::wstring s2 = x.getContentAsWideString();
        bool retval = (s1.compare(s2) == 0);
        return retval;
    }
    if (!x.isScalar()) {
        return false;
    }
    //  OK - we are not a string, so we have a numerical comparison.  To do this,
    // we have to make both objects the same type.
    ArrayOf y = *this;
    if (x.getDataClass() > y.getDataClass()) {
        y.promoteType(x.getDataClass());
    } else {
        x.promoteType(y.getDataClass());
    }
    // Finally, we can do a compare....
    const void* x_dp = x.dp->getData();
    const void* y_dp = y.dp->getData();
    bool retval = false;
    switch (x.dp->dataClass) {
    default: {
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CHAR:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_STRUCT_ARRAY:
        retval = false;
        break;
        caseMacroReal(NLS_LOGICAL, logical);
        caseMacroReal(NLS_UINT8, uint8);
        caseMacroReal(NLS_INT8, int8);
        caseMacroReal(NLS_UINT16, uint16);
        caseMacroReal(NLS_INT16, int16);
        caseMacroReal(NLS_UINT32, uint32);
        caseMacroReal(NLS_INT32, int32);
        caseMacroReal(NLS_UINT64, uint64);
        caseMacroReal(NLS_INT64, int64);
        caseMacroReal(NLS_SINGLE, single);
        caseMacroReal(NLS_DOUBLE, double);
        caseMacroComplex(NLS_SCOMPLEX, single);
        caseMacroComplex(NLS_DCOMPLEX, double);
    }
    return retval;
}
#undef caseMacroReal
#undef caseMacroComplex
//=============================================================================
bool
ArrayOf::testForCaseMatch(ArrayOf x) const
{
    if (isSparse()) {
        Error(_W("isPositive not supported for sparse arrays."));
    }
    // We had better be a scalar
    if (!(isScalar() || isCharacterArray())) {
        Error(_W("Switch argument must be a scalar or a string"));
    }
    // And we had better not be a reference type
    if (isReferenceType()) {
        Error(_W("Switch argument cannot be a reference type (struct or cell array)"));
    }
    // If x is a scalar, we just need to call the scalar version
    if (x.isScalar() || x.isRowVectorCharacterArray()) {
        return testCaseMatchScalar(x);
    }
    if (x.dp->dataClass != NLS_CELL_ARRAY && x.dp->dataClass != NLS_STRING_ARRAY) {
        Error(_W("Case arguments must either be a scalar or a cell array"));
    }
    const ArrayOf* qp = (const ArrayOf*)x.dp->getData();
    indexType len = x.getElementCount();
    bool foundMatch = false;
    indexType i = 0;
    while (i < len && !foundMatch) {
        foundMatch = testCaseMatchScalar(qp[i]);
        i++;
    }
    return foundMatch;
}
//=============================================================================
/**
 * Returns TRUE if we are empty (we have no elements).
 */
bool
ArrayOf::isEmpty(bool allDimensionsIsZero) const
{
    if (dp == nullptr) {
        return true;
    }
    if (allDimensionsIsZero) {
        Dimensions dims = dp->getDimensions();
        return dims.isEmpty(allDimensionsIsZero);
    }
    return (dp->getElementCount() == 0);
}
//=============================================================================
/*
 * Returns TRUE if we have only a single element.
 */
bool
ArrayOf::isScalar() const
{
    if (dp) {
        return dp->isScalar();
    }
    return false;
}
//=============================================================================
/**
 * Returns TRUE if we are 2-Dimensional.
 */
bool
ArrayOf::is2D() const
{
    if (dp) {
        return dp->is2D();
    }
    return false;
}
//=============================================================================
/**
 * Returns TRUE if we are 2-Dimensional and cols == rows.
 */
bool
ArrayOf::isSquare() const
{
    if (dp) {
        return dp->dimensions.isSquare();
    }
    return false;
}
//=============================================================================
/**
 * Returns TRUE if we are a vector.
 */
bool
ArrayOf::isVector() const
{
    return dp->isVector();
}
//=============================================================================
bool
ArrayOf::isRowVector() const
{
    return dp->dimensions.isRowVector();
}
//=============================================================================
bool
ArrayOf::isColumnVector() const
{
    return dp->dimensions.isColumnVector();
}
//=============================================================================
/**
 * Returns TRUE if we are a reference type (cell array, string array,
 * struct array, or handle).
 */
bool
ArrayOf::isReferenceType() const
{
    return (dp->dataClass == NLS_STRUCT_ARRAY) || (dp->dataClass == NLS_CELL_ARRAY)
        || (dp->dataClass == NLS_STRING_ARRAY) || (dp->dataClass == NLS_HANDLE)
        || (dp->dataClass == NLS_GO_HANDLE);
}
//=============================================================================
/**
 * Returns TRUE if we are a complex data type.
 */
bool
ArrayOf::isComplex() const
{
    return (dp->dataClass == NLS_DCOMPLEX || dp->dataClass == NLS_SCOMPLEX);
}
//=============================================================================
/**
 * Returns TRUE if we are a real data type.
 */
bool
ArrayOf::isReal() const
{
    return (!isComplex());
}
//=============================================================================
bool
ArrayOf::allReal() const
{
    bool res;
    switch (dp->dataClass) {
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE: {
        res = true;
    } break;
    case NLS_SCOMPLEX: {
        if (isEmpty(true)) {
            res = true;
        } else {
            single* psingle = (single*)dp->getData();
            singlecomplex* Bz = reinterpret_cast<singlecomplex*>(psingle);
            Eigen::Map<Eigen::MatrixXcf> mat(Bz, 1, dp->getDimensions().getElementCount());
            res = mat.imag().isZero(0);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (isEmpty(true)) {
            res = true;
        } else {
            if (isSparse()) {
                Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spCplxMat
                    = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)dp->getData();
                Eigen::SparseMatrix<double, 0, signedIndexType> spImgMat = spCplxMat->imag();
                Eigen::Map<Eigen::MatrixXd> mat(spImgMat.valuePtr(), 1, spImgMat.nonZeros());
                res = mat.isZero(0);
            } else {
                double* pdouble = (double*)dp->getData();
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>(pdouble);
                Eigen::Map<Eigen::MatrixXcd> mat(Bz, 1, dp->getDimensions().getElementCount());
                res = mat.imag().isZero(0);
            }
        }
    } break;
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        res = false;
    }
    }
    return res;
}
//=============================================================================
void
ArrayOf::copyElements(indexType srcIndex, void* dstPtr, indexType dstIndex, indexType count)
{
    indexType elSize(getElementSize());
    if (isSparse()) {
        Error(_W("copyElements not supported for sparse arrays."));
    }
    switch (dp->dataClass) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        const ArrayOf* sp = (const ArrayOf*)dp->getData();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        for (indexType i = 0; i < count; i++) {
            qp[dstIndex + i] = sp[srcIndex + i];
        }
    } break;
    case NLS_STRUCT_ARRAY: {
        const ArrayOf* sp = (const ArrayOf*)dp->getData();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        indexType fieldCount(dp->fieldNames.size());
        for (indexType i = 0; i < count; i++) {
            for (indexType j = 0; j < (indexType)fieldCount; j++) {
                qp[(dstIndex + i) * fieldCount + j] = sp[(srcIndex + i) * fieldCount + j];
            }
        }
        if (fieldCount > 0) {
            if (qp->getDataClass() == NLS_STRUCT_ARRAY) {
                qp->setStructType(dp->getStructTypeName());
            }
        }
    } break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    default: {
        const char* sp = (const char*)dp->getData();
        if (sp != nullptr) {
            char* qp = (char*)dstPtr;
            memcpy(qp + dstIndex * elSize, sp + srcIndex * elSize, count * elSize);
        }
    } break;
    }
}
//=============================================================================
static bool
isDoubleClass(Class classIn)
{
    return (classIn == NLS_DOUBLE || classIn == NLS_DCOMPLEX);
}
//=============================================================================
static bool
isSingleClass(Class classIn)
{
    return (classIn == NLS_SINGLE || classIn == NLS_SCOMPLEX);
}
//=============================================================================
static bool
isDoubleOrSingleClass(Class classIn)
{
    return (isSingleClass(classIn) || isDoubleClass(classIn));
}
//=============================================================================
#undef caseMacro
//=============================================================================
indexType
ArrayOf::getContentAsScalarIndex(bool bWithZero) const
{
    indexType idx = 0;
    if (getElementCount() != 1) {
        Error(ERROR_SCALAR_EXPECTED);
    }
    double valueAsDouble;
    ArrayOf P;
    if (getDataClass() != NLS_DOUBLE) {
        P = *this;
        P.promoteType(NLS_DOUBLE);
        double* qp = (double*)P.getDataPointer();
        valueAsDouble = qp[0];
    } else {
        double* qp = (double*)dp->getData();
        valueAsDouble = qp[0];
    }
    if ((floor(valueAsDouble) == valueAsDouble) && IsFinite((valueAsDouble))) {
        double maxIndexType = (double)std::numeric_limits<indexType>::max();
        if ((valueAsDouble) > maxIndexType) {
            idx = static_cast<indexType>(maxIndexType);
            Error(_W("Invalid index value > limit max."));
        } else if (valueAsDouble < 0) {
            Error(_W("Expected a positive integer scalar."));
        } else {
            idx = static_cast<indexType>(valueAsDouble);
        }
    } else {
        if (IsFinite(valueAsDouble)) {
            Error(_W("Expected a integer."));
        } else {
            Error(_W("NaN and Inf not allowed."));
        }
    }
    if (!bWithZero) {
        if (idx == 0) {
            Error(_W("Dimension argument must be a positive integer scalar "
                     "within indexing range."));
        }
    }
    return idx;
}
//=============================================================================
indexType*
ArrayOf::getContentAsIndexPointer()
{
    promoteType(NLS_DOUBLE);
    double* qp = (double*)dp->getData();
    size_t nbElements = dp->getDimensions().getElementCount();
    indexType* pIndex = new_with_exception<indexType>(nbElements, false);
    double maxIndexType = (double)std::numeric_limits<indexType>::max();
    for (size_t k = 0; k < nbElements; k++) {
        if ((floor(qp[k]) == qp[k]) && IsFinite((qp[k]))) {
            if ((qp[k]) > maxIndexType) {
                pIndex[k] = static_cast<indexType>(maxIndexType);
            } else if (qp[k] < 0) {
                pIndex[k] = 0;
            } else {
                double dVal = qp[k];
                pIndex[k] = static_cast<indexType>(dVal);
            }
        } else {
            delete[] pIndex;
            if (IsFinite(qp[k])) {
                Error(_W("Expected integer index."));
            } else {
                Error(_W("NaN and Inf not allowed."));
            }
        }
    }
    return pIndex;
}
//=============================================================================
bool
ArrayOf::isNumeric() const
{
    bool bRes = false;
    Class currentclass = this->getDataClass();
    switch (this->getDataClass()) {
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        bRes = true;
        break;
    default:
        bRes = false;
        break;
    }
    return bRes;
}
//=============================================================================
bool
ArrayOf::isDataClassReferenceType(Class cls)
{
    return (cls == NLS_CELL_ARRAY || cls == NLS_STRUCT_ARRAY || cls == NLS_STRING_ARRAY);
}
//=============================================================================
template <class T>
indexType
DoCountNNZReal(const void* dp, indexType len)
{
    indexType accum = 0;
    const T* cp = static_cast<const T*>(dp);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)len; i++)
        if (cp[i]) {
            accum++;
        }
    return accum;
}
//=============================================================================
template <class T>
indexType
DoCountNNZComplex(const void* dp, indexType len)
{
    indexType accum = 0;
    const T* cp = static_cast<const T*>(dp);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)len; i++)
        if (cp[2 * i] || cp[2 * i + 1]) {
            accum++;
        }
    return accum;
}
//=============================================================================
indexType
ArrayOf::nzmax()
{
    if (isSparse()) {
        return CountNonzerosMaxDynamicFunction(
            dp->dataClass, getDimensionLength(0), getDimensionLength(1), dp->getData());
    }
    switch (dp->dataClass) {
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_CHAR:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        return numel();
    case NLS_CELL_ARRAY:
        Error(_W("Undefined function 'nzmax' for input arguments of type 'cell'."));
    case NLS_STRING_ARRAY:
        Error(_W("Undefined function 'nzmax' for input arguments of type 'string'."));
    case NLS_STRUCT_ARRAY:
        Error(_W("Undefined function 'nzmax' for input arguments of type 'struct'."));
    default:
        Error(_W("Undefined function 'nzmax' for input arguments."));
    }
    return 0; // never here
}
//=============================================================================
indexType
ArrayOf::nnz()
{
    if (isSparse()) {
        if (isEmpty()) {
            return 0;
        }
        return CountNonzerosDynamicFunction(
            dp->dataClass, getDimensionLength(0), getDimensionLength(1), dp->getData());
    }
    // OK - its not sparse... now what?
    switch (dp->dataClass) {
    case NLS_LOGICAL:
        return DoCountNNZReal<logical>(dp->getData(), getElementCount());
    case NLS_INT8:
        return DoCountNNZReal<int8>(dp->getData(), getElementCount());
    case NLS_UINT8:
        return DoCountNNZReal<uint8>(dp->getData(), getElementCount());
    case NLS_CHAR:
        return DoCountNNZReal<charType>(dp->getData(), getElementCount());
    case NLS_INT16:
        return DoCountNNZReal<int16>(dp->getData(), getElementCount());
    case NLS_UINT16:
        return DoCountNNZReal<uint16>(dp->getData(), getElementCount());
    case NLS_INT32:
        return DoCountNNZReal<int32>(dp->getData(), getElementCount());
    case NLS_UINT32:
        return DoCountNNZReal<uint32>(dp->getData(), getElementCount());
    case NLS_INT64:
        return DoCountNNZReal<int64>(dp->getData(), getElementCount());
    case NLS_UINT64:
        return DoCountNNZReal<uint64>(dp->getData(), getElementCount());
    case NLS_SINGLE:
        return DoCountNNZReal<single>(dp->getData(), getElementCount());
    case NLS_DOUBLE:
        return DoCountNNZReal<double>(dp->getData(), getElementCount());
    case NLS_SCOMPLEX:
        return DoCountNNZComplex<single>(dp->getData(), getElementCount());
    case NLS_DCOMPLEX:
        return DoCountNNZComplex<double>(dp->getData(), getElementCount());
    case NLS_CELL_ARRAY:
        Error(_W("Undefined function 'nnz' for input arguments of type 'cell'."));
    case NLS_STRING_ARRAY:
        Error(_W("Undefined function 'nnz' for input arguments of type 'string'."));
    case NLS_STRUCT_ARRAY:
        Error(_W("Undefined function 'nnz' for input arguments of type 'struct'."));
    default:
        Error(_W("Undefined function 'nnz' for input arguments."));
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::numel() const
{
    Dimensions dims = getDimensions();
    return dims.getElementCount();
}
//=============================================================================
bool
isColonOperator(const ArrayOf& A)
{
    if ((A.getDataClass() == NLS_CHAR) && (A.getElementCount() == 1)) {
        std::wstring str = A.getContentAsWideString();
        return (str == L":");
    }
    return false;
}
//=============================================================================
/**
 * Given a vector of indexing arrays, convert them into
 * index pointers.  If a colon is encountered, it is
 * preserved (the first one -- the remaining colon expressions
 * are expanded out into vectors).
 */
constIndexPtr*
ProcessNDimIndexes(bool preserveColons, Dimensions& dims, ArrayOfVector& index, bool& anyEmpty,
    indexType& colonIndex, Dimensions& outDims, bool argCheck)
{
    indexType L = index.size();
    constIndexPtr* outndx = new_with_exception<constIndexPtr>(L, false);
    bool colonFound = false;
    anyEmpty = false;
    colonIndex = ((indexType)-1);
    for (size_t i = 0; i < index.size(); i++) {
        bool isColon = isColonOperator(index[i]);
        if (!colonFound && isColon && preserveColons) {
            colonFound = true;
            colonIndex = i;
            outndx[i] = NULL;
            outDims[i] = dims[i];
        } else if (isColon) {
            indexType* buildcolon = new_with_exception<indexType>(dims[i], false);
            for (int j = 1; j <= dims[i]; j++)
                buildcolon[j - 1] = (indexType)j;
            outndx[i] = buildcolon;
            outDims[i] = dims[i];
        } else if (index[i].isEmpty()) {
            anyEmpty = true;
            outndx[i] = NULL;
            outDims[i] = 0;
        } else {
            index[i].toOrdinalType();
            if (argCheck && (index[i].getMaxAsIndex() > dims[i])) {
                Error(_W("Index exceeds array bounds."));
            }
            outndx[i] = (constIndexPtr)index[i].getDataPointer();
            outDims[i] = index[i].getElementCount();
        }
    }
    return outndx;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
