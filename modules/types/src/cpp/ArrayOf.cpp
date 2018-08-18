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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "IEEEFP.hpp"
#include "SparseDynamicFunctions.hpp"
#include "SparseType.hpp"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include <Eigen/Dense>
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <inttypes.h>
#include <math.h>
#include <set>
#include <stdio.h>

#ifdef _MSC_VER
#define snprintf _snprintf
#endif

namespace Nelson {

static int objectBalance;
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
static Interface* io;

typedef std::set<uint32, std::less<uint32>> intSet;
intSet addresses;

ArrayOfVector
scalarArrayOfToArrayOfVector(ArrayOf a)
{
    ArrayOfVector retval;
    retval.push_back(a);
    return retval;
}

void
ArrayOf::setArrayOfIOInterface(Interface* a_io)
{
    io = a_io;
}

Interface*
ArrayOf::getArrayOfIOInterface()
{
    return io;
}

void
outputDoublePrecisionFloat(char* buf, double num)
{
    char temp_buf[100];
    char* tbuf;
    sizeType len;
    tbuf = temp_buf;
    if (num >= 0) {
        sprintf(tbuf, " ");
        tbuf++;
    }
    if (IsInfinite(num)) {
        sprintf(tbuf, "   Inf");
    } else if (IsNaN(num)) {
        sprintf(tbuf, "   NaN");
    } else if ((fabs(num) >= 0.1f && fabs(num) < 1.0f)
        || num <= std::numeric_limits<single>::epsilon()) {
        sprintf(tbuf, "  %0.15f", num);
    } else if (fabs(num) >= 0.01f && fabs(num) < 0.1f) {
        sprintf(tbuf, "  %0.16f", num);
    } else if (fabs(num) >= 0.001f && fabs(num) < 0.01f) {
        sprintf(tbuf, "  %0.17f", num);
    } else if (fabs(num) >= 1.0f && fabs(num) < 10.0f) {
        sprintf(tbuf, "  %1.15f", num);
    } else if (fabs(num) >= 10.0f && fabs(num) < 100.0f) {
        sprintf(tbuf, " %2.13f", num);
    } else if (fabs(num) >= 100.0f && fabs(num) < 1000.0f) {
        sprintf(tbuf, "%3.12f", num);
    } else {
        sprintf(tbuf, "  %1.14e", num);
    }
    len = strlen(temp_buf);
    memcpy(buf, temp_buf, len);
    memset(buf + len, ' ', 24 - len);
    buf[24] = 0;
}

void
outputSinglePrecisionFloat(char* buf, single num)
{
    char temp_buf[100];
    char* tbuf;
    sizeType len;
    tbuf = temp_buf;
    if (num >= 0) {
        sprintf(tbuf, " ");
        tbuf++;
    }
    if (IsNaN(num)) {
        sprintf(tbuf, "   NaN");
    } else if ((fabs(num) >= 0.1f && fabs(num) < 1.0f)
        || num <= std::numeric_limits<single>::epsilon()) {
        sprintf(tbuf, "  %0.8f", num);
    } else if (fabs(num) >= 0.01f && fabs(num) < 0.1f) {
        sprintf(tbuf, "  %0.9f", num);
    } else if (fabs(num) >= 0.001f && fabs(num) < 0.01f) {
        sprintf(tbuf, "  %0.10f", num);
    } else if (fabs(num) >= 1.0f && fabs(num) < 10.0f) {
        sprintf(tbuf, "  %1.7f", num);
    } else if (fabs(num) >= 10.0f && fabs(num) < 100.0f) {
        sprintf(tbuf, " %2.6f", num);
    } else if (fabs(num) >= 100.0f && fabs(num) < 1000.0f) {
        sprintf(tbuf, "%3.5f", num);
    } else {
        sprintf(tbuf, "  %1.7e", num);
    }
    len = strlen(temp_buf);
    memcpy(buf, temp_buf, len);
    memset(buf + len, ' ', 17 - len);
    buf[17] = 0;
}

void
dumpAllArrayOfs()
{
    intSet::iterator i = addresses.begin();
    int j = 0;
    while (i != addresses.end()) {
        uint64 addr = *i;
        ArrayOf* aptr = (ArrayOf*)addr;
        std::cout << "ArrayOf Number " << j << " = " << aptr->getReferenceCount() << "\n";
#ifdef NLS_INDEX_TYPE_64
        printf("  Address = %llx\n", addr);
#else
        printf("  Address = %08x\n", addr);
#endif
        aptr->printMe(1000, 80);
        ++i;
        j++;
    }
}

void
ArrayOf::copyObject(const ArrayOf& copy)
{
    if (copy.dp) {
        dp = copy.dp->getCopy();
    } else {
        dp = nullptr;
    }
}

inline void
ArrayOf::deleteContents(void)
{
    if (dp) {
        int m;
        m = dp->deleteCopy();
        if (m <= 1) {
            delete dp;
        }
        dp = nullptr;
    }
}

bool*
ArrayOf::getBinaryMap(indexType maxD)
{
    bool* map = new_with_exception<bool>(maxD);
    indexType N = getLength();
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

indexType
ArrayOf::getMaxAsIndex()
{
    indexType maxval;
    constIndexPtr rp = (constIndexPtr)dp->getData();
    indexType K = getLength();
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
        indexType len = getLength();
        indexType i = 0;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < len; i++)
            if (rp[i] != 0) {
                indexCount++;
            }
        // Allocate space to hold the new type.
        // indexType *lp = (indexType *) Malloc(indexCount*sizeof(indexType));
        indexType* lp = new_with_exception<indexType>(indexCount);
        indexType* qp = lp;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < len; i++)
            if (rp[i] != 0) {
                *qp++ = (indexType)(i + 1);
            }
        // Reset our data pointer to the new vector.
        Dimensions dimensions;
        dimensions[1] = 1;
        dimensions[0] = indexCount;
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
        indexType len = getLength();
        indexType ndx = 0;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
        indexType len = getLength();
        indexType ndx;
        // Allocate space to hold the new type
        indexType* lp = new_with_exception<indexType>(len);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType i = 0; i < len; i++) {
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
    case NLS_HANDLE: {
        Error(_W("Cannot convert handle arrays to indices."));
    } break;
    case NLS_CELL_ARRAY: {
        Error(_W("Cannot convert cell arrays to indices."));
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(_W("Cannot convert structure arrays to indices."));
    } break;
    }
}

ArrayOf::ArrayOf()
{
    Dimensions dims(0, 0);
    dp = nullptr;
}

/**
 * Create a variable with the specified contents.
 */
ArrayOf::ArrayOf(
    Class type, const Dimensions& dims, void* data, bool sparse, const stringVector& fnames)
{
    dp = new Data(type, dims, data, sparse, fnames);
}

ArrayOf::ArrayOf(Class type)
{
    Dimensions dims(0, 0);
    dp = new Data(type, dims, NULL);
}

/**
 * Destructor - free the data object.
 */
ArrayOf::~ArrayOf()
{
    if (dp) {
        int m;
        m = dp->deleteCopy();
        if (m <= 1) {
            delete dp;
        }
        dp = nullptr;
    }
}

void
ArrayOf::operator=(const ArrayOf& copy)
{
    if (this == &copy) {
        return;
    }
    if (dp) {
        if (dp->deleteCopy() <= 1) {
            delete dp;
        }
        dp = nullptr;
    }
    if (copy.dp) {
        dp = copy.dp->getCopy();
    } else {
        dp = nullptr;
    }
}

int
ArrayOf::getReferenceCount() const
{
    if (dp) {
        return dp->numberOfOwners();
    } else {
        return 0;
    }
}

indexType
ArrayOf::getLength() const
{
    if (dp) {
        return dp->dimensions.getElementCount();
    } else {
        return 0;
    }
}

Dimensions
ArrayOf::getDimensions() const
{
    if (dp) {
        return dp->dimensions;
    } else {
        return Dimensions();
    }
}

indexType
ArrayOf::getDimensionLength(int t) const
{
    if (dp) {
        return dp->dimensions[t];
    } else {
        return 0;
    }
}

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

void
ArrayOf::ensureSingleOwner()
{
    if (dp->numberOfOwners() > 1) {
        if (!dp->sparse) {
            void* np = allocateArrayOf(dp->dataClass, getLength(), dp->fieldNames);
            if (isEmpty()) {
                Dimensions dim = dp->getDimensions();
                dp = dp->putData(dp->dataClass, dim, np, dp->sparse, dp->fieldNames);
            } else {
                copyElements(0, np, 0, getLength());
                dp = dp->putData(dp->dataClass, dp->dimensions, np, dp->sparse, dp->fieldNames);
            }
        } else {
            dp = dp->putData(dp->dataClass, dp->dimensions,
                CopySparseMatrixDynamicFunction(
                    dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData()),
                dp->sparse, dp->fieldNames);
        }
    }
}

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

void
ArrayOf::setDataPointer(void* rp)
{
    dp = dp->putData(dp->dataClass, dp->dimensions, rp, dp->sparse, dp->fieldNames);
}

void
ArrayOf::scalarToMatrix(Dimensions newDimensions)
{
    if (isSparse()) {
        Error(_W("Sparse not supported."));
    }
    if (!isScalar()) {
        Error(ERROR_SCALAR_EXPECTED);
    }
    if (newDimensions.isScalar()) {
        return;
    }
    resize(newDimensions);
    switch (dp->dataClass) {
    case NLS_LOGICAL: {
        logical* ptr = (logical*)dp->getWriteableData();
        logical symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)dp->getWriteableData();
        uint8 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)dp->getWriteableData();
        int8 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)dp->getWriteableData();
        uint16 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)dp->getWriteableData();
        int16 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)dp->getWriteableData();
        uint32 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)dp->getWriteableData();
        int32 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)dp->getWriteableData();
        uint64 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)dp->getWriteableData();
        int64 symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)dp->getWriteableData();
        single symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)dp->getWriteableData();
        double symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)dp->getWriteableData();
        single symbolR = ptr[0];
        single symbolI = ptr[1];
        for (indexType k = 0; k < getDimensions().getElementCount() * 2; k = k + 2) {
            ptr[k] = symbolR;
            ptr[k + 1] = symbolI;
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)dp->getWriteableData();
        double symbolR = ptr[0];
        double symbolI = ptr[1];
        for (indexType k = 0; k < getDimensions().getElementCount() * 2; k = k + 2) {
            ptr[k] = symbolR;
            ptr[k + 1] = symbolI;
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)dp->getWriteableData();
        charType symbol = ptr[0];
        for (indexType k = 0; k < getDimensions().getElementCount(); k++) {
            ptr[k] = symbol;
        }
    } break;
    default: {
        Error(_W("Type not supported."));
    } break;
    }
}

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
    if (newSize.getElementCount() == getLength()) {
        ensureSingleOwner();
        dp->dimensions = newSize;
        return;
    }
    if (isSparse()) {
        Error(_W("Cannot resize sparse arrays."));
    }
    // Allocate space for our new size.
    void* dst_data = allocateArrayOf(dp->dataClass, newSize.getElementCount(), dp->fieldNames);
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

void
ArrayOf::vectorResize(indexType max_index)
{
    if (max_index > getLength()) {
        Dimensions newDim;
        if (isEmpty() || dp->dimensions.isScalar()) {
            newDim.reset();
            newDim[0] = 1;
            newDim[1] = max_index;
        } else if (dp->dimensions.isVector()) {
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
            tDim[1] = getLength();
            reshape(tDim);
            newDim.reset();
            newDim[0] = 1;
            newDim[1] = max_index;
        }
        resize(newDim);
    }
}

/**
 * Reshape an array.  This is only legal if the number of
 * elements remains the same after reshaping.
 */
void
ArrayOf::reshape(Dimensions& a)
{
    if (isClassStruct()) {
        Error(_W("Reshape operation not allowed for overloaded type."));
    }
    if (isFunctionHandle()) {
        Error(_W("Reshape operation not allowed for 'function_handle' type."));
    }
    if (a.getElementCount() != getLength()) {
        Error(_W("Reshape operation cannot change the number of elements in array."));
    }
    if (isSparse()) {
        if (a.is2D() || a.isVector() || a.isScalar()) {
            void* reshapedSparseMatrix = ReshapeSparseMatrixDynamicFunction(
                dp->dataClass, dp->dimensions[0], dp->dimensions[1], a[0], a[1], dp->getData());
            dp = dp->putData(dp->dataClass, a, reshapedSparseMatrix, true);
            dp->dimensions = a;
        } else {
            Error(_W("Reshape operation not allowed with N Dimensions sparse arrays."));
        }
    } else {
        ensureSingleOwner();
        dp->dimensions = a;
    }
}

/**
 * Get our data class (of type Class).
 */
Class
ArrayOf::getDataClass() const
{
    if (dp) {
        return dp->dataClass;
    } else {
        return NLS_DOUBLE;
    }
}

/**
 * Calculate the size of each element in this array.
 */
indexType
ArrayOf::getElementSize() const
{
    switch (dp->dataClass) {
    case NLS_HANDLE:
        return sizeof(nelson_handle);
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
    }
    return 0;
}

/**
 * Calculate the total number of bytes required to store this array.
 */
indexType
ArrayOf::getByteSize() const
{
    if (isSparse()) {
        Error(_W("Byte size calculation not supported for sparse arrays."));
    }
    return getElementSize() * getLength();
}

/**
 * Returns true if we are positive.
 */
#define caseMacro(caseLabel, dpType)                                                               \
    case caseLabel: {                                                                              \
        const dpType* qp = (const dpType*)dp->getData();                                           \
        bool allPositive = true;                                                                   \
        indexType len = getLength();                                                               \
        indexType i = 0;                                                                           \
        while (allPositive && (i < len)) {                                                         \
            allPositive = allPositive && (qp[i] >= 0);                                             \
            i++;                                                                                   \
        }                                                                                          \
        return allPositive;                                                                        \
    }

const bool
ArrayOf::isPositive() const
{
    if (dp->dataClass == NLS_UINT8 || dp->dataClass == NLS_UINT16 || dp->dataClass == NLS_UINT32
        || dp->dataClass == NLS_UINT64) {
        return true;
    }
    if (dp->dataClass == NLS_SCOMPLEX || dp->dataClass == NLS_DCOMPLEX) {
        return false;
    }
    if (isSparse()) {
        Error(_W("isPositive not supported for sparse arrays."));
    }
    switch (dp->dataClass) {
        caseMacro(NLS_SINGLE, single);
        caseMacro(NLS_DOUBLE, double);
        caseMacro(NLS_INT8, int8);
        caseMacro(NLS_INT16, int16);
        caseMacro(NLS_INT32, int32);
        caseMacro(NLS_INT64, int64);
    }
    return false;
}
#undef caseMacro

#define caseMacroReal(caseLabel, type)                                                             \
    case caseLabel:                                                                                \
        retval = (*((const type*)x_dp) == *((const type*)y_dp));                                   \
        break;

#define caseMacroComplex(caseLabel, type)                                                          \
    case caseLabel:                                                                                \
        retval = (((const type*)x_dp)[0] == ((const type*)y_dp)[0])                                \
            && (((const type*)x_dp)[1] == ((const type*)y_dp)[1]);                                 \
        break;

const bool
ArrayOf::testCaseMatchScalar(ArrayOf x) const
{
    if (isSparse()) {
        Error(_W("isPositive not supported for sparse arrays."));
    }
    // Now we have to compare ourselves to the argument.  Check for the
    // case that we are a string type
    if (isSingleString()) {
        // If x is not a string, we cannot match
        if (!x.isSingleString()) {
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
    case NLS_CELL_ARRAY:
    case NLS_CHAR:
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

const bool
ArrayOf::testForCaseMatch(ArrayOf x) const
{
    if (isSparse()) {
        Error(_W("isPositive not supported for sparse arrays."));
    }
    // We had better be a scalar
    if (!(isScalar() || isString())) {
        Error(_W("Switch argument must be a scalar or a string"));
    }
    // And we had better not be a reference type
    if (isReferenceType()) {
        Error(_W("Switch argument cannot be a reference type (struct or cell array)"));
    }
    // If x is a scalar, we just need to call the scalar version
    if (x.isScalar() || x.isSingleString()) {
        return testCaseMatchScalar(x);
    }
    if (x.dp->dataClass != NLS_CELL_ARRAY) {
        Error(_W("Case arguments must either be a scalar or a cell array"));
    }
    const ArrayOf* qp = (const ArrayOf*)x.dp->getData();
    indexType len = x.getLength();
    bool foundMatch = false;
    indexType i = 0;
    while (i < len && !foundMatch) {
        foundMatch = testCaseMatchScalar(qp[i]);
        i++;
    }
    return foundMatch;
}

/**
 * Returns TRUE if we are empty (we have no elements).
 */
const bool
ArrayOf::isEmpty(bool allDimensionsIsZero) const
{
    Dimensions dims = dp->getDimensions();
    return dims.isEmpty(allDimensionsIsZero);
}

/*
 * Returns TRUE if we have only a single element.
 */
const bool
ArrayOf::isScalar() const
{
    return dp->dimensions.isScalar();
}

/**
 * Returns TRUE if we are 2-Dimensional.
 */
const bool
ArrayOf::is2D() const
{
    return dp->dimensions.is2D();
}

/**
 * Returns TRUE if we are 2-Dimensional and cols == rows.
 */
const bool
ArrayOf::isSquare() const
{
    return dp->dimensions.isSquare();
}

/**
 * Returns TRUE if we are a vector.
 */
const bool
ArrayOf::isVector() const
{
    return dp->dimensions.isVector();
}

const bool
ArrayOf::isRowVector() const
{
    return dp->dimensions.isRowVector();
}

const bool
ArrayOf::isColumnVector() const
{
    return dp->dimensions.isColumnVector();
}

/**
 * Returns TRUE if we are a reference type (cell array or
 * struct array).
 */
const bool
ArrayOf::isReferenceType() const
{
    return (dp->dataClass == NLS_STRUCT_ARRAY) || (dp->dataClass == NLS_CELL_ARRAY);
}

/**
 * Returns TRUE if we are a complex data type.
 */
const bool
ArrayOf::isComplex() const
{
    return (dp->dataClass == NLS_DCOMPLEX || dp->dataClass == NLS_SCOMPLEX);
}

/**
 * Returns TRUE if we are a real data type.
 */
const bool
ArrayOf::isReal() const
{
    return (!isComplex());
}

const bool
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
            double* pdouble = (double*)dp->getData();
            doublecomplex* Bz = reinterpret_cast<doublecomplex*>(pdouble);
            Eigen::Map<Eigen::MatrixXcd> mat(Bz, 1, dp->getDimensions().getElementCount());
            res = mat.imag().isZero(0);
        }
    } break;
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        res = false;
    }
    }
    return res;
}

void
ArrayOf::copyElements(indexType srcIndex, void* dstPtr, indexType dstIndex, indexType count)
{
    indexType elSize(getElementSize());
    if (isSparse()) {
        Error(_W("copyElements not supported for sparse arrays."));
    }
    switch (dp->dataClass) {
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
template <class TIN, class TOUT>
void
saturate(Class classIn, Class classOut, const void* pIn, void* pOut, indexType count)
{
    const TIN* sp = (const TIN*)pIn;
    TOUT* qp = (TOUT*)pOut;
    if (classIn > classOut) {
        for (indexType i = 0; i < count; i++) {
            TIN min = (TIN)std::numeric_limits<TOUT>::min();
            TIN max = (TIN)std::numeric_limits<TOUT>::max();
            if (isDoubleOrSingleClass(classIn) && !isDoubleOrSingleClass(classOut)) {
                bool isNaN = false;
                if (isSingleClass(classIn)) {
                    isNaN = std::isnan((single)sp[i]);
                } else {
                    isNaN = std::isnan((double)sp[i]);
                }
                if (isNaN) {
                    qp[i] = (TOUT)0;
                } else {
                    if (sp[i] >= max) {
                        qp[i] = std::numeric_limits<TOUT>::max();
                    } else if (sp[i] < min) {
                        qp[i] = std::numeric_limits<TOUT>::min();
                    } else {
                        qp[i] = (TOUT)sp[i];
                    }
                }
            } else {
                if (sp[i] >= max) {
                    qp[i] = std::numeric_limits<TOUT>::max();
                } else if (sp[i] < min) {
                    qp[i] = std::numeric_limits<TOUT>::min();
                } else {
                    qp[i] = (TOUT)sp[i];
                }
            }
        }
    } else {
        for (indexType i = 0; i < count; i++) {
            TOUT min = (TOUT)std::numeric_limits<TOUT>::min();
            TOUT max = (TOUT)std::numeric_limits<TOUT>::max();
            if (classIn == NLS_DOUBLE || classIn == NLS_DCOMPLEX || classIn == NLS_SINGLE
                || classIn == NLS_SCOMPLEX) {
                if (std::isnan((double)sp[i])) {
                    qp[i] = (TOUT)0;
                } else {
                    if (sp[i] >= max) {
                        qp[i] = std::numeric_limits<TOUT>::max();
                    } else if (sp[i] < min) {
                        qp[i] = std::numeric_limits<TOUT>::min();
                    } else {
                        qp[i] = (TOUT)sp[i];
                    }
                }
            } else {
                if (sp[i] >= max) {
                    qp[i] = std::numeric_limits<TOUT>::max();
                } else if (sp[i] < min) {
                    qp[i] = std::numeric_limits<TOUT>::min();
                } else {
                    qp[i] = (TOUT)sp[i];
                }
            }
        }
    }
}
//=============================================================================
/**
 * Promote our data to a new type.
 *
 * Copy data from our data array to the specified
 * array, converting the data as we go.  We can only
 * convert data to or from base types.  So if the source
 * or destination types are reference types, we cannot
 * perform the conversion.
 *
 * For the remaining types, we have a matrix of
 * possibilities.  Here we list the conversion rules.
 *
 * Source type
 *  - string
 *    - logical dest = (source == 0) ? 0 : 1
 *    - real dest = (double) source
 *    - complex dest = (double) source
 *  - logical
 *    - string dest = (char) source
 *    - real   dest = (double) source
 *    - complex dest = (double) source
 *  - real
 *    - string dest = (char) source
 *    - logical dest = (source == 0) ? 0 : 1
 *    - complex dest = (double) source
 *  - complex
 *    - string dest = (char) real(source)
 *    - logical dest = (real(source) == 0 && imag(source) == 0) ? 0:1
 *    - real dest = real(source)
 */
void
ArrayOf::promoteType(Class dstClass, stringVector fNames)
{
    indexType elCount = 0;
    void* dstPtr = nullptr;
    if (isEmpty()) {
        dp = dp->putData(dstClass, dp->dimensions, NULL, isSparse(), fNames);
        return;
    }
    if (dp->dataClass == NLS_HANDLE)
        if (dstClass == NLS_HANDLE) {
            return;
        } else {
            Error(_W("Cannot convert handle-arrays to any other type."));
        }
    // Handle the reference types.
    // Cell arrays can be promoted with no effort to cell arrays.
    if (dp->dataClass == NLS_CELL_ARRAY)
        if (dstClass == NLS_CELL_ARRAY) {
            return;
        } else {
            Error(_W("Cannot convert cell-arrays to any other type."));
        }
    // Structure arrays can be promoted to structure arrays with different
    // field structures, but have to be rearranged.
    if (dp->dataClass == NLS_STRUCT_ARRAY)
        if (dstClass == NLS_STRUCT_ARRAY) {
            // TODO: Generalize this code to allow for one more field in destination
            // than in source...
            if (dp->fieldNames.size() > fNames.size()) {
                Error(_W("Cannot combine structures with different fields if the "
                         "combination "
                         "requires fields to be deleted from one of the structures."));
            }
            // We are promoting a struct array to a struct array.
            // To do so, we have to make sure that the field names work out.
            // The only thing we must check for is that every field name
            // in fieldnames is present in fnames.
            int extraCount = 0;
            int matchCount = 0;
            indexType i;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (i = 0; i < (int)fNames.size(); i++) {
                int64 ndx = getFieldIndex(fNames[i]);
                if (ndx == -1) {
                    extraCount++;
                } else {
                    matchCount++;
                }
            }
            // Now, matchCount should be equal to the size of fieldNames
            if (matchCount != dp->fieldNames.size()) {
                Error(_W("Cannot combine structures with different fields if the "
                         "combination "
                         "requires fields to be deleted from one of the structures."));
            }
            void* dstPtr = allocateArrayOf(dp->dataClass, getLength(), fNames);
            const ArrayOf* src_rp = (const ArrayOf*)dp->getData();
            ArrayOf* dst_rp = (ArrayOf*)dstPtr;
            indexType elCount(getLength());
            indexType fieldCount(dp->fieldNames.size());
            indexType newFieldCount(fNames.size());
            ;
            // Now we have to copy our existing fields into the new order...
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (i = 0; i < fieldCount; i++) {
                int64 newNdx = getFieldIndexFromList(dp->fieldNames[i], fNames);
                for (indexType j = 0; j < elCount; j++) {
                    dst_rp[j * newFieldCount + newNdx] = src_rp[j * fieldCount + i];
                }
            }
            dp = dp->putData(dp->dataClass, dp->dimensions, dstPtr, false, fNames);
            return;
        } else {
            Error(_W("Cannot convert struct-arrays to any other type."));
        }
    // Catch attempts to convert data types to reference types.
    if ((dstClass == NLS_CELL_ARRAY) || (dstClass == NLS_STRUCT_ARRAY)) {
        Error(_W("Cannot convert base types to reference types."));
    }
    // Do nothing for promoting to same class (no-op).
    if (isSparse()) {
        dp = dp->putData(dstClass, dp->dimensions,
            TypeConvertSparseDynamicFunction(
                dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData(), dstClass),
            true);
        return;
    }
    if (dstClass == dp->dataClass) {
        return;
    }
    elCount = getLength();
    // We have to promote...
    dstPtr = allocateArrayOf(dstClass, elCount);
    indexType count = elCount;
    switch (dp->dataClass) {
#define caseMacro(caseLabel, dpType, convCode)                                                     \
    case caseLabel: {                                                                              \
        dpType* qp = (dpType*)dstPtr;                                                              \
        for (indexType i = 0; i < count; i++)                                                      \
            convCode;                                                                              \
    } break;
    case NLS_CHAR: {
        charType* sp = (charType*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
            caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i]);
            caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i]);
            caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i]);
            caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i]);
            caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i]);
            caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i]);
            caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i]);
            caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i]);
        default: { } break; }
    } break;
    case NLS_LOGICAL: {
        const logical* sp = (const logical*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
            caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i]);
            caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i]);
            caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i]);
            caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i]);
            caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i]);
            caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i]);
            caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i]);
            caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i]);
        default: { } break; }
    } break;
    case NLS_UINT8: {
        const uint8* sp = (const uint8*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_INT8: {
            saturate<uint8, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<uint8, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint8, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<uint8, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint8, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<uint8, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint8, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_INT8: {
        const int8* sp = (const int8*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int8, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int8, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<int8, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int8, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<int8, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int8, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<int8, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_UINT16: {
        const uint16* sp = (const uint16*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<uint16, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<uint16, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint16, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<uint16, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint16, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<uint16, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint16, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_INT16: {
        const int16* sp = (const int16*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int16, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<int16, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int16, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int16, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<int16, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int16, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<int16, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_UINT32: {
        const uint32* sp = (const uint32*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_INT8: {
            saturate<uint32, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT8: {
            saturate<uint32, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<uint32, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint32, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint32, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<uint32, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint32, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_INT32: {
        const int32* sp = (const int32*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int32, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<int32, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int32, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<int32, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int32, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int32, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<int32, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_INT64: {
        const int64* sp = (const int64*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<int64, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<int64, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<int64, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<int64, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<int64, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<int64, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<int64, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_UINT64: {
        const uint64* sp = (const uint64*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<uint64, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<uint64, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<uint64, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<uint64, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<uint64, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<uint64, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<uint64, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_SINGLE: {
        const single* sp = (const single*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<single, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<single, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<single, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<single, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<single, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<single, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<single, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<single, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_DOUBLE: {
        const double* sp = (const double*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i]);
            caseMacro(NLS_LOGICAL, logical, qp[i] = (sp[i] == 0) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i]);
            caseMacro(NLS_SCOMPLEX, single, qp[i << 1] = (single)sp[i]);
            caseMacro(NLS_DCOMPLEX, double, qp[i << 1] = (double)sp[i]);
        case NLS_UINT8: {
            saturate<double, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT8: {
            saturate<double, int8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT16: {
            saturate<double, uint16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT16: {
            saturate<double, int16>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT32: {
            saturate<double, uint32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT32: {
            saturate<double, int32>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_INT64: {
            saturate<double, int64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        case NLS_UINT64: {
            saturate<double, uint64>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        default: { } break; }
    } break;
    case NLS_SCOMPLEX: {
        const single* sp = (const single*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i << 1]);
            caseMacro(NLS_LOGICAL, logical,
                qp[i] = ((sp[i << 1] == 0.0) && (sp[(i << 1) + 1] == 0.0)) ? 0 : 1);
        case NLS_SINGLE: {
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)sp);
            single* qp = (single*)dstPtr;
            Eigen::Map<Eigen::MatrixXcf> matA(Az, 1, dp->dimensions.getElementCount());
            Eigen::Map<Eigen::MatrixXf> matB(qp, 1, dp->dimensions.getElementCount());
            matB = matA.real();
        } break;
            caseMacro(NLS_DOUBLE, double, qp[i] = (double)sp[i << 1]);
            caseMacro(NLS_DCOMPLEX, double, {
                qp[i << 1] = (double)sp[i << 1];
                qp[(i << 1) + 1] = (double)sp[(i << 1) + 1];
            });
        case NLS_UINT8: {
            saturate<single, uint8>(dp->dataClass, dstClass, dp->getData(), dstPtr, count);
        } break;
        /*
        caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i << 1]);
        caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i << 1]);
        caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i << 1]);
        caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i << 1]);
        caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i << 1]);
        caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i << 1]);
        caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i << 1]);
        caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i << 1]);
        */
        default: { } break; }
    } break;
    case NLS_DCOMPLEX: {
        const double* sp = (const double*)dp->getData();
        switch (dstClass) {
            caseMacro(NLS_CHAR, charType, qp[i] = (charType)sp[i << 1]);
            caseMacro(NLS_LOGICAL, logical,
                qp[i] = ((sp[i << 1] == 0.0) && (sp[(i << 1) + 1] == 0.0)) ? 0 : 1);
            caseMacro(NLS_SINGLE, single, qp[i] = (single)sp[i << 1]);
        case NLS_DOUBLE: {
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)sp);
            double* qp = (double*)dstPtr;
            Eigen::Map<Eigen::MatrixXcd> matA(Az, 1, dp->dimensions.getElementCount());
            Eigen::Map<Eigen::MatrixXd> matB(qp, 1, dp->dimensions.getElementCount());
            matB = matA.real();
        } break;
            caseMacro(NLS_SCOMPLEX, single, {
                qp[i << 1] = (single)sp[i << 1];
                qp[(i << 1) + 1] = (single)sp[(i << 1) + 1];
            });
            caseMacro(NLS_UINT8, uint8, qp[i] = (uint8)sp[i << 1]);
            caseMacro(NLS_INT8, int8, qp[i] = (int8)sp[i << 1]);
            caseMacro(NLS_UINT16, uint16, qp[i] = (uint16)sp[i << 1]);
            caseMacro(NLS_INT16, int16, qp[i] = (int16)sp[i << 1]);
            caseMacro(NLS_UINT32, uint32, qp[i] = (uint32)sp[i << 1]);
            caseMacro(NLS_INT32, int32, qp[i] = (int32)sp[i << 1]);
            caseMacro(NLS_UINT64, uint64, qp[i] = (uint64)sp[i << 1]);
            caseMacro(NLS_INT64, int64, qp[i] = (int64)sp[i << 1]);
        default: { } break; }
    } break;
    }
    dp = dp->putData(dstClass, dp->dimensions, dstPtr);
}

#undef caseMacro

void
ArrayOf::promoteType(Class dstClass)
{
    stringVector dummy;
    promoteType(dstClass, dummy);
}

/********************************************************************************
 * Display functions *
 ********************************************************************************/

/**
 * Print this object when it is an element of a cell array.  This is
 * generally a shorthand summary of the description of the object.
 */
void
ArrayOf::summarizeCellEntry() const
{
    if (isEmpty()) {
        if (dp->dataClass == NLS_CHAR) {
            io->outputMessage("''");
        } else {
            io->outputMessage("[]");
        }
    } else {
        switch (dp->dataClass) {
        case NLS_CELL_ARRAY:
            io->outputMessage("{");
            dp->dimensions.printMe(io);
            io->outputMessage(" cell }");
            break;
        case NLS_STRUCT_ARRAY:
            io->outputMessage(" ");
            dp->dimensions.printMe(io);
            if (dp->getStructTypeName() == NLS_FUNCTION_HANDLE_STR) {
                io->outputMessage(std::string(" ") + NLS_FUNCTION_HANDLE_STR);
            } else if (dp->getStructTypeName() == NLS_STRUCT_ARRAY_STR) {
                io->outputMessage(" struct array");
            } else {
                io->outputMessage(std::string(" class ") + dp->getStructTypeName());
            }
            break;
        case NLS_CHAR: {
            Dimensions dims = dp->dimensions;
            if (dims.isRowVector()) {
                if (dims.getColumns() < (indexType)(io->getTerminalWidth() - 3)) {
                    std::wstring str = getContentAsWideString();
                    str = L"\'" + str + L"\'";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dp->dimensions.printMe(io);
            io->outputMessage(" string]");
        } break;
        case NLS_HANDLE:
            if (dp->dimensions.isScalar()) {
                io->outputMessage("[handle]");
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" handle]");
            }
            break;
        case NLS_LOGICAL:
            if (!isSparse() && dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const logical*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" logical]");
            }
            break;
        case NLS_UINT8:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint8*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint8]");
            }
            break;
        case NLS_INT8:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int8*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int8]");
            }
            break;
        case NLS_UINT16:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint16*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint16]");
            }
            break;
        case NLS_INT16:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int16*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int16]");
            }
            break;
        case NLS_UINT32:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint32*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint32]");
            }
            break;
        case NLS_INT32:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int32*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int32]");
            }
            break;
        case NLS_UINT64: {
            if (dp->dimensions.isScalar()) {
                uint64 val = *((const uint64*)dp->getData());
                std::string msg = "[" + std::to_string(val) + "]";
                // snprintf(msgBuffer, MSGBUFLEN, "[" PRIu64 "]", *((const
                // uint64*)dp->getData())); io->outputMessage(msgBuffer);
                io->outputMessage(msg.c_str());
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint64]");
            }
        } break;
        case NLS_INT64: {
            if (dp->dimensions.isScalar()) {
                int64 value = *((const int64*)dp->getData());
                std::string msg = std::string("[") + std::to_string(value) + std::string("]");
                // snprintf(msgBuffer, MSGBUFLEN, "[" PRId64 "]", *((const
                // int64*)dp->getData()));
                io->outputMessage(msg.c_str());
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int64]");
            }
        } break;
        case NLS_DOUBLE:
            if (!isSparse() && dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%lf]", *((const double*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" double]");
            }
            break;
        case NLS_DCOMPLEX:
            if (!isSparse() && dp->dimensions.isScalar()) {
                const double* ap = (const double*)dp->getData();
                snprintf(msgBuffer, MSGBUFLEN, "[%lf+%lfi]", ap[0], ap[1]);
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" dcomplex]");
            }
            break;
        case NLS_SINGLE:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%f]", *((const single*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" single]");
            }
            break;
        case NLS_SCOMPLEX:
            if (dp->dimensions.isScalar()) {
                const single* ap = (const single*)dp->getData();
                snprintf(msgBuffer, MSGBUFLEN, "[%f+%fi]", ap[0], ap[1]);
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" complex]");
            }
            break;
        }
    }
}

void
emitElement(char* msgBuffer, const void* dp, indexType num, Class dcls)
{
    switch (dcls) {
    case NLS_STRUCT_ARRAY: {
    } break;
    case NLS_HANDLE: {
    } break;
    case NLS_INT8: {
        const int8* ap = (const int8*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "% 4d", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_UINT8: {
        const uint8* ap = (const uint8*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%3u", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_INT16: {
        const int16* ap = (const int16*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "% 6d", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_UINT16: {
        const uint16* ap = (const uint16*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%5u", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_INT32: {
        const int32* ap = (const int32*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%13d", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_UINT32: {
        const uint32* ap = (const uint32*)dp;
        snprintf(msgBuffer, MSGBUFLEN, "%12u", ap[num]);
        io->outputMessage(msgBuffer);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_INT64: {
        const int64* ap = (const int64*)dp;
        std::string msg = std::to_string(ap[num]) + "  ";
        // snprintf(msgBuffer, MSGBUFLEN, "%13d", ap[num]);
        // io->outputMessage(msgBuffer);
        // snprintf(msgBuffer, MSGBUFLEN, "  ");
        // io->outputMessage(msgBuffer);
        io->outputMessage(msg.c_str());
        break;
    }
    case NLS_UINT64: {
        const uint64* ap = (const uint64*)dp;
        std::string msg("");
        msg = std::to_string(ap[num]) + "  ";
        // snprintf(msgBuffer, MSGBUFLEN, "%12u", ap[num]);
        // io->outputMessage(msgBuffer);
        // snprintf(msgBuffer, MSGBUFLEN, "  ");
        // io->outputMessage(msgBuffer);
        io->outputMessage(msg.c_str());
        break;
    }
    case NLS_LOGICAL: {
        const logical* ap = (const logical*)dp;
        if (ap[num] == 0) {
            snprintf(msgBuffer, MSGBUFLEN, "false  ");
        } else {
            snprintf(msgBuffer, MSGBUFLEN, "true   ");
        }
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_CHAR: {
        const wchar_t* ap = (const wchar_t*)dp;
        std::wstring wstr;
        wstr.push_back(ap[num]);
        io->outputMessage(wstr);
        break;
    }
    case NLS_SINGLE: {
        const single* ap = (const single*)dp;
        outputSinglePrecisionFloat(msgBuffer, ap[num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_DOUBLE: {
        const double* ap = (const double*)dp;
        outputDoublePrecisionFloat(msgBuffer, ap[num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_SCOMPLEX: {
        const single* ap = (const single*)dp;
        outputSinglePrecisionFloat(msgBuffer, ap[2 * num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, " ");
        io->outputMessage(msgBuffer);
        outputSinglePrecisionFloat(msgBuffer, ap[2 * num + 1]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "i  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_DCOMPLEX: {
        const double* ap = (const double*)dp;
        outputDoublePrecisionFloat(msgBuffer, ap[2 * num]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, " ");
        io->outputMessage(msgBuffer);
        outputDoublePrecisionFloat(msgBuffer, ap[2 * num + 1]);
        io->outputMessage(msgBuffer);
        memset(msgBuffer, 0, MSGBUFLEN);
        snprintf(msgBuffer, MSGBUFLEN, "i  ");
        io->outputMessage(msgBuffer);
        break;
    }
    case NLS_CELL_ARRAY: {
        ArrayOf* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            io->outputMessage("[]");
        } else {
            ap[num].summarizeCellEntry();
        }
        // io->outputMessage("  ");
    }
    }
}

/**
 * Display this variable on the given output stream.
 */
void
ArrayOf::printMe(int printLimit, sizeType termWidth) const
{
    int nominalWidth;
    // Print the class...
    switch (dp->dataClass) {
    case NLS_HANDLE:
        io->outputMessage("  <handle>  ");
        nominalWidth = 5;
        break;
    case NLS_UINT8:
        io->outputMessage("  <uint8>  ");
        nominalWidth = 5;
        break;
    case NLS_INT8:
        io->outputMessage("  <int8>  ");
        nominalWidth = 6;
        break;
    case NLS_UINT16:
        io->outputMessage("  <uint16>  ");
        nominalWidth = 7;
        break;
    case NLS_INT16:
        io->outputMessage("  <int16>  ");
        nominalWidth = 8;
        break;
    case NLS_UINT32:
        io->outputMessage("  <uint32>  ");
        nominalWidth = 14;
        break;
    case NLS_INT32:
        io->outputMessage("  <int32>  ");
        nominalWidth = 15;
        break;
    case NLS_UINT64:
        io->outputMessage("  <uint64>  ");
        nominalWidth = 14;
        break;
    case NLS_INT64:
        io->outputMessage("  <int64>  ");
        nominalWidth = 15;
        break;
    case NLS_SINGLE:
        io->outputMessage("  <single>  ");
        nominalWidth = 20;
        break;
    case NLS_DOUBLE:
        io->outputMessage("  <double>  ");
        nominalWidth = 30;
        break;
    case NLS_LOGICAL:
        io->outputMessage("  <logical>  ");
        nominalWidth = 2;
        break;
    case NLS_CHAR:
        io->outputMessage("  <string>  ");
        nominalWidth = 1;
        break;
    case NLS_SCOMPLEX:
        io->outputMessage("  <single>  ");
        nominalWidth = 36;
        break;
    case NLS_DCOMPLEX:
        io->outputMessage("  <double>  ");
        nominalWidth = 54;
        break;
    case NLS_CELL_ARRAY:
        io->outputMessage("  <cell> ");
        nominalWidth = 10;
        break;
    case NLS_STRUCT_ARRAY:
        io->outputMessage("  <struct> ");
        nominalWidth = 10;
        break;
    }
    io->outputMessage("- size: ");
    dp->dimensions.printMe(io);
    io->outputMessage("\n");
    if (isEmpty()) {
        if (isStruct()) {
            stringVector fieldsName = getFieldNames();
            if (fieldsName.size() == 0) {
                io->outputMessage("  []\n");
            } else {
                for (size_t k = 0; k < fieldsName.size(); k++) {
                    io->outputMessage("    ");
                    io->outputMessage(fieldsName[k]);
                    io->outputMessage("\n");
                }
            }
        } else {
            io->outputMessage("  []\n");
        }
        return;
    }
    if (isSparse()) {
        sprintf(msgBuffer, _("\tMatrix is sparse with %d nonzeros\n").c_str(), getNonzeros());
        io->outputMessage(msgBuffer);
        return;
    }
    if (dp->dataClass == NLS_STRUCT_ARRAY) {
        if (dp->dimensions.isScalar()) {
            ArrayOf* ap;
            ap = (ArrayOf*)dp->getData();
            for (sizeType n = 0; n < (sizeType)dp->fieldNames.size(); n++) {
                io->outputMessage("    ");
                io->outputMessage(dp->fieldNames[n].c_str());
                io->outputMessage(": ");
                ap[n].summarizeCellEntry();
                io->outputMessage("\n");
            }
        } else {
            if (dp->fieldNames.size() > 0) {
                io->outputMessage("  Fields\n");
                for (sizeType n = 0; n < (sizeType)dp->fieldNames.size(); n++) {
                    io->outputMessage("    ");
                    io->outputMessage(dp->fieldNames[n].c_str());
                    io->outputMessage("\n");
                }
            }
        }
    } else {
        const void* ap = dp->getData();
        if (dp->dimensions.getLength() == 2) {
            indexType rows = dp->dimensions.getRows();
            indexType columns = dp->dimensions.getColumns();
            int items_printed;
            items_printed = 0;
            // Determine how many columns will fit across
            // the terminal width
            indexType colsPerPage = (indexType)floor((termWidth - 1) / ((single)nominalWidth));
            indexType pageCount = (indexType)ceil(columns / ((single)colsPerPage));
            for (indexType k = 0; k < pageCount && (items_printed < printLimit); k++) {
                indexType colsInThisPage = columns - colsPerPage * k;
                colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                if (dp->dimensions.getElementCount() > 1 && dp->dataClass != NLS_CHAR) {
                    snprintf(msgBuffer, MSGBUFLEN, _("\nColumns %d to %d\n").c_str(),
                        k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                    io->outputMessage(msgBuffer);
                }
                memset(msgBuffer, 0, MSGBUFLEN);
                for (indexType i = 0; i < rows && (items_printed < printLimit); i++) {
                    snprintf(msgBuffer, MSGBUFLEN, " ");
                    io->outputMessage(msgBuffer);
                    memset(msgBuffer, 0, MSGBUFLEN);
                    for (indexType j = 0; j < colsInThisPage && (items_printed < printLimit); j++) {
                        emitElement(msgBuffer, ap, i + (k * colsPerPage + j) * rows, dp->dataClass);
                        items_printed++;
                    }
                    snprintf(msgBuffer, MSGBUFLEN, "\n");
                    io->outputMessage(msgBuffer);
                    memset(msgBuffer, 0, MSGBUFLEN);
                }
            }
            if (items_printed >= printLimit) {
                io->outputMessage(_W("\n... Output truncated - use setprintlimit function to see "
                                     "more of the output ...\n"));
            }
        } else if (dp->dimensions.getLength() > 2) {
            /**
             * For N-ary arrays, data slice  -  start with
             * [1,1,1,...,1].  We keep doing the matrix
             * print , incrementing from the highest dimension,
             * and rolling downwards.
             */
            Dimensions wdims(dp->dimensions.getLength());
            indexType rows(dp->dimensions.getRows());
            indexType columns(dp->dimensions.getColumns());
            int items_printed;
            items_printed = 0;
            indexType offset = 0;
            while (wdims.inside(dp->dimensions) && (items_printed < printLimit)) {
                snprintf(msgBuffer, MSGBUFLEN, "(:,:");
                io->outputMessage(msgBuffer);
                for (sizeType m = 2; m < dp->dimensions.getLength(); m++) {
                    snprintf(msgBuffer, MSGBUFLEN, ",%d", (int)wdims[m] + 1);
                    io->outputMessage(msgBuffer);
                }
                snprintf(msgBuffer, MSGBUFLEN, ") =\n\n");
                io->outputMessage(msgBuffer);
                // Determine how many columns will fit across
                // the terminal width
                indexType colsPerPage = (indexType)floor((termWidth - 1) / ((single)nominalWidth));
                int pageCount;
                pageCount = (int)ceil(columns / ((single)colsPerPage));
                for (int k = 0; k < pageCount && (items_printed < printLimit); k++) {
                    indexType colsInThisPage = columns - colsPerPage * k;
                    colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
                    snprintf(msgBuffer, MSGBUFLEN, _("\nColumns %d to %d\n").c_str(),
                        k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                    io->outputMessage(msgBuffer);
                    memset(msgBuffer, 0, MSGBUFLEN);
                    for (indexType i = 0; i < rows && (items_printed < printLimit); i++) {
                        snprintf(msgBuffer, MSGBUFLEN, " ");
                        io->outputMessage(msgBuffer);
                        memset(msgBuffer, 0, MSGBUFLEN);
                        for (indexType j = 0; j < colsInThisPage && (items_printed < printLimit);
                             j++) {
                            emitElement(msgBuffer, ap, i + (k * colsPerPage + j) * rows + offset,
                                dp->dataClass);
                            items_printed++;
                        }
                        snprintf(msgBuffer, MSGBUFLEN, "\n");
                        io->outputMessage(msgBuffer);
                        memset(msgBuffer, 0, MSGBUFLEN);
                    }
                }
                offset += rows * columns;
                wdims.incrementModulo(dp->dimensions, 2);
            }
            if (items_printed >= printLimit) {
                io->outputMessage(_W("\n... Output truncated - use setprintlimit function to see "
                                     "more of the output ...\n"));
            }
        }
    }
}

indexType
ArrayOf::getContentAsScalarIndex(bool bWithZero)
{
    indexType idx = 0;
    if (getLength() != 1) {
        Error(ERROR_SCALAR_EXPECTED);
    }
    promoteType(NLS_DOUBLE);
    double* qp = (double*)dp->getData();
    if ((floor(*qp) == *qp) && IsFinite((*qp))) {
        double maxIndexType = (double)std::numeric_limits<indexType>::max();
        if ((*qp) > maxIndexType) {
            idx = static_cast<indexType>(maxIndexType);
            Error(_W("Invalid index value > limit max."));
        } else if (*qp < 0) {
            Error(_W("Expected a positive integer scalar."));
        } else {
            double dVal = (*qp);
            idx = static_cast<indexType>(dVal);
        }
    } else {
        if (IsFinite(*qp)) {
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

indexType*
ArrayOf::getContentAsIndexPointer()
{
    promoteType(NLS_DOUBLE);
    double* qp = (double*)dp->getData();
    size_t nbElements = dp->getDimensions().getElementCount();
    indexType* pIndex = new_with_exception<indexType>(nbElements);
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

const bool
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

bool
ArrayOf::isDataClassReferenceType(Class cls)
{
    return (cls == NLS_CELL_ARRAY || cls == NLS_STRUCT_ARRAY);
}

template <class T>
indexType
DoCountNNZReal(const void* dp, indexType len)
{
    indexType accum = 0;
    const T* cp = static_cast<const T*>(dp);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType i = 0; i < len; i++)
        if (cp[i]) {
            accum++;
        }
    return accum;
}

template <class T>
indexType
DoCountNNZComplex(const void* dp, indexType len)
{
    indexType accum = 0;
    const T* cp = static_cast<const T*>(dp);
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType i = 0; i < len; i++)
        if (cp[2 * i] || cp[2 * i + 1]) {
            accum++;
        }
    return accum;
}

indexType
ArrayOf::nzmax()
{
    if (isSparse()) {
        if (isEmpty()) {
            return 0;
        }
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
    case NLS_STRUCT_ARRAY:
        Error(_W("Undefined function 'nzmax' for input arguments of type 'struct'."));
    default:
        Error(_W("Undefined function 'nzmax' for input arguments."));
    }
    return 0; // never here
}

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
        return DoCountNNZReal<logical>(dp->getData(), getLength());
    case NLS_INT8:
        return DoCountNNZReal<int8>(dp->getData(), getLength());
    case NLS_UINT8:
        return DoCountNNZReal<uint8>(dp->getData(), getLength());
    case NLS_CHAR:
        return DoCountNNZReal<charType>(dp->getData(), getLength());
    case NLS_INT16:
        return DoCountNNZReal<int16>(dp->getData(), getLength());
    case NLS_UINT16:
        return DoCountNNZReal<uint16>(dp->getData(), getLength());
    case NLS_INT32:
        return DoCountNNZReal<int32>(dp->getData(), getLength());
    case NLS_UINT32:
        return DoCountNNZReal<uint32>(dp->getData(), getLength());
    case NLS_INT64:
        return DoCountNNZReal<int64>(dp->getData(), getLength());
    case NLS_UINT64:
        return DoCountNNZReal<uint64>(dp->getData(), getLength());
    case NLS_SINGLE:
        return DoCountNNZReal<single>(dp->getData(), getLength());
    case NLS_DOUBLE:
        return DoCountNNZReal<double>(dp->getData(), getLength());
    case NLS_SCOMPLEX:
        return DoCountNNZComplex<single>(dp->getData(), getLength());
    case NLS_DCOMPLEX:
        return DoCountNNZComplex<double>(dp->getData(), getLength());
    case NLS_CELL_ARRAY:
        Error(_W("Undefined function 'nnz' for input arguments of type 'cell'."));
    case NLS_STRUCT_ARRAY:
        Error(_W("Undefined function 'nnz' for input arguments of type 'struct'."));
    default:
        Error(_W("Undefined function 'nnz' for input arguments."));
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::numel()
{
    Dimensions dims = getDimensions();
    return dims.getElementCount();
}
//=============================================================================
bool
isColonOperator(const ArrayOf& A)
{
    if ((A.getDataClass() == NLS_CHAR) && (A.getLength() == 1)) {
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
ProcessNDimIndexes(bool preserveColons, Dimensions dims, ArrayOfVector& index, bool& anyEmpty,
    int& colonIndex, Dimensions& outDims, bool argCheck)
{
    indexType L = index.size();
    constIndexPtr* outndx = new_with_exception<constIndexPtr>(L);
    bool colonFound = false;
    anyEmpty = false;
    colonIndex = -1;
    for (int i = 0; i < index.size(); i++) {
        bool isColon = isColonOperator(index[i]);
        if (!colonFound && isColon && preserveColons) {
            colonFound = true;
            colonIndex = i;
            outndx[i] = NULL;
            outDims[i] = dims[i];
        } else if (isColon) {
            indexType* buildcolon = new_with_exception<indexType>(dims[i]);
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
            outDims[i] = index[i].getLength();
        }
    }
    return outndx;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
