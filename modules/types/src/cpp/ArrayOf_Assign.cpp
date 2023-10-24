//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Exception.hpp"
#include "SparseDynamicFunctions.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
setNDimSubsetNoColonReal(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // For each output element
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        // Retrieve the index values based on ndxptr
        // Use these to calculate the source address
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        // Copy the value
        sp[srcadd] = *destp;
        destp += advance;
        // Update the ndxset
        ndxptr[0]++;
        for (indexType j = 0; j < numDims - 1; j++) {
            if (ndxptr[j] >= outDims[j]) {
                ndxptr[j] = 0;
                ndxptr[j + 1]++;
            }
        }
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetNoColonBurst(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType burstLen,
    indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // For each output element
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        // Retrieve the index values based on ndxptr
        // Use these to calculate the source address
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        // Copy the value
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[k];
        }
        destp += burstLen * advance;
        // Update the ndxset
        ndxptr[0]++;
        for (indexType j = 0; j < numDims - 1; j++) {
            if (ndxptr[j] >= outDims[j]) {
                ndxptr[j] = 0;
                ndxptr[j + 1]++;
            }
        }
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetFirstColonReal(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    indexType numrows = outDims[0];
    // For each output element
    for (indexType i = 0; i < outCount; i += numrows) {
        indexType srcadd = 0;
        // Retrieve the index values based on ndxptr
        // Use these to calculate the source address
        for (indexType j = 1; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        // Copy the value
        for (indexType k = 0; k < numrows; k++) {
            sp[srcadd + k] = *destp;
            destp += advance;
        }
        // Update the ndxset
        ndxptr[1]++;
        for (indexType j = 1; j < numDims - 1; j++) {
            if (ndxptr[j] >= outDims[j]) {
                ndxptr[j] = 0;
                ndxptr[j + 1]++;
            }
        }
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetFirstColonBurst(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType burstLen,
    indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    indexType numrows = outDims[0];
    // For each output element
    for (indexType i = 0; i < outCount; i += numrows) {
        indexType srcadd = 0;
        // Retrieve the index values based on ndxptr
        // Use these to calculate the source address
        for (indexType j = 1; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        // Copy the value
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[i * burstLen * advance + k];
        }
        // Update the ndxset
        ndxptr[1]++;
        for (indexType j = 1; j < numDims - 1; j++) {
            if (ndxptr[j] >= outDims[j]) {
                ndxptr[j] = 0;
                ndxptr[j + 1]++;
            }
        }
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetAnyColonReal(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType colonIndex,
    indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // For each output element
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        // Retrieve the index values based on ndxptr
        // Use these to calculate the source address
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval;
            if (j == colonIndex) {
                ndxval = ndxptr[j];
            } else {
                ndxval = ndx[j][ndxptr[j]] - 1;
            }
            srcadd += ndxval * srcfact[j];
        }
        // Copy the value
        sp[srcadd] = *destp;
        destp += advance;
        // Update the ndxset
        ndxptr[0]++;
        for (indexType j = 0; j < numDims - 1; j++) {
            if (ndxptr[j] >= outDims[j]) {
                ndxptr[j] = 0;
                ndxptr[j + 1]++;
            }
        }
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetAnyColonBurst(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType colonIndex,
    indexType burstLen, indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // For each output element
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        // Retrieve the index values based on ndxptr
        // Use these to calculate the source address
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval;
            if (j == colonIndex) {
                ndxval = ndxptr[j];
            } else {
                ndxval = ndx[j][ndxptr[j]] - 1;
            }
            srcadd += ndxval * srcfact[j];
        }
        // Copy the value
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[i * advance + k];
        }
        // Update the ndxset
        ndxptr[0]++;
        for (indexType j = 0; j < numDims - 1; j++) {
            if (ndxptr[j] >= outDims[j]) {
                ndxptr[j] = 0;
                ndxptr[j + 1]++;
            }
        }
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetSliceReal(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType colonIndex,
    indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // Calculate the start element
    indexType start = 0;
    for (indexType j = 0; j < numDims; j++) {
        indexType ndxval;
        if (j == colonIndex) {
            ndxval = 0;
        } else {
            ndxval = ndx[j][0] - 1;
        }
        start += ndxval * srcfact[j];
    }
    // Next, calculate the stride distance
    // we do this by setting the colon component to 1
    indexType stride = 0;
    for (indexType j = 0; j < numDims; j++) {
        indexType ndxval;
        if (j == colonIndex) {
            ndxval = 1;
        } else {
            ndxval = ndx[j][0] - 1;
        }
        stride += ndxval * srcfact[j];
    }
    stride -= start;
    indexType srcadd = start;
    // The output equation is easy now
    for (indexType i = 0; i < outCount; i++) {
        sp[srcadd] = *destp;
        destp += advance;
        srcadd += stride;
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetSliceBurst(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType colonIndex,
    indexType burstLen, indexType advance)
{
    // Calculate the number of output elements
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    // Initialize the ndxpointer to zero
    indexType ndxptr[maxDims];
    for (indexType j = 0; j < numDims; j++) {
        ndxptr[j] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // Calculate the start element
    indexType start = 0;
    for (indexType j = 0; j < numDims; j++) {
        indexType ndxval;
        if (j == colonIndex) {
            ndxval = 0;
        } else {
            ndxval = ndx[j][0] - 1;
        }
        start += ndxval * srcfact[j];
    }
    // Next, calculate the stride distance
    // we do this by setting the colon component to 1
    indexType stride = 0;
    for (indexType j = 0; j < numDims; j++) {
        indexType ndxval;
        if (j == colonIndex) {
            ndxval = 1;
        } else {
            ndxval = ndx[j][0] - 1;
        }
        stride += ndxval * srcfact[j];
    }
    stride -= start;
    indexType srcadd = start;
    // The output equation is easy now
    for (indexType i = 0; i < outCount; i++) {
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[k];
        }
        destp += advance * burstLen;
        srcadd += stride;
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetDispatchBurst(indexType colonIndex, T* srcptr, const T* destptr,
    indexType outDimsInt[maxDims], indexType srcDimsInt[maxDims], constIndexPtr* indx, indexType L,
    indexType burstLen, indexType advance)
{
    indexType elCount = 1;
    for (indexType i = 0; i < L; i++) {
        elCount *= outDimsInt[i];
    }
    if (colonIndex == ((indexType)-1)) {
        setNDimSubsetNoColonBurst<T>(
            srcptr, destptr, outDimsInt, srcDimsInt, indx, L, burstLen, advance);
    } else if (colonIndex == 0) {
        setNDimSubsetFirstColonBurst<T>(
            srcptr, destptr, outDimsInt, srcDimsInt, indx, L, burstLen, advance);
    } else if (elCount > srcDimsInt[colonIndex]) {
        setNDimSubsetAnyColonBurst<T>(
            srcptr, destptr, outDimsInt, srcDimsInt, indx, L, colonIndex, burstLen, advance);
    } else {
        setNDimSubsetSliceBurst<T>(
            srcptr, destptr, outDimsInt, srcDimsInt, indx, L, colonIndex, burstLen, advance);
    }
}
//=============================================================================
template <class T>
void
setNDimSubsetDispatchReal(indexType colonIndex, T* srcptr, const T* destptr,
    indexType outDimsInt[maxDims], indexType srcDimsInt[maxDims], constIndexPtr* indx, indexType L,
    indexType advance)
{
    indexType elCount = 1;
    for (indexType i = 0; i < L; i++) {
        elCount *= outDimsInt[i];
    }
    if (colonIndex == ((indexType)-1)) {
        setNDimSubsetNoColonReal<T>(srcptr, destptr, outDimsInt, srcDimsInt, indx, L, advance);
    } else if (colonIndex == 0) {
        setNDimSubsetFirstColonReal<T>(srcptr, destptr, outDimsInt, srcDimsInt, indx, L, advance);
    } else if (elCount > srcDimsInt[colonIndex]) {
        setNDimSubsetAnyColonReal<T>(
            srcptr, destptr, outDimsInt, srcDimsInt, indx, L, colonIndex, advance);
    } else {
        setNDimSubsetSliceReal<T>(
            srcptr, destptr, outDimsInt, srcDimsInt, indx, L, colonIndex, advance);
    }
}
//=============================================================================
// Set functions
//=============================================================================
/**
 * Take the contents of data, and insert this data.
 *
 * This requires the following steps:
 *  1. Compute the maximum along each dimension
 *  2. Compute the dimensions of the right hand side
 *  3. Check that data is either a scalar or the right size
 *  4. If necessary, zero-extend the variable.
 *  5. Copy in the result.
 *
 * This is true for integer arguments - not for logical ones.
 * Logical indices need to be converted into integer lists
 * before they can be used.
 */
void
ArrayOf::setNDimSubset(ArrayOfVector& index, ArrayOf& rightData)
{
    constIndexPtr* indx = nullptr;
    if (rightData.isEmpty()) {
        bool deleteAllowed = (rightData.getDataClass() == NLS_DOUBLE)
            || (getDataClass() == NLS_CHAR && rightData.getDataClass() == NLS_CHAR);
        if (!deleteAllowed) {
            Error(_W("Empty matrix of type double expected."));
        }
        deleteNDimSubset(index);
        return;
    }
    bool haveColonOperator = false;

    if (isEmpty()) {
        if (rightData.isVector()
            && (index.size() < 3 || index[0].getElementCount() == rightData.getElementCount())) {
            for (size_t i = 0; i < index.size(); i++) {
                if (isColonOperator(index[i])) {
                    haveColonOperator = true;
                    if (i == 0) {
                        index[i] = ArrayOf::integerRangeConstructor(
                            1, 1, rightData.getElementCount(), true);
                    } else {
                        index[i] = ArrayOf::integerRangeConstructor(1, 1, 1, true);
                    }
                }
            }
        } else {
            size_t colonDim = 0;
            for (auto& i : index) {
                if (isColonOperator(i)) {
                    haveColonOperator = true;
                    i = ArrayOf::integerRangeConstructor(
                        1, 1, rightData.getDimensionLength((int)(colonDim++)), true);
                }
            }
            if ((colonDim > 0) && (colonDim < rightData.getDimensions().getLength())) {
                Error(_W("Size mismatch in assignment A(I1,I2,...,In) = B."));
            }
        }
    }
    try {
        indexType L = index.size();
        Dimensions myDims(dp->dimensions);
        Dimensions outDims;
        bool anyEmpty;
        indexType colonIndex;

        indx = ProcessNDimIndexes(true, myDims, index, anyEmpty, colonIndex, outDims, false);
        if (anyEmpty) {
            delete[] indx;
            return;
        }
        Dimensions a(L);
        // First, we compute the maximum along each dimension.
        indexType dataCount = 1;
        for (int i = 0; i < L; i++) {
            if (isColonOperator(index[i])) {
                a[i] = myDims[i];
                dataCount *= myDims[i];
            } else {
                a[i] = index[i].getMaxAsIndex();
                dataCount *= index[i].getElementCount();
            }
        }
        if (isEmpty()) {
            if ((dataCount > rightData.getElementCount()) && !haveColonOperator) {
                Error(_W("Size mismatch in assignment A(I1,I2,...,In) = B."));
            }
        }
        // Next, we compute the dimensions of the right hand side
        indexType advance = 0;
        if (rightData.isSparse()) {
            rightData.makeDense();
        }
        if (rightData.isScalar()) {
            advance = 0;
        } else if (!isEmpty() && (rightData.getElementCount() == dataCount)) {
            advance = 1;
        } else if (!isEmpty()) {
            if (isStringArray() && rightData.isCharacterArray() && rightData.isRowVector()) {
                advance = 0;
            } else {
                Error(_W("Size mismatch in assignment A(I1,I2,...,In) = B."));
            }
        } else {
            advance = 1;
        }

        if (getDataClass() != rightData.getDataClass()) {
            if (isStringArray()) {
                bool needToOverload = false;
                ArrayOf promute = ArrayOf::toStringArray(rightData, needToOverload);
                if (needToOverload) {
                    Error(_W("Cannot promote to string array."));
                }
                rightData = promute;
            } else {
                if (!isEmpty()) {
                    bool isLeftComplex = isComplex();
                    bool isRightSingle = rightData.isSingleClass();
                    bool isLeftSingle = isSingleClass();
                    bool isRightDouble = rightData.isDoubleClass();
                    bool isLeftDouble = isDoubleClass();
                    if (isLeftSingle && isRightDouble) {
                        if (isLeftComplex) {
                            rightData.promoteType(NLS_SCOMPLEX, dp->fieldNames);
                        } else {
                            rightData.promoteType(NLS_SINGLE, dp->fieldNames);
                        }
                    } else if (isLeftDouble && isRightSingle) {
                        if (isLeftComplex) {
                            rightData.promoteType(NLS_DCOMPLEX, dp->fieldNames);
                        } else {
                            rightData.promoteType(NLS_DOUBLE, dp->fieldNames);
                        }
                    } else {
                        if (rightData.isComplex() && !isComplex()) {
                            promoteType(rightData.dp->dataClass, rightData.dp->fieldNames);
                        } else {
                            rightData.promoteType(dp->dataClass, dp->fieldNames);
                        }
                    }
                } else {
                    promoteType(rightData.dp->dataClass, rightData.dp->fieldNames);
                }
            }
        } else {
            if (!isEmpty() && (rightData.getDataClass() == NLS_STRUCT_ARRAY)
                && (getDataClass() == NLS_STRUCT_ARRAY)) {
                if (rightData.isFunctionHandle() && !isFunctionHandle()) {
                    Error(_W("Cannot promote to function_handle array."));
                }
                if (rightData.dp->fieldNames.size() > dp->fieldNames.size()) {
                    promoteType(NLS_STRUCT_ARRAY, rightData.dp->fieldNames);
                } else {
                    rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames);
                }
            }
            if (!isEmpty() && (rightData.getDataClass() == NLS_CLASS_ARRAY)
                && (getDataClass() == NLS_CLASS_ARRAY)) {
                if (rightData.getClassType() != getClassType()) {
                    Error(_W("Cannot promote to class array."));
                }
                if (rightData.dp->fieldNames.size() > dp->fieldNames.size()) {
                    promoteType(NLS_STRUCT_ARRAY, rightData.dp->fieldNames);
                } else {
                    rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames);
                }
            }
            if (!isEmpty() && (rightData.getDataClass() == NLS_FUNCTION_HANDLE)
                && (getDataClass() == NLS_FUNCTION_HANDLE)) {
                if (rightData.dp->fieldNames.size() > dp->fieldNames.size()) {
                    promoteType(NLS_STRUCT_ARRAY, rightData.dp->fieldNames);
                } else {
                    rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames);
                }
            }
        }

        if (isSparse()) {
            if (L > 2) {
                Error(_W("Multidimensional indexing not legal for sparse "
                         "arrays in assignment A(I1,I2,...,IN) = B"));
            }
            indexType rows = getDimensionLength(0);
            indexType cols = getDimensionLength(1);
            void* qp = SetSparseNDimSubsetsDynamicFunction(dp->dataClass, rows, cols, dp->getData(),
                static_cast<const indexType*>(indx[0]), outDims[0],
                static_cast<const indexType*>(indx[1]), outDims[1], rightData.getDataPointer(),
                static_cast<int>(advance));
            Dimensions newdim;
            newdim[0] = rows;
            newdim[1] = cols;
            dp = dp->putData(dp->dataClass, newdim, qp, true);
            return;
        }
        resize(a);
        if (a.getElementCount() < rightData.getElementCount()) {
            Error(_W("Size mismatch in assignment A(I1,I2,...,In) = B."));
        }
        myDims = dp->dimensions;
        // Get a writable data pointer
        void* qp = (void*)getDataPointer();
        indexType outDimsInt[maxDims];
        indexType srcDimsInt[maxDims];
        for (indexType i = 0; i < L; i++) {
            outDimsInt[i] = outDims[i];
            srcDimsInt[i] = myDims[i];
        }
        outDims.simplify();
        switch (dp->dataClass) {
        case NLS_SCOMPLEX:
            setNDimSubsetDispatchBurst<float>(colonIndex, static_cast<float*>(qp),
                static_cast<const float*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, 2, advance);
            break;
        case NLS_DCOMPLEX:
            setNDimSubsetDispatchBurst<double>(colonIndex, static_cast<double*>(qp),
                static_cast<const double*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, 2, advance);
            break;
        case NLS_LOGICAL:
            setNDimSubsetDispatchReal<logical>(colonIndex, static_cast<logical*>(qp),
                static_cast<const logical*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_SINGLE:
            setNDimSubsetDispatchReal<float>(colonIndex, static_cast<float*>(qp),
                static_cast<const float*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_DOUBLE:
            setNDimSubsetDispatchReal<double>(colonIndex, static_cast<double*>(qp),
                static_cast<const double*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_INT8:
            setNDimSubsetDispatchReal<int8>(colonIndex, static_cast<int8*>(qp),
                static_cast<const int8*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_UINT8:
            setNDimSubsetDispatchReal<uint8>(colonIndex, static_cast<uint8*>(qp),
                static_cast<const uint8*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_INT16:
            setNDimSubsetDispatchReal<int16>(colonIndex, static_cast<int16*>(qp),
                static_cast<const int16*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_UINT16:
            setNDimSubsetDispatchReal<uint16>(colonIndex, static_cast<uint16*>(qp),
                static_cast<const uint16*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_INT32:
            setNDimSubsetDispatchReal<int32>(colonIndex, static_cast<int32*>(qp),
                static_cast<const int32*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_UINT32:
            setNDimSubsetDispatchReal<uint32>(colonIndex, static_cast<uint32*>(qp),
                static_cast<const uint32*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_INT64:
            setNDimSubsetDispatchReal<int64>(colonIndex, static_cast<int64*>(qp),
                static_cast<const int64*>(rightData.getDataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_UINT64:
            setNDimSubsetDispatchReal<uint64>(colonIndex, static_cast<uint64*>(qp),
                static_cast<const uint64*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_HANDLE:
        case NLS_GO_HANDLE:
            setNDimSubsetDispatchReal<nelson_handle>(colonIndex, static_cast<nelson_handle*>(qp),
                static_cast<const nelson_handle*>(rightData.getDataPointer()), outDimsInt,
                srcDimsInt, indx, L, advance);
            break;
        case NLS_CHAR:
            setNDimSubsetDispatchReal<charType>(colonIndex, static_cast<charType*>(qp),
                static_cast<const charType*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_CELL_ARRAY:
            setNDimSubsetDispatchReal<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_STRING_ARRAY:
            setNDimSubsetDispatchReal<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_FUNCTION_HANDLE:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, dp->fieldNames.size(), advance);
            break;
        case NLS_CLASS_ARRAY:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, dp->fieldNames.size(), advance);
            break;
        case NLS_STRUCT_ARRAY:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.getDataPointer()), outDimsInt, srcDimsInt,
                indx, L, dp->fieldNames.size(), advance);
            break;
        default: {
        } break;
        }
        delete[] indx;
        dp->dimensions.simplify();
        dp->refreshDimensionCache();
    } catch (const Exception& e) {
        delete[] indx;
        throw e;
    }
}
//=============================================================================
/**
 * This is the vector version of the multidimensional replacement function.
 *
 * This requires the following steps:
 *  1. Compute the maximum along each dimension
 *  2. Check that data is either scalar or the right size.
 */
void
ArrayOf::setVectorSubset(ArrayOf& index, ArrayOf& rightData)
{
    if (index.isEmpty()) {
        return;
    }
    // Check the right-hand-side - if it is empty, then
    // we have a delete command in disguise.
    if (rightData.isEmpty()) {
        if (!rightData.isEmpty(true)) {
            Error(_W("Size mismatch in assignment A(I1,I2,...,In) = B."));
        }
        bool deleteAllowed = (rightData.getDataClass() == NLS_DOUBLE)
            || (getDataClass() == NLS_CHAR && rightData.getDataClass() == NLS_CHAR);
        if (!deleteAllowed) {
            Error(_W("Empty matrix of type double expected."));
        }
        deleteVectorSubset(index);
        return;
    }
    if (isColonOperator(index)) {
        if (rightData.isScalar()) {
            index = ArrayOf::integerRangeConstructor(1, 1, getElementCount(), true);
        } else {
            Dimensions myDims;
            if (!isEmpty()) {
                myDims = dp->dimensions;
            } else {
                myDims = rightData.getDimensions();
            }
            if (myDims.getElementCount() != rightData.getElementCount()) {
                Error(_("Assignment A(:) = B requires A and B to be the same size"));
            }
            dp = rightData.dp->getCopy();
            reshape(myDims);
            return;
        }
    }
    // Make sure the index is an ordinal type
    index.toOrdinalType();
    indexType index_length = index.getElementCount();
    if (index_length == 0) {
        return;
    }
    // Get a pointer to the index data set
    auto index_p = static_cast<constIndexPtr>(index.dp->getData());
    int advance = 0;
    // Set the right hand side advance pointer to
    //  - 0 if the rhs is a scalar
    //  - 1 else
    if (rightData.isSparse()) {
        rightData.makeDense();
    }
    if (isStringArray() && (rightData.isCharacterArray() && rightData.isRowVector())) {
        advance = 0;
    } else if (rightData.isScalar()) {
        advance = 0;
    } else if (rightData.getElementCount() == index_length) {
        advance = 1;
    } else {
        Error("Size mismatch in assignment A(I) = B.\n");
    }
    // Compute the maximum index;
    indexType max_index = index.getMaxAsIndex();
    // If the RHS type is superior to ours, we
    // force our type to agree with the inserted data.
    // Also, if we are empty, we promote ourselves (regardless of
    // our type).
    if (isStringArray()) {
        bool needToOverload = false;
        ArrayOf promute = ArrayOf::toStringArray(rightData, needToOverload);
        if (needToOverload) {
            Error(_W("Cannot promote to string array."));
        }
        rightData = promute;
    } else if (!isEmpty() && (rightData.getDataClass() == NLS_STRUCT_ARRAY)
        && (getDataClass() == NLS_STRUCT_ARRAY)) {
        if (rightData.isFunctionHandle() && !isFunctionHandle()) {
            Error(_W("Cannot promote to function_handle array."));
        }
        if (rightData.dp->fieldNames.size() > dp->fieldNames.size()) {
            promoteType(NLS_STRUCT_ARRAY, rightData.dp->fieldNames);
        } else {
            rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames);
        }
    } else if (!isEmpty() && (rightData.getDataClass() == NLS_CLASS_ARRAY)
        && (getDataClass() == NLS_CLASS_ARRAY)) {
        if (rightData.getClassType() != getClassType()) {
            Error(_W("Cannot promote to another class type array."));
        }
        if (rightData.dp->fieldNames.size() > dp->fieldNames.size()) {
            promoteType(NLS_CLASS_ARRAY, rightData.dp->fieldNames);
        } else {
            rightData.promoteType(NLS_CLASS_ARRAY, dp->fieldNames);
        }
    } else if (!isEmpty() && (rightData.getDataClass() == NLS_FUNCTION_HANDLE)
        && (getDataClass() == NLS_FUNCTION_HANDLE)) {
        if (rightData.dp->fieldNames.size() > dp->fieldNames.size()) {
            promoteType(NLS_FUNCTION_HANDLE, rightData.dp->fieldNames);
        } else {
            rightData.promoteType(NLS_FUNCTION_HANDLE, dp->fieldNames);
        }
    } else {
        if (isEmpty() || rightData.getDataClass() > getDataClass()) {
            bool isLeftComplex = isComplex();
            bool isRightSingle = rightData.isSingleClass();
            bool isLeftSingle = isSingleClass();
            bool isRightDouble = rightData.isDoubleClass();
            bool isLeftDouble = isDoubleClass();
            // If our type is superior to the RHS, we convert
            // the RHS to our type
            if (isLeftSingle && isRightDouble) {
                if (isLeftComplex) {
                    rightData.promoteType(NLS_SCOMPLEX, dp->fieldNames);
                } else {
                    rightData.promoteType(NLS_SINGLE, dp->fieldNames);
                }
            } else if (isLeftDouble && isRightSingle) {
                if (isLeftComplex) {
                    rightData.promoteType(NLS_DCOMPLEX, dp->fieldNames);
                } else {
                    rightData.promoteType(NLS_DOUBLE, dp->fieldNames);
                }
            } else {
                promoteType(rightData.getDataClass(), rightData.dp->fieldNames);
            }
        } else {
            if (rightData.getDataClass() <= dp->dataClass) {
                rightData.promoteType(dp->dataClass, dp->fieldNames);
            }
        }
    }
    // If the max index is larger than our current length, then
    // we have to resize ourselves - but this is only legal if we are
    // a vector.
    if (isSparse()) {
        if (dp->dataClass == NLS_LOGICAL) {
            rightData.promoteType(NLS_UINT64);
        }
        indexType rows = getDimensionLength(0);
        indexType cols = getDimensionLength(1);
        void* qp = SetSparseVectorSubsetsDynamicFunction(dp->dataClass, rows, cols, dp->getData(),
            static_cast<const indexType*>(index.dp->getData()), index.getDimensionLength(0),
            index.getDimensionLength(1), rightData.getDataPointer(), advance);
        Dimensions newdim;
        newdim[0] = rows;
        newdim[1] = cols;
        dp = dp->putData(dp->dataClass, newdim, qp, true);
        return;
    }
    vectorResize(max_index);
    // Get a writable data pointer
    void* qp = getReadWriteDataPointer();
    // Now, we copy data from the RHS to our real part,
    // computing indices along the way.
    indexType srcIndex = 0;
    indexType j;
    for (int i = 0; i < index_length; i++) {
        j = index_p[i] - 1;
        rightData.copyElements(srcIndex, qp, j, 1);
        srcIndex += advance;
    }
}
//=============================================================================
void
ArrayOf::setValue(const ArrayOf& value)
{
    if (dp && (dp->deleteCopy() <= 1)) {
        dp->freeDataBlock();
    }
    dp = value.dp->getCopy();
}
//=============================================================================
void
ArrayOf::setValueAtIndex(uint64 index, const ArrayOf& scalarValue)
{
    if (!scalarValue.isScalar()) {
        Error(ERROR_SCALAR_EXPECTED);
    }
    auto length = static_cast<uint64>(this->getElementCount());
    if (index >= length) {
        Error(_W("Index exceeds matrix dimensions."));
    }
    // call insertion overloading here for not supported types
    if (isSparse()) {
        indexType rows = getDimensionLength(0);
        indexType cols = getDimensionLength(1);
        auto indx = static_cast<indexType>(index);
        void* qp = SetSparseVectorSubsetsDynamicFunction(
            dp->dataClass, rows, cols, dp->getData(), &indx, 1, 1, scalarValue.getDataPointer(), 0);
        Dimensions newdim;
        newdim[0] = rows;
        newdim[1] = cols;
        dp = dp->putData(dp->dataClass, newdim, qp, true);
    } else {
        indexType elSize(getElementSize());
        char* ptr = static_cast<char*>(getReadWriteDataPointer());
        const char* val = static_cast<const char*>(scalarValue.getDataPointer());
        memcpy(ptr + index * elSize, val, scalarValue.getByteSize());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
