//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "ArrayOfAssignConversion.hpp"
#include "ArrayOfIndexPlan.hpp"
#include "ArrayOfView.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "SparseType.hpp"
#include "Types.hpp"
#include "HandleState.hpp"
#include "MutableArrayOfView.hpp"
#include "ArrayOf_AssignFastPath.hpp"
#include <algorithm>
#include <cmath>
#include <cstring>
//=============================================================================
namespace Nelson {
//=============================================================================
static inline void
dropAllZeroImaginarySparse(ArrayOf& array)
{
    // An element assignment that removes the last imaginary entry of a sparse
    // complex matrix canonicalizes it back to a real sparse matrix.
    if (array.isSparse() && array.isComplex() && array.allReal()) {
        array.promoteType(array.dataClass() == NLS_SCOMPLEX ? NLS_SINGLE : NLS_DOUBLE);
    }
}
//=============================================================================
static bool
isStringScalarAssignmentValue(const ArrayOf& value)
{
    return (value.isCharacterArray() && value.isRowVector())
        || (value.isStringArray() && value.isScalar());
}
//=============================================================================
static ArrayOf
stringScalarAssignmentElement(const ArrayOf& value)
{
    if (value.isStringArray()) {
        const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
        return elements[0];
    }
    return value;
}
//=============================================================================
static bool
isCellScalarAssignmentValue(const ArrayOf& value)
{
    return value.isCell() && value.isScalar();
}
//=============================================================================
static ArrayOf
cellScalarAssignmentElement(const ArrayOf& value)
{
    const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
    return elements[0];
}
//=============================================================================
static void
checkValueFitsEmptySelection(const ArrayOf& rightData, bool destinationIsString, bool linear)
{
    // a selection without elements accepts a scalar (or text for a string array)
    if (rightData.isScalar() || (destinationIsString && isStringScalarAssignmentValue(rightData))) {
        return;
    }
    if (linear) {
        Error(
            L"Nelson:dimagree:sizeMismatchAssignmentAI", L"Size mismatch in assignment A(I) = B.");
    }
    Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
        L"Size mismatch in assignment A(I1,I2,...,In) = B.");
}
//=============================================================================
static bool
isSingleFullColonIndex(const ArrayOfVector& index)
{
    return index.size() == 1 && isColonOperator(index[0]);
}
//=============================================================================
static bool
isAscendingUnitStrideIndex(constIndexPtr index, indexType length)
{
    if (length == 0 || index[0] == 0) {
        return false;
    }
    for (indexType i = 1; i < length; ++i) {
        if (index[i] != index[i - 1] + 1) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static NelsonType
ordinalIndexClassForAssignView();
//=============================================================================
static bool
trySetFullColonCellScalar(
    ArrayOf& destination, const ArrayOfVector& index, const ArrayOf& rightData)
{
    if (destination.dataClass() != NLS_CELL_ARRAY || !isSingleFullColonIndex(index)
        || !rightData.isCell() || !rightData.isScalar()) {
        return false;
    }
    const auto* rightElements = static_cast<const ArrayOf*>(rightData.dataPointer());
    ArrayOf scalarValue = rightElements[0];
    MutableArrayOfView<ArrayOf> elements(destination, NLS_CELL_ARRAY);
    const indexType elementCount = destination.elementCount();
    std::fill_n(elements.data(), elementCount, scalarValue);
    return true;
}
//=============================================================================
static bool
trySetFullColonStringScalar(
    ArrayOf& destination, const ArrayOfVector& index, const ArrayOf& rightData)
{
    if (destination.dataClass() != NLS_STRING_ARRAY || !isSingleFullColonIndex(index)
        || !isStringScalarAssignmentValue(rightData)) {
        return false;
    }
    ArrayOf scalarValue = stringScalarAssignmentElement(rightData);
    MutableArrayOfView<ArrayOf> elements(destination, NLS_STRING_ARRAY);
    const indexType elementCount = destination.elementCount();
    std::fill_n(elements.data(), elementCount, scalarValue);
    return true;
}
//=============================================================================
static bool
trySetVectorArrayOfScalarNoResize(
    ArrayOf& destination, const ArrayOfVector& index, const ArrayOf& scalarValue)
{
    if (index.size() != 1 || isColonOperator(index[0]) || !index[0].isDoubleType(true)
        || index[0].isSparse()) {
        return false;
    }
    const auto selectedCount = index[0].elementCount();
    if (selectedCount == 0) {
        return true;
    }
    const indexType elementCount = destination.elementCount();
    const auto* selected = static_cast<const double*>(index[0].dataPointer());
    for (indexType k = 0; k < selectedCount; ++k) {
        const double scalarIndex = selected[k];
        if (!std::isfinite(scalarIndex) || scalarIndex < 1.0
            || scalarIndex > static_cast<double>(elementCount)) {
            return false;
        }
        const auto ordinal = static_cast<indexType>(scalarIndex);
        if (scalarIndex != static_cast<double>(ordinal)) {
            return false;
        }
    }
    MutableArrayOfView<ArrayOf> elements(destination, destination.dataClass());
    for (indexType k = 0; k < selectedCount; ++k) {
        elements[static_cast<indexType>(selected[k]) - 1] = scalarValue;
    }
    return true;
}
//=============================================================================
static bool
trySetVectorCellScalarNoResize(
    ArrayOf& destination, const ArrayOfVector& index, const ArrayOf& rightData)
{
    if (destination.dataClass() != NLS_CELL_ARRAY || !rightData.isCell() || !rightData.isScalar()) {
        return false;
    }
    const auto* rightElements = static_cast<const ArrayOf*>(rightData.dataPointer());
    return trySetVectorArrayOfScalarNoResize(destination, index, rightElements[0]);
}
//=============================================================================
static bool
trySetVectorStringScalarNoResize(
    ArrayOf& destination, const ArrayOfVector& index, const ArrayOf& rightData)
{
    if (destination.dataClass() != NLS_STRING_ARRAY || !isStringScalarAssignmentValue(rightData)) {
        return false;
    }
    return trySetVectorArrayOfScalarNoResize(
        destination, index, stringScalarAssignmentElement(rightData));
}
//=============================================================================
template <class T>
void
setNDimSubsetNoColonReal(T* sp, const T* destp, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* ndx, indexType numDims, indexType advance)
{
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        sp[srcadd] = *destp;
        destp += advance;
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
    indexType outCount = 1;
    indexType ndxptr[maxDims];
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
        ndxptr[i] = 0;
    }
    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[k];
        }
        destp += burstLen * advance;
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
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    indexType numrows = outDims[0];
    for (indexType i = 0; i < outCount; i += numrows) {
        indexType srcadd = 0;
        for (indexType j = 1; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        for (indexType k = 0; k < numrows; k++) {
            sp[srcadd + k] = *destp;
            destp += advance;
        }
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
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    indexType numrows = outDims[0];
    for (indexType i = 0; i < outCount; i += numrows) {
        indexType srcadd = 0;
        for (indexType j = 1; j < numDims; j++) {
            indexType ndxval = ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[i * burstLen * advance + k];
        }
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
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval = (j == colonIndex) ? ndxptr[j] : ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        sp[srcadd] = *destp;
        destp += advance;
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
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    for (indexType i = 0; i < outCount; i++) {
        indexType srcadd = 0;
        for (indexType j = 0; j < numDims; j++) {
            indexType ndxval = (j == colonIndex) ? ndxptr[j] : ndx[j][ndxptr[j]] - 1;
            srcadd += ndxval * srcfact[j];
        }
        for (indexType k = 0; k < burstLen; k++) {
            sp[burstLen * srcadd + k] = destp[i * advance + k];
        }
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
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
    // the stride is found by setting the colon component to 1
    indexType start = 0;
    indexType stride = 0;
    for (indexType j = 0; j < numDims; j++) {
        indexType ndxval = ndx[j][0] - 1;
        start += (j == colonIndex ? 0 : ndxval) * srcfact[j];
        stride += (j == colonIndex ? 1 : ndxval) * srcfact[j];
    }
    stride -= start;
    indexType srcadd = start;
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
    indexType outCount = 1;
    for (indexType i = 0; i < numDims; i++) {
        outCount *= outDims[i];
    }
    indexType ndxptr[maxDims] = { 0 };

    indexType srcfact[maxDims];
    srcfact[0] = 1;
    for (indexType j = 1; j < numDims; j++) {
        srcfact[j] = srcfact[j - 1] * srcDims[j - 1];
    }
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
    // the stride is found by setting the colon component to 1
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
/**
 * Take the contents of data, and insert this data.
 * Logical indices must be converted into integer lists before they can be used.
 */
void
ArrayOf::setNDimSubset(ArrayOfVector& index, ArrayOf& rightData)
{
    materializeScalar();
    constIndexPtr* indx = nullptr;
    bool isStringScalarAssignment = isStringArray() && isStringScalarAssignmentValue(rightData);
    bool isStringEmptyAssign
        = (dataClass() == NLS_STRING_ARRAY) && (rightData.dataClass() == NLS_CHAR);

    if (rightData.isEmpty() && !isStringEmptyAssign) {
        // A subscript that is an all-false logical mask selects zero elements,
        // so the whole selection is empty and an empty right-hand side is a
        // no-op regardless of its class (e.g. single([]) must not be rejected
        // as non-double).
        auto isAllFalseLogical = [](const ArrayOf& item) {
            if (item.dataClass() != NLS_LOGICAL) {
                return false;
            }
            indexType nbIndex = item.elementCount();
            const logical* mat = (const logical*)item.dataPointer();
            return std::all_of(mat, mat + nbIndex, [](logical v) { return v == false; });
        };
        if (!rightData.isEmpty(true)) {
            // an empty right-hand side other than [] is an assignment, not a
            // deletion: it only fits a selection without elements
            Dimensions selectable = dp->dimensions_;
            if (static_cast<indexType>(index.size()) < selectable.length()) {
                selectable = selectable.foldForSubscriptCount(index.size());
            }
            bool selectsNothing = false;
            for (size_t i = 0; i < index.size(); i++) {
                if (isColonOperator(index[i])) {
                    selectsNothing = selectsNothing
                        || selectable.dimensionLength(static_cast<indexType>(i)) == 0;
                } else {
                    selectsNothing
                        = selectsNothing || index[i].isEmpty() || isAllFalseLogical(index[i]);
                }
            }
            if (!selectsNothing) {
                Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                    L"Size mismatch in assignment A(I1,I2,...,In) = B.");
            }
            return;
        }
        if (std::any_of(index.begin(), index.end(), isAllFalseLogical)) {
            return;
        }
        bool deleteAllowed
            = (rightData.dataClass() == NLS_DOUBLE || rightData.dataClass() == NLS_DCOMPLEX)
            || (dataClass() == NLS_CHAR && rightData.dataClass() == NLS_CHAR);
        if (!deleteAllowed) {
            Error(L"Nelson:types:emptyDoubleMatrixExpected",
                L"Empty matrix of type double expected.");
        }
        deleteNDimSubset(index);
        return;
    }
    convertMissingAssignmentValue(rightData);

    // A logical subscript selects the positions of its true entries. Converting it to an
    // ordinal list makes an all-false mask select nothing instead of the numeric index 0.
    for (size_t i = 0; i < index.size(); i++) {
        if (index[i].isLogical()) {
            index[i].toOrdinalType();
        }
    }

    if (!isEmpty() && !isSparse()
        && (trySetFullColonCellScalar(*this, index, rightData)
            || trySetFullColonStringScalar(*this, index, rightData)
            || trySetVectorCellScalarNoResize(*this, index, rightData)
            || trySetVectorStringScalarNoResize(*this, index, rightData))) {
        return;
    }
    bool haveColonOperator = false;

    if (isEmpty() && !isEmpty(true) && index.size() > 1) {
        // A partially empty array (e.g. 2x0) is addressed like any other array:
        // ':' spans the current extent of its dimension, even when that extent
        // is zero (then nothing is assigned), and the other subscripts may still
        // grow the array: zeros(2,0)(:,1) = 1 gives [1;1], zeros(2,0)(3,:) = 1
        // gives a 3x0 array.
        Dimensions grown(dp->dimensions_);
        bool selectsNothing = false;
        for (size_t i = 0; i < index.size(); i++) {
            if (isColonOperator(index[i])) {
                haveColonOperator = true;
                indexType extent = dp->dimensions_.dimensionLength(static_cast<indexType>(i));
                index[i] = ArrayOf::integerRange(1, 1, extent, true);
                selectsNothing = selectsNothing || (extent == 0);
            } else if (index[i].isEmpty()) {
                selectsNothing = true;
            } else {
                index[i].toOrdinalType();
                indexType largest = index[i].maxAsIndex();
                if (largest > grown.dimensionLength(static_cast<indexType>(i))) {
                    grown[static_cast<indexType>(i)] = largest;
                }
            }
        }
        if (selectsNothing) {
            // nothing is assigned, but the value must still fit the empty selection
            if (!rightData.isScalar() && !rightData.isEmpty()) {
                Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                    L"Size mismatch in assignment A(I1,I2,...,In) = B.");
            }
            resize(grown);
            return;
        }
    } else if (isEmpty()) {
        if (rightData.isVector()
            && (index.size() < 3 || index[0].elementCount() == rightData.elementCount())) {
            for (size_t i = 0; i < index.size(); i++) {
                if (isColonOperator(index[i])) {
                    haveColonOperator = true;
                    if (i == 0) {
                        index[i] = ArrayOf::integerRange(1, 1, rightData.elementCount(), true);
                    } else {
                        index[i] = ArrayOf::integerRange(1, 1, 1, true);
                    }
                }
            }
        } else {
            size_t colonDim = 0;
            for (auto& i : index) {
                if (isColonOperator(i)) {
                    haveColonOperator = true;
                    i = ArrayOf::integerRange(
                        1, 1, rightData.dimensionLength((int)(colonDim++)), true);
                }
            }
            if ((colonDim > 0) && (colonDim < rightData.dimensions().length())) {
                Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                    L"Size mismatch in assignment A(I1,I2,...,In) = B.");
            }
        }
    }
    try {
        indexType L = index.size();
        Dimensions myDims(dp->dimensions_);
        // With fewer subscripts than dimensions, the last subscript addresses the product of the
        // trailing dimensions. In column-major order this folding is contiguous, so a shape with
        // those dimensions collapsed addresses the same elements. The array never grows through
        // folded subscripts.
        bool foldTrailing = !isEmpty() && (L >= 2) && (L < myDims.length());
        Dimensions indexDims = foldTrailing ? myDims.foldForSubscriptCount(L) : myDims;
        Dimensions outDims;
        bool anyEmpty;
        indexType colonIndex;

        IndexPlan plan(true, indexDims, index, false);
        anyEmpty = plan.anyEmpty();
        colonIndex = plan.colonIndex();
        outDims = plan.outDims();
        indx = plan.data();
        if (anyEmpty) {
            checkValueFitsEmptySelection(rightData, isStringArray(), false);
            return;
        }
        convertAssignmentValueToDestinationClass(*this, rightData);
        Dimensions a(L);
        indexType dataCount = 1;
        for (int i = 0; i < L; i++) {
            if (isColonOperator(index[i])) {
                a[i] = indexDims[i];
                dataCount *= indexDims[i];
            } else {
                a[i] = index[i].maxAsIndex();
                dataCount *= index[i].elementCount();
            }
        }
        if (isEmpty()) {
            // A scalar right-hand side is replicated by the assignment loop, so A(1:3,1:3) = 7 can
            // grow an empty array.
            if ((dataCount > rightData.elementCount()) && !haveColonOperator
                && !rightData.isScalar()) {
                Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                    L"Size mismatch in assignment A(I1,I2,...,In) = B.");
            }
        }
        indexType advance = 0;
        if (rightData.isSparse()) {
            rightData.makeDense();
        }
        if (rightData.isScalar()) {
            advance = 0;
        } else if (!isEmpty() && (rightData.elementCount() == dataCount)) {
            advance = 1;
        } else if (!isEmpty()) {
            if (isStringArray() && rightData.isCharacterArray()
                && (rightData.isRowVector() || rightData.isEmpty())) {
                advance = 0;
            } else {
                Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                    L"Size mismatch in assignment A(I1,I2,...,In) = B.");
            }
        } else {
            advance = 1;
        }

        if (dataClass() != rightData.dataClass()) {
            if (isStringArray()) {
                if (!isStringScalarAssignment) {
                    bool needToOverload = false;
                    ArrayOf promute = ArrayOf::toStringArray(rightData, needToOverload);
                    if (needToOverload) {
                        Error(L"Nelson:types:conversionNotSupported",
                            L"Cannot promote to %s array.", L"string");
                    }
                    rightData = promute;
                    isStringScalarAssignment = rightData.isScalar();
                }
            } else {
                if (!isEmpty()) {
                    if (rightData.isComplex() && !isComplex()) {
                        promoteType(rightData.dataClass(), rightData.fieldNames());
                    } else {
                        rightData.promoteType(dp->dataClass, dp->fieldNames());
                    }
                } else {
                    promoteType(rightData.dataClass(), rightData.fieldNames());
                }
            }
        } else {
            if (!isEmpty() && (rightData.dataClass() == NLS_STRUCT_ARRAY)
                && (dataClass() == NLS_STRUCT_ARRAY)) {
                if (rightData.isFunctionHandle() && !isFunctionHandle()) {
                    Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                        L"function_handle");
                }
                if (rightData.fieldNames().size() > dp->fieldNames().size()) {
                    promoteType(NLS_CLASS_ARRAY, rightData.fieldNames());
                } else {
                    rightData.promoteType(NLS_CLASS_ARRAY, dp->fieldNames());
                }
            }
            if (!isEmpty() && (rightData.dataClass() == NLS_CLASS_ARRAY)
                && (dataClass() == NLS_CLASS_ARRAY)) {
                if (rightData.classType() != classType()) {
                    Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                        L"class");
                }
                if (rightData.fieldNames().size() > dp->fieldNames().size()) {
                    promoteType(NLS_STRUCT_ARRAY, rightData.fieldNames());
                } else {
                    rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames());
                }
            }
            if (!isEmpty() && (rightData.dataClass() == NLS_CLASSDEF_ARRAY)
                && (dataClass() == NLS_CLASSDEF_ARRAY)) {
                if (rightData.classType() != classType()) {
                    Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                        L"class");
                }
                if (rightData.fieldNames().size() > dp->fieldNames().size()) {
                    promoteType(NLS_CLASSDEF_ARRAY, rightData.fieldNames());
                } else {
                    rightData.promoteType(NLS_CLASSDEF_ARRAY, dp->fieldNames());
                }
            }
            if (!isEmpty() && (rightData.dataClass() == NLS_FUNCTION_HANDLE)
                && (dataClass() == NLS_FUNCTION_HANDLE)) {
                if (rightData.fieldNames().size() > dp->fieldNames().size()) {
                    promoteType(NLS_STRUCT_ARRAY, rightData.fieldNames());
                } else {
                    rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames());
                }
            }
        }

        if (rightData.isClassType() && isClassType() && rightData.dataClass() == dataClass()) {
            setClassType(rightData.classType());
            if (rightData.isClassdefType()) {
                setClassdefLayout(rightData.classdefLayout());
            }
        }

        if (isSparse()) {
            if (L > 2) {
                Error(L"Nelson:types:sparseNDimIndexing",
                    L"Multidimensional indexing (more than 2 dimensions) not legal for sparse "
                    L"arrays.");
            }
            indexType rows = dimensionLength(0);
            indexType cols = dimensionLength(1);
            void* sparseData = sparseReadWriteDataPointer();
            void* qp = Eigen_SetSparseNDimSubsets(dp->dataClass, rows, cols, sparseData,
                static_cast<const indexType*>(indx[0]), outDims[0],
                static_cast<const indexType*>(indx[1]), outDims[1], rightData.dataPointer(),
                static_cast<int>(advance));
            Dimensions newdim(rows, cols);
            dp = dp->putData(dp->dataClass, newdim, qp, true);
            dropAllZeroImaginarySparse(*this);
            return;
        }
        if (foldTrailing) {
            // The final subscript addresses the fixed product of the trailing
            // dimensions and cannot grow it; the leading subscripts may grow
            // their own dimension, as ordinary N-D assignment does.
            if (a[L - 1] > indexDims[L - 1]) {
                Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
            }
            bool needGrow = false;
            Dimensions growTarget(myDims);
            for (indexType i = 0; i < L - 1; i++) {
                if (a[i] > growTarget.dimensionLength(i)) {
                    growTarget[i] = a[i];
                    needGrow = true;
                }
            }
            if (needGrow) {
                resize(growTarget);
                // Recompute the folded shape from the grown dimensions: only the
                // leading extents (and thus the column-major strides) change, the
                // trailing product is unchanged.
                indexDims = dp->dimensions_.foldForSubscriptCount(L);
            }
        } else {
            resize(a);
        }
        if (!isStringScalarAssignment && dataCount < rightData.elementCount()) {
            Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                L"Size mismatch in assignment A(I1,I2,...,In) = B.");
        }
        myDims = dp->dimensions_;
        void* qp = readWriteDataPointer();
        indexType outDimsInt[maxDims];
        indexType srcDimsInt[maxDims];
        for (indexType i = 0; i < L; i++) {
            outDimsInt[i] = outDims[i];
            srcDimsInt[i] = foldTrailing ? indexDims[i] : myDims[i];
        }
        outDims.simplify();
        if (dp->dataClass == NLS_STRING_ARRAY && isStringScalarAssignment) {
            ArrayOf scalarValue = stringScalarAssignmentElement(rightData);
            if (L == 1) {
                auto* elements = static_cast<ArrayOf*>(qp);
                const indexType nbElements = elementCount();
                if (colonIndex == 0) {
                    std::fill_n(elements, nbElements, scalarValue);
                } else {
                    const auto* selected = static_cast<const indexType*>(indx[0]);
                    const indexType selectedCount = dataCount;
                    for (indexType i = 0; i < selectedCount; i++) {
                        indexType targetIndex = selected[i] - 1;
                        if (targetIndex < 0 || targetIndex >= nbElements) {
                            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
                        }
                        elements[targetIndex] = scalarValue;
                    }
                }
                dp->dimensions_.simplify();
                dp->refreshDimensionCache();
                return;
            }
            setNDimSubsetDispatchReal<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp), &scalarValue,
                outDimsInt, srcDimsInt, indx, L, 0);
            dp->dimensions_.simplify();
            dp->refreshDimensionCache();
            return;
        }
        if (dp->dataClass == NLS_CELL_ARRAY && rightData.isCell() && rightData.isScalar()) {
            ArrayOf scalarValue = static_cast<const ArrayOf*>(rightData.dataPointer())[0];
            if (L == 1) {
                auto* elements = static_cast<ArrayOf*>(qp);
                const indexType nbElements = elementCount();
                if (colonIndex == 0) {
                    std::fill_n(elements, nbElements, scalarValue);
                } else {
                    const auto* selected = static_cast<const indexType*>(indx[0]);
                    const indexType selectedCount = dataCount;
                    for (indexType i = 0; i < selectedCount; i++) {
                        indexType targetIndex = selected[i] - 1;
                        if (targetIndex < 0 || targetIndex >= nbElements) {
                            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
                        }
                        elements[targetIndex] = scalarValue;
                    }
                }
                dp->dimensions_.simplify();
                dp->refreshDimensionCache();
                return;
            }
            setNDimSubsetDispatchReal<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp), &scalarValue,
                outDimsInt, srcDimsInt, indx, L, 0);
            dp->dimensions_.simplify();
            dp->refreshDimensionCache();
            return;
        }
        if (trySetContiguousBlocks(
                rightData, qp, outDimsInt, srcDimsInt, indx, L, colonIndex, advance)) {
            if (dp->dataClass == NLS_GO_HANDLE) {
                refreshGraphicsObjectClassName();
            }
            dp->dimensions_.simplify();
            dp->refreshDimensionCache();
            return;
        }
        switch (dp->dataClass) {
        case NLS_SCOMPLEX:
            setNDimSubsetDispatchBurst<float>(colonIndex, static_cast<float*>(qp),
                static_cast<const float*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                2, advance);
            break;
        case NLS_DCOMPLEX:
            setNDimSubsetDispatchBurst<double>(colonIndex, static_cast<double*>(qp),
                static_cast<const double*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, 2, advance);
            break;
        case NLS_LOGICAL:
            setNDimSubsetDispatchReal<logical>(colonIndex, static_cast<logical*>(qp),
                static_cast<const logical*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_SINGLE:
            setNDimSubsetDispatchReal<float>(colonIndex, static_cast<float*>(qp),
                static_cast<const float*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                advance);
            break;
        case NLS_DOUBLE:
            setNDimSubsetDispatchReal<double>(colonIndex, static_cast<double*>(qp),
                static_cast<const double*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_INT8:
            setNDimSubsetDispatchReal<int8>(colonIndex, static_cast<int8*>(qp),
                static_cast<const int8*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                advance);
            break;
        case NLS_UINT8:
            setNDimSubsetDispatchReal<uint8>(colonIndex, static_cast<uint8*>(qp),
                static_cast<const uint8*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                advance);
            break;
        case NLS_INT16:
            setNDimSubsetDispatchReal<int16>(colonIndex, static_cast<int16*>(qp),
                static_cast<const int16*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                advance);
            break;
        case NLS_UINT16:
            setNDimSubsetDispatchReal<uint16>(colonIndex, static_cast<uint16*>(qp),
                static_cast<const uint16*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_INT32:
            setNDimSubsetDispatchReal<int32>(colonIndex, static_cast<int32*>(qp),
                static_cast<const int32*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                advance);
            break;
        case NLS_UINT32:
            setNDimSubsetDispatchReal<uint32>(colonIndex, static_cast<uint32*>(qp),
                static_cast<const uint32*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_INT64:
            setNDimSubsetDispatchReal<int64>(colonIndex, static_cast<int64*>(qp),
                static_cast<const int64*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx, L,
                advance);
            break;
        case NLS_UINT64:
            setNDimSubsetDispatchReal<uint64>(colonIndex, static_cast<uint64*>(qp),
                static_cast<const uint64*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_HANDLE:
            setNDimSubsetDispatchReal<HandleRef>(colonIndex, static_cast<HandleRef*>(qp),
                static_cast<const HandleRef*>(rightData.dataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            break;
        case NLS_GO_HANDLE:
            setNDimSubsetDispatchReal<nelson_handle>(colonIndex, static_cast<nelson_handle*>(qp),
                static_cast<const nelson_handle*>(rightData.dataPointer()), outDimsInt, srcDimsInt,
                indx, L, advance);
            refreshGraphicsObjectClassName();
            break;
        case NLS_CHAR:
            setNDimSubsetDispatchReal<charType>(colonIndex, static_cast<charType*>(qp),
                static_cast<const charType*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_CELL_ARRAY:
            setNDimSubsetDispatchReal<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_STRING_ARRAY:
            setNDimSubsetDispatchReal<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, advance);
            break;
        case NLS_FUNCTION_HANDLE:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, dp->fieldNames().size(), advance);
            break;
        case NLS_CLASS_ARRAY:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, dp->fieldNames().size(), advance);
            break;
        case NLS_CLASSDEF_ARRAY:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, dp->fieldNames().size(), advance);
            break;
        case NLS_STRUCT_ARRAY:
            setNDimSubsetDispatchBurst<ArrayOf>(colonIndex, static_cast<ArrayOf*>(qp),
                static_cast<const ArrayOf*>(rightData.dataPointer()), outDimsInt, srcDimsInt, indx,
                L, dp->fieldNames().size(), advance);
            break;
        default: {
        } break;
        }
        dp->dimensions_.simplify();
        dp->refreshDimensionCache();
    } catch (const NelsonException&) {
        throw;
    }
}
//=============================================================================
/**
 * Vector version of the multidimensional replacement function.
 */
void
ArrayOf::setVectorSubset(ArrayOf& index, ArrayOf& rightData)
{
    if (index.isEmpty() && !(rightData.isEmpty(true) && rightData.dataClass() == NLS_DOUBLE)) {
        // nothing is selected; only A([]) = [] still reshapes A into a row
        if (!rightData.isEmpty()) {
            checkValueFitsEmptySelection(rightData, isStringArray(), true);
        }
        return;
    }
    materializeScalar();
    // an empty right-hand side is a delete in disguise
    if (rightData.isEmpty()) {
        bool isIndexAllFalse = false;
        if (index.dataClass() == NLS_LOGICAL) {
            indexType nbIndex = index.elementCount();
            logical* mat = (logical*)index.dataPointer();
            isIndexAllFalse = std::all_of(mat, mat + nbIndex, [](logical v) { return v == false; });
        }
        // An all-false logical index selects zero elements, so an empty right-hand side is a
        // no-op regardless of its class (e.g. single([]) or int8([]) must not be rejected).
        if (isIndexAllFalse) {
            return;
        }
        bool lhsAndRhsEmpty = isEmpty() && rightData.isEmpty();
        if (!rightData.isEmpty(true) && !lhsAndRhsEmpty && !isIndexAllFalse) {
            Error(L"Nelson:dimagree:sizeMismatchAssignmentAI1I2",
                L"Size mismatch in assignment A(I1,I2,...,In) = B.");
        }
        bool deleteAllowed
            = (rightData.dataClass() == NLS_DOUBLE || rightData.dataClass() == NLS_DCOMPLEX)
            || (dataClass() == NLS_CHAR && rightData.dataClass() == NLS_CHAR);
        if (!deleteAllowed) {
            Error(L"Nelson:types:emptyDoubleMatrixExpected",
                L"Empty matrix of type double expected.");
        }
        deleteVectorSubset(index);
        return;
    }
    convertMissingAssignmentValue(rightData);
    if (isColonOperator(index)) {
        convertAssignmentValueToDestinationClass(*this, rightData);
        if (isStringArray() && isStringScalarAssignmentValue(rightData)) {
            ArrayOf scalarValue = stringScalarAssignmentElement(rightData);
            auto* elements = static_cast<ArrayOf*>(readWriteDataPointer());
            indexType nbElements = elementCount();
            std::fill_n(elements, nbElements, scalarValue);
            dp->dimensions_.simplify();
            dp->refreshDimensionCache();
            return;
        }
        if (rightData.isScalar()) {
            if (dataClass() == NLS_CELL_ARRAY && isCellScalarAssignmentValue(rightData)
                && !isSparse()) {
                ArrayOf scalarValue = cellScalarAssignmentElement(rightData);
                auto* elements = static_cast<ArrayOf*>(readWriteDataPointer());
                indexType nbElements = elementCount();
                std::fill_n(elements, nbElements, scalarValue);
                dp->dimensions_.simplify();
                dp->refreshDimensionCache();
                return;
            }
            index = ArrayOf::integerRange(1, 1, elementCount(), true);
        } else {
            Dimensions myDims;
            if (!isEmpty()) {
                myDims = dp->dimensions_;
            } else {
                myDims = rightData.dimensions();
            }
            if (myDims.elementCount() != rightData.elementCount()) {
                Error("Nelson:dimagree:assignmentABRequiresAB",
                    "Assignment A(:) = B requires A and B to be the same size");
            }
            deleteContents();
            copyObject(rightData);
            reshape(myDims);
            return;
        }
    }
    index.toOrdinalType();
    indexType index_length = index.elementCount();
    if (index_length == 0) {
        checkValueFitsEmptySelection(rightData, isStringArray(), true);
        return;
    }
    convertAssignmentValueToDestinationClass(*this, rightData);
    ArrayOfView<indexType> indexView(index, ordinalIndexClassForAssignView());
    auto index_p = indexView.data();
    int advance = 0;
    if (rightData.isSparse()) {
        rightData.makeDense();
    }
    if (isStringArray() && (rightData.isCharacterArray() && rightData.isRowVector())) {
        advance = 0;
    } else if (rightData.isScalar()) {
        advance = 0;
    } else if (rightData.elementCount() == index_length) {
        advance = 1;
    } else {
        Error("Nelson:dimagree:sizeMismatchAssignmentAI", "Size mismatch in assignment A(I) = B.");
    }
    indexType max_index = index.maxAsIndex();
    bool isStringScalarAssignment = isStringArray() && isStringScalarAssignmentValue(rightData);
    // A higher RHS class forces our class to agree with the inserted data.
    // An empty array is promoted regardless of its type.
    if (isStringArray()) {
        if (!isStringScalarAssignment) {
            bool needToOverload = false;
            ArrayOf promute = ArrayOf::toStringArray(rightData, needToOverload);
            if (needToOverload) {
                Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                    L"string");
            }
            rightData = promute;
        }
    } else if (!isEmpty() && (rightData.dataClass() == NLS_STRUCT_ARRAY)
        && (dataClass() == NLS_STRUCT_ARRAY)) {
        if (rightData.isFunctionHandle() && !isFunctionHandle()) {
            Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                L"function_handle");
        }
        if (rightData.fieldNames().size() > dp->fieldNames().size()) {
            promoteType(NLS_STRUCT_ARRAY, rightData.fieldNames());
        } else {
            rightData.promoteType(NLS_STRUCT_ARRAY, dp->fieldNames());
        }
    } else if (!isEmpty() && (rightData.dataClass() == NLS_CLASS_ARRAY)
        && (dataClass() == NLS_CLASS_ARRAY)) {
        if (rightData.classType() != classType()) {
            Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                L"another class type");
        }
        if (rightData.fieldNames().size() > dp->fieldNames().size()) {
            promoteType(NLS_CLASS_ARRAY, rightData.fieldNames());
        } else {
            rightData.promoteType(NLS_CLASS_ARRAY, dp->fieldNames());
        }
    } else if (!isEmpty() && (rightData.dataClass() == NLS_CLASSDEF_ARRAY)
        && (dataClass() == NLS_CLASSDEF_ARRAY)) {
        if (rightData.classType() != classType()) {
            Error(L"Nelson:types:conversionNotSupported", L"Cannot promote to %s array.",
                L"another class type");
        }
        if (rightData.fieldNames().size() > dp->fieldNames().size()) {
            promoteType(NLS_CLASSDEF_ARRAY, rightData.fieldNames());
        } else {
            rightData.promoteType(NLS_CLASSDEF_ARRAY, dp->fieldNames());
        }
    } else if (!isEmpty() && (rightData.dataClass() == NLS_FUNCTION_HANDLE)
        && (dataClass() == NLS_FUNCTION_HANDLE)) {
        if (rightData.fieldNames().size() > dp->fieldNames().size()) {
            promoteType(NLS_FUNCTION_HANDLE, rightData.fieldNames());
        } else {
            rightData.promoteType(NLS_FUNCTION_HANDLE, dp->fieldNames());
        }
    } else {
        if (isEmpty() || rightData.dataClass() > dataClass()) {
            // An empty double destination adopts the class of the value, except
            // single which stays double.
            if (isDoubleClass() && rightData.isSingleClass()) {
                rightData.promoteType(rightData.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE);
            } else {
                promoteType(rightData.dataClass(), rightData.fieldNames());
            }
        } else {
            if (rightData.dataClass() <= dp->dataClass) {
                rightData.promoteType(dp->dataClass, dp->fieldNames());
            }
        }
    }
    if (rightData.isClassType() && isClassType() && rightData.dataClass() == dataClass()) {
        setClassType(rightData.classType());
        if (rightData.isClassdefType()) {
            setClassdefLayout(rightData.classdefLayout());
        }
    }
    // A max index larger than the current length resizes the array, which is only legal
    // for a vector.
    if (isSparse()) {
        if (rightData.isSparse()) {
            rightData.makeDense();
        }
        indexType rows = dimensionLength(0);
        indexType cols = dimensionLength(1);
        void* sparseData = sparseReadWriteDataPointer();
        void* qp = Eigen_SetSparseVectorSubsets(dp->dataClass, rows, cols, sparseData, index_p,
            index.dimensionLength(0), index.dimensionLength(1), rightData.dataPointer(), advance);
        Dimensions newdim;
        newdim[0] = rows;
        newdim[1] = cols;
        dp = dp->putData(dp->dataClass, newdim, qp, true);
        dropAllZeroImaginarySparse(*this);
        return;
    }
    if (max_index <= elementCount()) {
        void* qp = readWriteDataPointer();
        if (dp->dataClass == NLS_CELL_ARRAY && isCellScalarAssignmentValue(rightData)) {
            ArrayOf scalarValue = cellScalarAssignmentElement(rightData);
            auto* elements = static_cast<ArrayOf*>(qp);
            for (indexType i = 0; i < index_length; i++) {
                elements[index_p[i] - 1] = scalarValue;
            }
            return;
        }
        if (isStringScalarAssignment) {
            ArrayOf scalarValue = stringScalarAssignmentElement(rightData);
            auto* elements = static_cast<ArrayOf*>(qp);
            for (indexType i = 0; i < index_length; i++) {
                elements[index_p[i] - 1] = scalarValue;
            }
            return;
        }
    }
    vectorResize(max_index);
    void* qp = readWriteDataPointer();
    if (dp->dataClass == NLS_CELL_ARRAY && isCellScalarAssignmentValue(rightData)) {
        ArrayOf scalarValue = cellScalarAssignmentElement(rightData);
        auto* elements = static_cast<ArrayOf*>(qp);
        for (indexType i = 0; i < index_length; i++) {
            elements[index_p[i] - 1] = scalarValue;
        }
        return;
    }
    if (isStringScalarAssignment) {
        ArrayOf scalarValue = stringScalarAssignmentElement(rightData);
        auto* elements = static_cast<ArrayOf*>(qp);
        for (indexType i = 0; i < index_length; i++) {
            elements[index_p[i] - 1] = scalarValue;
        }
        return;
    }
    if (advance == 1 && isAscendingUnitStrideIndex(index_p, index_length)) {
        rightData.copyElements(0, qp, index_p[0] - 1, index_length);
        if (dataClass() == NLS_GO_HANDLE) {
            refreshGraphicsObjectClassName();
        }
        return;
    }
    if (trySetDenseVectorSubsetDirect(*this, rightData, index_p, index_length, advance)) {
        return;
    }
    indexType srcIndex = 0;
    indexType j;
    for (int i = 0; i < index_length; i++) {
        j = index_p[i] - 1;
        rightData.copyElements(srcIndex, qp, j, 1);
        srcIndex += advance;
    }
    if (dataClass() == NLS_GO_HANDLE) {
        refreshGraphicsObjectClassName();
    }
}
//=============================================================================
void
ArrayOf::setValue(const ArrayOf& value)
{
    if (this == &value) {
        return;
    }
    std::string currentName = _name;
    deleteContents();
    copyObject(value);
    _name = currentName;
}
//=============================================================================
void
ArrayOf::setValueAtIndex(uint64 index, const ArrayOf& scalarValue)
{
    materializeScalar();
    if (!scalarValue.isScalar()) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    auto length = static_cast<uint64>(this->elementCount());
    if (index >= length) {
        Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
    }
    if (isSparse()) {
        indexType rows = dimensionLength(0);
        indexType cols = dimensionLength(1);
        auto indx = static_cast<indexType>(index);
        void* sparseData = sparseReadWriteDataPointer();
        void* qp = Eigen_SetSparseVectorSubsets(
            dp->dataClass, rows, cols, sparseData, &indx, 1, 1, scalarValue.dataPointer(), 0);
        Dimensions newdim;
        newdim[0] = rows;
        newdim[1] = cols;
        dp = dp->putData(dp->dataClass, newdim, qp, true);
        dropAllZeroImaginarySparse(*this);
    } else {
        if (dataClass() == NLS_HANDLE) {
            auto* ptr = static_cast<HandleRef*>(readWriteDataPointer());
            const auto* val = static_cast<const HandleRef*>(scalarValue.dataPointer());
            ptr[index] = val[0];
        } else if (dataClass() == NLS_STRING_ARRAY || dataClass() == NLS_CELL_ARRAY) {
            // String and cell arrays store nested ArrayOf elements. A raw memcpy would copy the
            // reference without copy semantics and corrupt the reference counting.
            auto* ptr = static_cast<ArrayOf*>(readWriteDataPointer());
            const auto* val = static_cast<const ArrayOf*>(scalarValue.dataPointer());
            ptr[index] = val[0];
        } else if (dataClass() == NLS_STRUCT_ARRAY || dataClass() == NLS_CLASS_ARRAY
            || dataClass() == NLS_CLASSDEF_ARRAY || dataClass() == NLS_FUNCTION_HANDLE) {
            // Structure, class and function-handle elements each hold fieldCount nested ArrayOf
            // slots. A raw memcpy would copy those references without owner accounting: destroying
            // the source would free the shared storage and leave this element dangling.
            // Assign each field slot with ArrayOf semantics instead.
            const indexType dstFieldCount = static_cast<indexType>(dp->fieldNames().size());
            const indexType srcFieldCount
                = scalarValue.dp ? static_cast<indexType>(scalarValue.dp->fieldNames().size()) : 0;
            const indexType fieldCount = std::min(dstFieldCount, srcFieldCount);
            auto* ptr = static_cast<ArrayOf*>(readWriteDataPointer());
            const auto* val = static_cast<const ArrayOf*>(scalarValue.dataPointer());
            for (indexType j = 0; j < fieldCount; ++j) {
                ptr[index * dstFieldCount + j] = val[j];
            }
        } else {
            indexType elSize(elementSize());
            char* ptr = static_cast<char*>(readWriteDataPointer());
            const char* val = static_cast<const char*>(scalarValue.dataPointer());
            memcpy(ptr + index * elSize, val, scalarValue.byteSize());
        }
        if (dataClass() == NLS_GO_HANDLE) {
            refreshGraphicsObjectClassName();
        }
    }
}
//=============================================================================
static NelsonType
ordinalIndexClassForAssignView()
{
#ifdef NLS_INDEX_TYPE_64
    return NLS_UINT64;
#else
    return NLS_UINT32;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
