//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include <memory>
#include "ArrayOf.hpp"
#include "ArrayOfIndexPlan.hpp"
#include "ArrayOfView.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
#include "MutableArrayOfView.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
ordinalIndexClassForCellView();
//=============================================================================
static bool
trySetVectorContentsDoubleScalarIndex(ArrayOf& destination, ArrayOf& index, ArrayOf& data);
//=============================================================================
static void
checkContentsAssignmentTarget(const ArrayOf& destination);
//=============================================================================
static void
checkStringContentsValue(const ArrayOf& value);
//=============================================================================
bool
ArrayOf::isCell() const
{
    return (this->dataClass() == NLS_CELL_ARRAY);
}
//=============================================================================
static ArrayOf
cellArrayFromArrayOfCharacterVectors(const stringVector& vectorStr, bool bAsColumn)
{
    const size_t nbElements = vectorStr.size();
    if (nbElements == 0) {
        Dimensions dims = bAsColumn ? Dimensions(0, 1) : Dimensions(1, 0);
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }

    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements = std::make_unique<ArrayOf[]>(nbElements);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    for (size_t k = 0; k < nbElements; ++k) {
        elements[k] = ArrayOf::characterArray(vectorStr[k]);
    }

    Dimensions dims = bAsColumn ? Dimensions(nbElements, 1) : Dimensions(1, nbElements);
    try {
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release(); // Only release ownership after successful construction
        return result;
    } catch (...) {
        // If construction fails, elements will be deleted by unique_ptr
        throw;
    }
}
//=============================================================================
static ArrayOf
cellArrayFromArrayOfCharacterVectors(const wstringVector& vectorStr, bool bAsColumn)
{
    const size_t nbElements = vectorStr.size();
    if (nbElements == 0) {
        Dimensions dims = bAsColumn ? Dimensions(0, 1) : Dimensions(1, 0);
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }

    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements = std::make_unique<ArrayOf[]>(nbElements);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    for (size_t k = 0; k < nbElements; ++k) {
        elements[k] = ArrayOf::characterArray(vectorStr[k]);
    }

    Dimensions dims = bAsColumn ? Dimensions(nbElements, 1) : Dimensions(1, nbElements);
    try {
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release(); // Only release ownership after successful construction
        return result;
    } catch (...) {
        // If construction fails, elements will be deleted by unique_ptr
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::cellArrayFromArrayOfCharacterRowVectors(const stringVector& elements)
{
    return cellArrayFromArrayOfCharacterVectors(elements, false);
}
//=============================================================================
ArrayOf
ArrayOf::cellArrayFromArrayOfCharacterRowVectors(const wstringVector& elements)
{
    return cellArrayFromArrayOfCharacterVectors(elements, false);
}
//=============================================================================
ArrayOf
ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(const stringVector& elements)
{
    return cellArrayFromArrayOfCharacterVectors(elements, true);
}
//=============================================================================
ArrayOf
ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(const wstringVector& elements)
{
    return cellArrayFromArrayOfCharacterVectors(elements, true);
}
//=============================================================================
bool
ArrayOf::isCellArrayOfCharacterVectors() const
{
    if (!isCell()) {
        return false;
    }
    if (isEmpty()) {
        return true;
    }
    ArrayOf* arg = (ArrayOf*)(dataPointer());
    indexType nbElements = elementCount();
    for (indexType k = 0; k < nbElements; k++) {
        if (!arg[k].isCharacterArray()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
ArrayOf
ArrayOf::cellArrayFrom(ArrayOf m)
{
    if (m.isCell()) {
        return m;
    }
    ArrayOf* elements
        = static_cast<ArrayOf*>(allocateArrayOf(NLS_CELL_ARRAY, 1, stringVector(), false));
    elements[0] = m;
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
}
//=============================================================================
ArrayOf
ArrayOf::cellArray(ArrayOfMatrix& m)
{
    indexType columnCount = 0, rowCount = 0;
    ArrayOf* qp = nullptr;
    try {
        auto i = m.begin();
        while (i != m.end()) {
            ArrayOfVector ptr = *i;
            if (i == m.begin()) {
                columnCount = ptr.size();
            } else {
                if (ptr.size() != columnCount) {
                    Error(L"Nelson:dimagree:cellDefinitionNumberElementsEachRow",
                        L"Cell definition must have same number of elements in each row");
                }
            }
            ++i;
        }
        rowCount = m.size();
        Dimensions retDims(2);
        retDims[0] = rowCount;
        retDims[1] = columnCount;
        qp = static_cast<ArrayOf*>(
            allocateArrayOf(NLS_CELL_ARRAY, retDims.elementCount(), stringVector(), false));
        ArrayOf* sp;
        // A cell array with rows but zero columns has no storage (qp is null), and
        // incrementing a null pointer is undefined behaviour.
        if (qp != nullptr) {
            sp = qp;
            i = m.begin();
            while (i != m.end()) {
                ArrayOfVector ptr = *i;
                ArrayOf* cp = sp;
                for (const auto& j : ptr) {
                    *cp = j;
                    cp += rowCount;
                }
                ++i;
                sp++;
            }
        }
        return ArrayOf(NLS_CELL_ARRAY, retDims, qp);
    } catch (const NelsonException&) {
        auto* rp = qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
}
//=============================================================================
/**
 * Return the contents of a cell array - must be a scalar...
 */
ArrayOf
ArrayOf::vectorContents(ArrayOf& indexing)
{
    if (!this->isCell()) {
        Error(L"Nelson:types:cellContentsExpected:attemptApplyContentsIndexingNonCellX",
            L"Attempt to apply contents-indexing to non cell-array object.");
    }
    if (indexing.isEmpty()) {
        Error(L"Nelson:types:invalidIndex:emptyContentsIndexingNotDefined",
            L"Empty contents indexing is not defined.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"vectorContents");
    }
    indexing.toOrdinalType();
    // The output has the size of the index, not of the source, and the type of the source.
    // The index must be a scalar for contents-based addressing.
    if (indexing.elementCount() != 1) {
        Error(L"Nelson:types:invalidIndex:contentIndexingReturnASingleValue",
            L"Content indexing must return a single value.");
    }
    ArrayOfView<indexType> indexView(indexing, ordinalIndexClassForCellView());
    auto index_p = indexView.data();
    if (*index_p == 0) {
        Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
    } else {
        indexType ndx = *index_p - 1;
        indexType bound = elementCount();
        if (ndx >= bound) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        const auto* srcPart = static_cast<const ArrayOf*>(dp->data());
        return srcPart[ndx];
    }
    return {}; // never here
}
//=============================================================================
/**
 * Return the contents of a cell array - indexed via a multi-dim index.
 */
ArrayOf
ArrayOf::nDimContents(ArrayOfVector& indexing)
{
    if (!this->isCell()) {
        Error(L"Nelson:types:cellContentsExpected:attemptApplyContentsIndexingNonCellX",
            L"Attempt to apply contents-indexing to non cell-array object.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"nDimContents");
    }
    indexType L = indexing.size();
    Dimensions outPos(L);
    // Contents-based addressing needs scalar indices. Conversion errors propagate.
    for (indexType i = 0; i < L; i++) {
        indexing[i].toOrdinalType();
        if (indexing[i].elementCount() != 1) {
            Error(L"Nelson:types:invalidIndex:contentIndexingReturnASingleValue",
                L"Content indexing must return a single value.");
        }
        ArrayOfView<indexType> indexView(indexing[i], ordinalIndexClassForCellView());
        auto sp = indexView.data();
        outPos[i] = *sp - 1;
    }
    // Fewer subscripts than dimensions fold the trailing dimensions into the
    // last subscript (column-major rule).
    Dimensions effectiveDims = dp->dimensions_.foldForSubscriptCount(L);
    indexType j = effectiveDims.mapPoint(outPos);
    const auto* qp = static_cast<const ArrayOf*>(dp->data());
    return qp[j];
}
//=============================================================================
/**
 * Return a subset of a cell array as a list.
 */
ArrayOfVector
ArrayOf::vectorContentsAsList(ArrayOf& index)
{
    ArrayOfVector m;
    if (!this->isCell() && !this->isStringArray()) {
        Error(L"Nelson:types:cellContentsExpected:attemptApplyContentsIndexingNonCellX",
            L"Attempt to apply contents-indexing to non cell-array object.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"vectorContentsAsList");
    }
    if (index.isEmpty()) {
        return {};
    }
    if (isEmpty()) {
        return {};
    }
    if (index.isRowVectorCharacterArray()) {
        std::wstring str = index.asWideString();
        if (str != L":") {
            Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
                L"index must either be real positive integers or logicals.");
        }
        index = ArrayOf::integerRange(1, 1, dp->elementCount(), true);
    }
    index.toOrdinalType();
    indexType max_index = index.maxAsIndex();
    indexType bound = elementCount();
    if (max_index > bound) {
        Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
    }
    indexType index_length = index.elementCount();
    m.reserve(static_cast<size_t>(index_length));
    ArrayOfView<indexType> indexView(index, ordinalIndexClassForCellView());
    auto index_p = indexView.data();
    const auto* qp = static_cast<const ArrayOf*>(dp->data());
    for (indexType i = 0; i < index_length; i++) {
        m << qp[index_p[i] - 1];
    }
    return m;
}
//=============================================================================
/**
 * Return the contents of an cell array as a list.
 */
ArrayOfVector
ArrayOf::nDimContentsAsList(ArrayOfVector& index)
{
    bool isStringArray = this->isStringArray();
    if (!this->isCell() && !isStringArray) {
        Error(L"Nelson:types:cellContentsExpected:attemptApplyContentsIndexingNonCell",
            L"Attempt to apply contents-indexing to non cell or string array object.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"nDimContentsAsList");
    }
    ArrayOfVector m;
    indexType L = index.size();
    // Fewer subscripts than dimensions fold the trailing dimensions into the
    // last subscript (column-major rule), for both colon expansion and the
    // linear addressing below.
    Dimensions sourceDims = dp->dimensions_.foldForSubscriptCount(L);
    IndexPlan plan(false, sourceDims, index, false);
    constIndexPtr* indx = plan.data();
    Dimensions outDims = plan.outDims();
    Dimensions argPointer(L);
    Dimensions currentIndex(L);
    const auto* qp = static_cast<const ArrayOf*>(dp->data());
    indexType srcindex = 0;
    m.reserve(static_cast<size_t>(outDims.elementCount()));
    while (argPointer.inside(outDims)) {
        for (indexType i = 0; i < L; i++) {
            currentIndex[i] = indx[i][argPointer[i]] - 1;
        }
        srcindex = sourceDims.mapPoint(currentIndex);
        if (isStringArray) {
            if (!qp[srcindex].isCharacterArray()) {
                Error(
                    L"Nelson:types:conversionNotSupported:conversionFromMissingCharacterVectorNot",
                    L"Conversion from <missing> to character vector is not supported.");
            }
        }
        m << qp[srcindex];
        argPointer.incrementModulo(outDims, 0);
    }
    return m;
}
//=============================================================================
/**
 * Vector version of the cell replacement function for content-based indexing (curly
 * brackets). Unlike replaceData, an index larger than the size resizes the array to a
 * vector of sufficient length, and deletions do not occur.
 */
void
ArrayOf::setVectorContents(ArrayOf& index, ArrayOf& data)
{
    if (dataClass() == NLS_STRING_ARRAY) {
        // S{I} = B keeps a string array, empty or not.
        ArrayOfVector values;
        values.push_back(data);
        setVectorContentsAsList(index, values);
        return;
    }
    checkContentsAssignmentTarget(*this);
    promoteType(NLS_CELL_ARRAY, data.fieldNames());
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"setVectorContents");
    }
    if (trySetVectorContentsDoubleScalarIndex(*this, index, data)) {
        return;
    }
    index.toOrdinalType();
    if (index.elementCount() == 0) {
        return;
    }
    if (index.elementCount() != 1) {
        Error(L"Nelson:types:invalidIndex:expressionAIBIReference",
            L"In expression A{I} = B, I must reference a single element of cell-array A.");
    }
    ArrayOfView<indexType> indexView(index, ordinalIndexClassForCellView());
    auto index_p = indexView.data();
    if (*index_p == 0) {
        Error(L"Nelson:types:invalidIndex:illegalNegativeIndexExpressionAI",
            L"Illegal negative index in expression A{I} = B.");
    }
    indexType ndx = *index_p - 1;
    if (dataClass() == NLS_CELL_ARRAY && ndx < elementCount()) {
        MutableArrayOfView<ArrayOf> elements(*this, NLS_CELL_ARRAY);
        elements[ndx] = data;
        return;
    }
    vectorResize(ndx + 1);
    MutableArrayOfView<ArrayOf> elements(*this, NLS_CELL_ARRAY);
    elements[ndx] = data;
    dp->dimensions_.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * This is the multidimensional cell-replacement function.
 * This is for content-based indexing (curly brackets).
 */
void
ArrayOf::setNDimContents(ArrayOfVector& index, ArrayOf& data)
{
    if (dataClass() == NLS_STRING_ARRAY) {
        ArrayOfVector values;
        values.push_back(data);
        setNDimContentsAsList(index, values);
        return;
    }
    checkContentsAssignmentTarget(*this);
    promoteType(NLS_CELL_ARRAY, data.fieldNames());
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"setNDimContents");
    }
    indexType L = index.size();
    Dimensions outPos(L);
    indexType i;
    for (i = 0; i < L; i++) {
        index[i].toOrdinalType();
        if (!index[i].isScalar()) {
            Error(L"Nelson:types:cellIndexSingleElement",
                L"In expression A{I1,I2,...,IN} = B, (I1,...,IN) must reference a single element "
                L"of cell-array A.");
        }
        ArrayOfView<indexType> indexView(index[i], ordinalIndexClassForCellView());
        auto sp = indexView.data();
        outPos[i] = *sp;
    }
    // Fewer subscripts than dimensions fold the trailing dimensions into the
    // last subscript: the folded extent is fixed (cannot grow), while the
    // leading subscripts may grow their dimension, and the array keeps its rank.
    bool foldTrailing = !isEmpty() && (L >= 2) && (L < dp->dimensions_.length());
    Dimensions effectiveDims;
    if (foldTrailing) {
        effectiveDims = dp->dimensions_.foldForSubscriptCount(L);
        if (outPos[L - 1] > effectiveDims[L - 1]) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        bool needGrow = false;
        Dimensions growTarget(dp->dimensions_);
        for (i = 0; i < L - 1; i++) {
            if (outPos[i] > growTarget.dimensionLength(i)) {
                growTarget[i] = outPos[i];
                needGrow = true;
            }
        }
        if (needGrow) {
            resize(growTarget);
            effectiveDims = dp->dimensions_.foldForSubscriptCount(L);
        }
    } else {
        resize(outPos);
        effectiveDims = dp->dimensions_;
    }
    for (i = 0; i < L; i++) {
        outPos[i] = outPos[i] - 1;
    }
    indexType j = effectiveDims.mapPoint(outPos);
    MutableArrayOfView<ArrayOf> elements(*this, NLS_CELL_ARRAY);
    elements[j] = data;
    dp->dimensions_.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * Vector version of the cell replacement function for content-based indexing (curly
 * brackets). Unlike replaceData, an index larger than the size resizes the array to a
 * vector of sufficient length, and deletions do not occur.
 */
void
ArrayOf::setVectorContentsAsList(ArrayOf& index, ArrayOfVector& data)
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"setVectorContentsAsList");
    }
    bool asStringArray = (dataClass() == NLS_STRING_ARRAY);
    if (asStringArray) {
        promoteType(NLS_STRING_ARRAY);
    } else {
        checkContentsAssignmentTarget(*this);
        promoteType(NLS_CELL_ARRAY);
    }
    index.toOrdinalType();
    if (static_cast<indexType>(data.size()) < index.elementCount()) {
        Error(L"Nelson:types:notEnoughRhsValues",
            L"Not enough right hand side values to satisfy left hand side expression.");
    }
    if (index.elementCount() == 0) {
        return;
    }
    indexType max_index = index.maxAsIndex();
    vectorResize(max_index);
    MutableArrayOfView<ArrayOf> dataView(*this, asStringArray ? NLS_STRING_ARRAY : NLS_CELL_ARRAY);
    auto* qp = dataView.data();
    ArrayOfView<indexType> indexView(index, ordinalIndexClassForCellView());
    auto index_p = indexView.data();
    indexType index_length = index.elementCount();
    ArrayOf front = data.front();
    for (indexType i = 0; i < index_length; i++) {
        indexType ndx = index_p[i] - 1;
        if (asStringArray) {
            checkStringContentsValue(front);
        }
        qp[ndx] = front;
        data.pop_front();
        if (data.size() > 0) {
            front = data.front();
        }
    }
    dp->dimensions_.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * This is the multidimensional cell-replacement function.
 * This is for content-based indexing (curly brackets).
 */
void
ArrayOf::setNDimContentsAsList(ArrayOfVector& index, ArrayOfVector& data)
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"setNDimContentsAsList");
    }
    bool asStringArray = (dataClass() == NLS_STRING_ARRAY);
    if (asStringArray) {
        promoteType(NLS_STRING_ARRAY);
    } else {
        checkContentsAssignmentTarget(*this);
        promoteType(NLS_CELL_ARRAY);
    }
    indexType L = index.size();
    try {
        Dimensions myDims = dp->dimensions_;
        // Fewer subscripts than dimensions fold the trailing dimensions into the last subscript:
        // the folded extent is fixed, while the leading subscripts may grow their dimension and
        // the array keeps its rank.
        bool foldTrailing = !isEmpty() && (L >= 2) && (L < myDims.length());
        Dimensions sourceDims = foldTrailing ? myDims.foldForSubscriptCount(L) : myDims;
        IndexPlan plan(false, sourceDims, index, false);
        constIndexPtr* indx = plan.data();
        Dimensions a(L);
        Dimensions argLengths(L);
        Dimensions argPointer(L);
        indexType dataCount = 1;
        for (indexType i = 0; i < L; i++) {
            argLengths[i] = index[i].elementCount();
            dataCount *= argLengths[i];
        }
        if (static_cast<int>(data.size()) < dataCount) {
            Error(L"Nelson:types:notEnoughRhsValues",
                L"Not enough right hand side values to satisfy left hand side expression.");
        }
        if (dataCount == 0) {
            return;
        }
        for (indexType i = 0; i < L; i++) {
            a[i] = index[i].maxAsIndex();
        }
        if (foldTrailing) {
            if (a[L - 1] > sourceDims[L - 1]) {
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
                sourceDims = dp->dimensions_.foldForSubscriptCount(L);
            }
        } else {
            resize(a);
            sourceDims = dp->dimensions_;
        }
        MutableArrayOfView<ArrayOf> dataView(
            *this, asStringArray ? NLS_STRING_ARRAY : NLS_CELL_ARRAY);
        auto* qp = dataView.data();
        Dimensions currentIndex(sourceDims.length());
        indexType j;
        while (argPointer.inside(argLengths)) {
            ArrayOf front = data.front();
            for (indexType i = 0; i < L; i++) {
                currentIndex[i] = indx[i][argPointer[i]] - 1;
            }
            j = sourceDims.mapPoint(currentIndex);
            if (asStringArray) {
                checkStringContentsValue(front);
            }
            qp[j] = front;
            data.pop_front();
            argPointer.incrementModulo(argLengths, 0);
        }
        dp->dimensions_.simplify();
        dp->refreshDimensionCache();
    } catch (const NelsonException&) {
        throw;
    }
}
//=============================================================================
static NelsonType
ordinalIndexClassForCellView()
{
#ifdef NLS_INDEX_TYPE_64
    return NLS_UINT64;
#else
    return NLS_UINT32;
#endif
}
//=============================================================================
static bool
trySetVectorContentsDoubleScalarIndex(ArrayOf& destination, ArrayOf& index, ArrayOf& data)
{
    if (destination.dataClass() != NLS_CELL_ARRAY || !index.isDoubleType(true) || index.isSparse()
        || index.elementCount() != 1) {
        return false;
    }
    const double scalarIndex = static_cast<const double*>(index.dataPointer())[0];
    if (!std::isfinite(scalarIndex)
        || scalarIndex > static_cast<double>(std::numeric_limits<indexType>::max())) {
        Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
            L"index must either be real positive integers or logicals.");
    }
    const auto ordinal = static_cast<indexType>(scalarIndex);
    if (scalarIndex != static_cast<double>(ordinal)) {
        Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
            L"index must either be real positive integers or logicals.");
    }
    if (ordinal <= 0) {
        Error(L"Nelson:types:invalidIndex:zeroNegativeIndexEncountered",
            L"Zero or negative index encountered.");
    }
    const indexType targetIndex = ordinal - 1;
    if (targetIndex >= destination.elementCount()) {
        destination.vectorResize(ordinal);
    }
    MutableArrayOfView<ArrayOf> elements(destination, NLS_CELL_ARRAY);
    elements[targetIndex] = data;
    return true;
}
//=============================================================================
// A{I} = B only applies to a cell (or string) array; an empty value of any
// other type becomes a cell array.
static void
checkContentsAssignmentTarget(const ArrayOf& destination)
{
    NelsonType destinationClass = destination.dataClass();
    if (destinationClass == NLS_CELL_ARRAY || destinationClass == NLS_STRING_ARRAY
        || destination.isEmpty()) {
        return;
    }
    Error(L"Nelson:cellAssToNonCell",
        L"Unable to perform assignment because brace indexing is not supported for variables of "
        L"this type.");
}
//=============================================================================
// S{I} = B on a string array accepts a character row vector, a 0x0 char or
// [] (missing); anything else, a string scalar included, is rejected.
static void
checkStringContentsValue(const ArrayOf& value)
{
    if (value.isCharacterArray() && (value.isRowVector() || value.isEmpty(true))) {
        return;
    }
    if (value.isDoubleType(true) && value.isEmpty(true)) {
        return;
    }
    Error(L"Nelson:string:UnsupportedCurlyAsgnValue",
        L"Curly brace assignment into a string expects a character vector.");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
