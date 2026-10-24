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
#include <type_traits>
#include <vector>
#include "ValidatorsInternal.hpp"
#include "ValidatorsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct SortedCheck
    {
        bool ascending;
        bool descending;
        bool strict;
        bool ascendingMissingLast;
        bool descendingMissingLast;
    };
    //=============================================================================
    // Element of a string array or cell array of character vectors.
    // nullptr is a missing string: every comparison with it is false, like NaN.
    struct TextKey
    {
        const std::wstring* text;
        bool
        operator<(const TextKey& other) const
        {
            return text && other.text && (*text < *other.text);
        }
        bool
        operator<=(const TextKey& other) const
        {
            return text && other.text && (*text <= *other.text);
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
static bool
nativeIsSorted(const ArrayOf& arg, indexType dim, const SortedCheck& check, bool& sorted);
static bool
textIsSorted(const ArrayOf& arg, indexType count, indexType stride, indexType n, indexType outer,
    const SortedCheck& check, bool& sorted);
template <class T>
static bool
isSortedAlong(const T* x, indexType stride, indexType n, indexType outer, const SortedCheck& check);
//=============================================================================
void
mustBeSorted(const ArrayOf& arg, indexType dim, const std::wstring& direction,
    const std::wstring& missingPlacement, const std::wstring& comparisonMethod, int argPosition,
    bool asCaller)
{
    SortedCheck check;
    check.strict = direction.rfind(L"strict", 0) == 0;
    std::wstring order = check.strict ? direction.substr(6) : direction;
    check.ascending = (order != L"descend");
    check.descending = (order != L"ascend");
    check.ascendingMissingLast = (missingPlacement != L"first");
    check.descendingMissingLast = (missingPlacement == L"last");
    bool sorted = false;
    if (comparisonMethod == L"abs" || !nativeIsSorted(arg, dim, check, sorted)) {
        ArrayOfVector args;
        args << arg;
        args << ArrayOf::doubleScalar(static_cast<double>(dim));
        args << ArrayOf::characterArray(direction);
        args << ArrayOf::characterArray(missingPlacement);
        args << ArrayOf::characterArray(comparisonMethod);
        sorted = evaluateFunction(args, 1, "__mustBeSorted__")[0].asLogicalScalar();
    }
    if (!sorted) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeSorted",
                                    L"%s must be in sorted order."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeSorted", asCaller);
    }
}
//=============================================================================
// Single pass over real dense numeric, logical and char arrays, string arrays
// and cell arrays of character vectors.
// Returns false when the type is not handled here.
bool
nativeIsSorted(const ArrayOf& arg, indexType dim, const SortedCheck& check, bool& sorted)
{
    if (arg.isSparse() || arg.isComplex()) {
        return false;
    }
    Dimensions dims = arg.dimensions();
    if (dim == 0) {
        dim = 1;
        for (indexType k = 0; k < dims.length(); ++k) {
            if (dims.at(k) != 1) {
                dim = k + 1;
                break;
            }
        }
    }
    indexType n = dims.dimensionLength(dim - 1);
    indexType count = dims.elementCount();
    indexType stride = 1;
    for (indexType k = 0; k + 1 < dim; ++k) {
        stride *= dims.dimensionLength(k);
    }
    indexType outer = (count == 0 || n == 0) ? 0 : count / (stride * n);
    const void* data = arg.dataPointer();
    switch (arg.dataClass()) {
    case NLS_DOUBLE:
        sorted = isSortedAlong(static_cast<const double*>(data), stride, n, outer, check);
        return true;
    case NLS_SINGLE:
        sorted = isSortedAlong(static_cast<const single*>(data), stride, n, outer, check);
        return true;
    case NLS_INT8:
        sorted = isSortedAlong(static_cast<const int8*>(data), stride, n, outer, check);
        return true;
    case NLS_UINT8:
    case NLS_LOGICAL:
        sorted = isSortedAlong(static_cast<const uint8*>(data), stride, n, outer, check);
        return true;
    case NLS_INT16:
        sorted = isSortedAlong(static_cast<const int16*>(data), stride, n, outer, check);
        return true;
    case NLS_UINT16:
        sorted = isSortedAlong(static_cast<const uint16*>(data), stride, n, outer, check);
        return true;
    case NLS_INT32:
        sorted = isSortedAlong(static_cast<const int32*>(data), stride, n, outer, check);
        return true;
    case NLS_UINT32:
        sorted = isSortedAlong(static_cast<const uint32*>(data), stride, n, outer, check);
        return true;
    case NLS_INT64:
        sorted = isSortedAlong(static_cast<const int64*>(data), stride, n, outer, check);
        return true;
    case NLS_UINT64:
        sorted = isSortedAlong(static_cast<const uint64*>(data), stride, n, outer, check);
        return true;
    case NLS_CHAR:
        sorted = isSortedAlong(static_cast<const charType*>(data), stride, n, outer, check);
        return true;
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
        return textIsSorted(arg, count, stride, n, outer, check, sorted);
    default:
        return false;
    }
}
//=============================================================================
// Returns false for a cell array that is not a cell array of character vectors.
bool
textIsSorted(const ArrayOf& arg, indexType count, indexType stride, indexType n, indexType outer,
    const SortedCheck& check, bool& sorted)
{
    const auto* elements = static_cast<const ArrayOf*>(arg.dataPointer());
    bool isCell = (arg.dataClass() == NLS_CELL_ARRAY);
    std::vector<std::wstring> texts(count);
    std::vector<TextKey> keys(count);
    for (indexType k = 0; k < count; ++k) {
        const ArrayOf& element = elements[k];
        bool isText = element.isRowVectorCharacterArray()
            || (element.isCharacterArray() && element.isEmpty());
        if (isText) {
            texts[k] = element.asWideString();
            keys[k].text = &texts[k];
        } else if (isCell) {
            return false;
        } else {
            keys[k].text = nullptr;
        }
    }
    sorted = isSortedAlong(keys.data(), stride, n, outer, check);
    return true;
}
//=============================================================================
template <class T>
static inline bool
isMissingValue(T value)
{
    if constexpr (std::is_floating_point_v<T>) {
        return std::isnan(value);
    } else if constexpr (std::is_same_v<T, TextKey>) {
        return value.text == nullptr;
    } else {
        return false;
    }
}
//=============================================================================
// Strict orders reject missing values: every comparison with NaN is false.
template <class T>
static inline bool
isPairInOrder(T a, T b, bool ascending, bool strict, bool missingLast)
{
    if (strict) {
        return ascending ? (a < b) : (b < a);
    }
    if (ascending ? (a <= b) : (b <= a)) {
        return true;
    }
    // not ordered: only a missing value placed on the expected side is accepted
    return missingLast ? isMissingValue(b) : isMissingValue(a);
}
//=============================================================================
// Column-major walk: for each slice along dim, compare the consecutive slices
// element-wise so that memory is read contiguously whatever dim is.
template <class T>
bool
isSortedAlong(const T* x, indexType stride, indexType n, indexType outer, const SortedCheck& check)
{
    bool ascending = check.ascending;
    bool descending = check.descending;
    for (indexType o = 0; o < outer; ++o) {
        const T* block = x + o * stride * n;
        for (indexType k = 1; k < n; ++k) {
            const T* previous = block + (k - 1) * stride;
            const T* next = previous + stride;
            for (indexType i = 0; i < stride; ++i) {
                if (ascending
                    && !isPairInOrder(
                        previous[i], next[i], true, check.strict, check.ascendingMissingLast)) {
                    ascending = false;
                }
                if (descending
                    && !isPairInOrder(
                        previous[i], next[i], false, check.strict, check.descendingMissingLast)) {
                    descending = false;
                }
            }
            if (!ascending && !descending) {
                return false;
            }
        }
    }
    return ascending || descending;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
