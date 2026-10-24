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
#include <deque>
#include <limits>
#include <type_traits>
#include <vector>
#include "MovingWindow.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
movingWindowOfClass(const ArrayOf& x, indexType n, indexType cols, indexType before,
    indexType after, indexType firstRow, indexType lastRow, bool omitNaN,
    MovingWindowOperation operation);
template <class T>
static double
reduceWindow(const T* column, indexType low, indexType high, bool omitNaN,
    MovingWindowOperation operation, std::vector<T>& window, std::vector<T>& deviations);
template <class T>
static double
sumWindow(const T* column, indexType low, indexType high, bool omitNaN);
template <class T>
static double
meanWindow(const T* column, indexType low, indexType high, bool omitNaN);
template <class T>
static double
extremumWindow(const T* column, indexType low, indexType high, bool omitNaN, bool minimum);
template <class T>
static void
slidingExtremumColumn(const T* column, indexType n, indexType before, indexType after,
    indexType firstRow, indexType lastRow, bool omitNaN, bool minimum, double* out);
template <class T, class Acc>
static void
slidingSumColumn(const T* column, indexType n, indexType before, indexType after,
    indexType firstRow, indexType lastRow, bool omitNaN, bool mean, double* out);
template <class T>
static double
medianWindow(const T* column, indexType low, indexType high, bool omitNaN, bool deviation,
    std::vector<T>& window, std::vector<T>& deviations);
template <class T>
static T
selectMedian(std::vector<T>& values);
template <class T>
static T
castMedianValue(double value);
template <class T>
static T
saturatedDifference(T a, T b);
template <class T>
static T
saturatedAbs(T value);
template <class T>
static bool
isNaNValue(T value);
//=============================================================================
bool
MovingWindowOperationFromName(const std::wstring& name, MovingWindowOperation& operation)
{
    if (name == L"median") {
        operation = MovingWindowOperation::Median;
    } else if (name == L"mad") {
        operation = MovingWindowOperation::Mad;
    } else if (name == L"sum") {
        operation = MovingWindowOperation::Sum;
    } else if (name == L"mean") {
        operation = MovingWindowOperation::Mean;
    } else if (name == L"min") {
        operation = MovingWindowOperation::Min;
    } else if (name == L"max") {
        operation = MovingWindowOperation::Max;
    } else {
        return false;
    }
    return true;
}
//=============================================================================
ArrayOf
MovingWindow(const ArrayOf& x, indexType before, indexType after, indexType firstRow,
    indexType lastRow, bool omitNaN, MovingWindowOperation operation, bool& needToOverload)
{
    needToOverload = false;
    if (x.isSparse() || x.isComplex() || !x.is2D()) {
        needToOverload = true;
        return {};
    }
    Dimensions dims = x.dimensions();
    indexType n = dims[0];
    indexType cols = dims[1];
    if (firstRow < 1 || lastRow > n) {
        Error("Nelson:data_analysis:rowRangeOutOfBounds", "Row range exceeds the array.");
    }
    switch (x.dataClass()) {
    case NLS_DOUBLE:
        return movingWindowOfClass<double>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_SINGLE:
        return movingWindowOfClass<single>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_INT8:
        return movingWindowOfClass<int8>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_UINT8:
        return movingWindowOfClass<uint8>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_INT16:
        return movingWindowOfClass<int16>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_UINT16:
        return movingWindowOfClass<uint16>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_INT32:
        return movingWindowOfClass<int32>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_UINT32:
        return movingWindowOfClass<uint32>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_INT64:
        return movingWindowOfClass<int64>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    case NLS_UINT64:
        return movingWindowOfClass<uint64>(
            x, n, cols, before, after, firstRow, lastRow, omitNaN, operation);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
movingWindowOfClass(const ArrayOf& x, indexType n, indexType cols, indexType before,
    indexType after, indexType firstRow, indexType lastRow, bool omitNaN,
    MovingWindowOperation operation)
{
    indexType rows = (lastRow >= firstRow) ? (lastRow - firstRow + 1) : 0;
    double* y = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, rows * cols);
    ArrayOf result(NLS_DOUBLE, Dimensions(rows, cols), y);
    if (rows == 0 || cols == 0) {
        return result;
    }
    const T* values = (const T*)x.dataPointer();
    std::vector<T> window;
    std::vector<T> deviations;
    window.reserve((size_t)(before + after + 1));
    deviations.reserve((size_t)(before + after + 1));
    bool isExtremum
        = (operation == MovingWindowOperation::Min || operation == MovingWindowOperation::Max);
    bool minimum = (operation == MovingWindowOperation::Min);
    bool isRunningSum
        = (operation == MovingWindowOperation::Sum || operation == MovingWindowOperation::Mean);
    bool mean = (operation == MovingWindowOperation::Mean);
    for (indexType c = 0; c < cols; ++c) {
        const T* column = values + c * n;
        double* out = y + c * rows;
        if (isExtremum) {
            slidingExtremumColumn<T>(
                column, n, before, after, firstRow, lastRow, omitNaN, minimum, out);
            continue;
        }
        if (isRunningSum) {
            // The sum keeps the accumulator class of sumWindow (single stays single).
            // The mean accumulates in double like meanWindow.
            if (mean) {
                slidingSumColumn<T, double>(
                    column, n, before, after, firstRow, lastRow, omitNaN, true, out);
            } else {
                typedef typename std::conditional<std::is_integral<T>::value, double, T>::type
                    SumAccumulator;
                slidingSumColumn<T, SumAccumulator>(
                    column, n, before, after, firstRow, lastRow, omitNaN, false, out);
            }
            continue;
        }
        for (indexType r = firstRow; r <= lastRow; ++r) {
            indexType low = (r > before) ? (r - before) : 1;
            indexType high = std::min(n, r + after);
            out[r - firstRow]
                = reduceWindow<T>(column, low, high, omitNaN, operation, window, deviations);
        }
    }
    return result;
}
//=============================================================================
template <class T>
static double
reduceWindow(const T* column, indexType low, indexType high, bool omitNaN,
    MovingWindowOperation operation, std::vector<T>& window, std::vector<T>& deviations)
{
    switch (operation) {
    case MovingWindowOperation::Sum:
        return sumWindow<T>(column, low, high, omitNaN);
    case MovingWindowOperation::Mean:
        return meanWindow<T>(column, low, high, omitNaN);
    case MovingWindowOperation::Min:
        return extremumWindow<T>(column, low, high, omitNaN, true);
    case MovingWindowOperation::Max:
        return extremumWindow<T>(column, low, high, omitNaN, false);
    case MovingWindowOperation::Mad:
        return medianWindow<T>(column, low, high, omitNaN, true, window, deviations);
    default:
        return medianWindow<T>(column, low, high, omitNaN, false, window, deviations);
    }
}
//=============================================================================
template <class T>
static double
sumWindow(const T* column, indexType low, indexType high, bool omitNaN)
{
    typedef typename std::conditional<std::is_integral<T>::value, double, T>::type Accumulator;
    Accumulator accumulator = 0;
    for (indexType k = low; k <= high; ++k) {
        T value = column[k - 1];
        if (omitNaN && isNaNValue(value)) {
            continue;
        }
        accumulator += (Accumulator)value;
    }
    return (double)accumulator;
}
//=============================================================================
template <class T>
static double
meanWindow(const T* column, indexType low, indexType high, bool omitNaN)
{
    double accumulator = 0;
    double count = 0;
    for (indexType k = low; k <= high; ++k) {
        T value = column[k - 1];
        if (omitNaN && isNaNValue(value)) {
            continue;
        }
        accumulator += (double)value;
        count += 1;
    }
    // the mean of single values is returned in single precision
    if (std::is_same<T, single>::value) {
        return (double)(single)(accumulator / count);
    }
    return accumulator / count;
}
//=============================================================================
template <class T>
static double
extremumWindow(const T* column, indexType low, indexType high, bool omitNaN, bool minimum)
{
    const double nan = std::numeric_limits<double>::quiet_NaN();
    bool found = false;
    T best = T();
    for (indexType k = low; k <= high; ++k) {
        T value = column[k - 1];
        if (isNaNValue(value)) {
            if (!omitNaN) {
                return nan;
            }
            continue;
        }
        if (!found || (minimum ? (value < best) : (value > best))) {
            best = value;
            found = true;
        }
    }
    return found ? (double)best : nan;
}
//=============================================================================
template <class T>
static void
slidingExtremumColumn(const T* column, indexType n, indexType before, indexType after,
    indexType firstRow, indexType lastRow, bool omitNaN, bool minimum, double* out)
{
    // O(n): a deque of candidate indices with monotonic values, front is the
    // current extremum. Both window ends only advance, so each index enters and
    // leaves the deque once.
    const double nan = std::numeric_limits<double>::quiet_NaN();
    std::deque<indexType> dq;
    indexType added = (firstRow > before) ? (firstRow - before - 1) : 0;
    indexType lastNaN = 0; // largest NaN index seen so far (0 means none)
    for (indexType r = firstRow; r <= lastRow; ++r) {
        indexType low = (r > before) ? (r - before) : 1;
        indexType high = std::min(n, r + after);
        while (added < high) {
            ++added;
            T value = column[added - 1];
            if (isNaNValue(value)) {
                lastNaN = added;
                continue;
            }
            while (!dq.empty()) {
                T back = column[dq.back() - 1];
                if (minimum ? (back >= value) : (back <= value)) {
                    dq.pop_back();
                } else {
                    break;
                }
            }
            dq.push_back(added);
        }
        while (!dq.empty() && dq.front() < low) {
            dq.pop_front();
        }
        if (!omitNaN && lastNaN >= low) {
            out[r - firstRow] = nan;
        } else if (dq.empty()) {
            out[r - firstRow] = nan;
        } else {
            out[r - firstRow] = (double)column[dq.front() - 1];
        }
    }
}
//=============================================================================
template <class T, class Acc>
static void
slidingSumColumn(const T* column, indexType n, indexType before, indexType after,
    indexType firstRow, indexType lastRow, bool omitNaN, bool mean, double* out)
{
    // O(n): a running total updated as the window slides. Acc is the accumulator
    // class: the sum keeps the class of the values (integers accumulate in
    // double), the mean accumulates in double and casts the output for single.
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const bool castSingle = mean && std::is_same<T, single>::value;
    Acc runningSum = (Acc)0;
    indexType count = 0; // non-NaN values currently in the window
    indexType nanCount = 0; // NaN values currently in the window
    indexType added = (firstRow > before) ? (firstRow - before - 1) : 0;
    indexType left = added + 1; // first index still in the window
    for (indexType r = firstRow; r <= lastRow; ++r) {
        indexType low = (r > before) ? (r - before) : 1;
        indexType high = std::min(n, r + after);
        // Drop leaving elements first to keep the running total near one window.
        while (left < low) {
            T value = column[left - 1];
            if (isNaNValue(value)) {
                --nanCount;
            } else {
                runningSum -= (Acc)value;
                --count;
            }
            ++left;
        }
        while (added < high) {
            ++added;
            T value = column[added - 1];
            if (isNaNValue(value)) {
                ++nanCount;
            } else {
                runningSum += (Acc)value;
                ++count;
            }
        }
        double value;
        if (mean) {
            if (!omitNaN && nanCount > 0) {
                value = nan;
            } else {
                value = (count > 0) ? ((double)runningSum / (double)count) : nan;
            }
        } else {
            value = (!omitNaN && nanCount > 0) ? nan : (double)runningSum;
        }
        if (castSingle && !std::isnan(value)) {
            value = (double)(single)value;
        }
        out[r - firstRow] = value;
    }
}
//=============================================================================
template <class T>
static double
medianWindow(const T* column, indexType low, indexType high, bool omitNaN, bool deviation,
    std::vector<T>& window, std::vector<T>& deviations)
{
    const double nan = std::numeric_limits<double>::quiet_NaN();
    window.clear();
    for (indexType k = low; k <= high; ++k) {
        T value = column[k - 1];
        if (isNaNValue(value)) {
            if (!omitNaN) {
                return nan;
            }
        } else {
            window.push_back(value);
        }
    }
    if (window.empty()) {
        return nan;
    }
    T median = selectMedian(window);
    if (!deviation) {
        return (double)median;
    }
    deviations.clear();
    for (T value : window) {
        T distance = saturatedAbs(saturatedDifference(value, median));
        if (isNaNValue(distance)) {
            return nan;
        }
        deviations.push_back(distance);
    }
    return (double)selectMedian(deviations);
}
//=============================================================================
template <class T>
static T
selectMedian(std::vector<T>& values)
{
    // values is not empty and holds no NaN; it is reordered.
    size_t half = values.size() / 2;
    std::nth_element(values.begin(), values.begin() + half, values.end());
    T high = values[half];
    if (values.size() % 2 == 1) {
        return high;
    }
    T low = *std::max_element(values.begin(), values.begin() + half);
    return castMedianValue<T>(((double)low + (double)high) / 2.0);
}
//=============================================================================
template <class T>
static T
castMedianValue(double value)
{
    // cast semantics of the language: round half away from zero, then clamp.
    if (!std::is_integral<T>::value) {
        return (T)value;
    }
    double rounded = std::round(value);
    if (rounded >= (double)std::numeric_limits<T>::max()) {
        return std::numeric_limits<T>::max();
    }
    if (rounded <= (double)std::numeric_limits<T>::lowest()) {
        return std::numeric_limits<T>::lowest();
    }
    return (T)rounded;
}
//=============================================================================
template <class T>
static T
saturatedDifference(T a, T b)
{
    // a - b with the saturated integer arithmetic of the language.
    if (std::is_integral<T>::value) {
        if (std::is_signed<T>::value) {
            if (b < (T)0 && a > (T)(std::numeric_limits<T>::max() + b)) {
                return std::numeric_limits<T>::max();
            }
            if (b > (T)0 && a < (T)(std::numeric_limits<T>::lowest() + b)) {
                return std::numeric_limits<T>::lowest();
            }
        } else if (a < b) {
            return (T)0;
        }
    }
    return (T)(a - b);
}
//=============================================================================
template <class T>
static T
saturatedAbs(T value)
{
    if (std::is_integral<T>::value && std::is_signed<T>::value
        && value == std::numeric_limits<T>::lowest()) {
        return std::numeric_limits<T>::max();
    }
    return (value < (T)0) ? (T)(0 - value) : value;
}
//=============================================================================
template <class T>
static bool
isNaNValue(T value)
{
    return std::is_floating_point<T>::value && std::isnan((double)value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
