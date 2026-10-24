//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__findpeaks__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>
//=============================================================================
// Internal helper of findpeaks.m: the per-peak searches that walk the signal.
//
// [leftPos, rightPos] = __findpeaks__('bases', signal, peakIndices)
//   For each peak, the nearest higher sample on each side bounds a window; the
//   outputs are the positions of the lowest sample of each window (NaN samples
//   are transparent). Without a higher sample on the left the window is the
//   whole prefix and the last lowest sample is reported; on the right and
//   inside a bounded window the first lowest sample is reported.
// [left, right] = __findpeaks__('crossings', signal, peakIndices, reference)
//   For each peak, the nearest sample on each side that is not above its
//   reference level (a NaN sample stops the search), the peak itself when it
//   is not above the level, and the signal edge when nothing qualifies.
// keep = __findpeaks__('distance', locations, order, minDistance)
//   Greedy selection: the peaks are visited in the given order, a visited
//   peak that is still kept discards every other peak within minDistance.
//=============================================================================
namespace Nelson::SignalProcessingGateway {
//=============================================================================
static const double*
realVectorData(ArrayOf& value, const wchar_t* name);
static std::vector<indexType>
indexVector(const ArrayOf& value, indexType upperBound, const wchar_t* name);
static std::vector<indexType>
sortedByPosition(const std::vector<indexType>& positions);
static ArrayOf
positionOutput(const std::vector<indexType>& positions);
static ArrayOfVector
peakBases(const ArrayOf& signalArg, const ArrayOf& peaksArg);
static ArrayOfVector
peakCrossings(const ArrayOf& signalArg, const ArrayOf& peaksArg, const ArrayOf& referenceArg);
static ArrayOfVector
peakDistance(const ArrayOf& locationsArg, const ArrayOf& orderArg, double minDistance);
template <class T>
static void
distanceKeep(const T* location, indexType n, const std::vector<indexType>& order,
    double minDistance, logical* keep);
//=============================================================================
ArrayOfVector
__findpeaks__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 2);
    std::wstring operation = argIn[0].asWideString();
    if (operation == L"bases") {
        nargincheck(argIn, 3, 3);
        return peakBases(argIn[1], argIn[2]);
    }
    if (operation == L"crossings") {
        nargincheck(argIn, 4, 4);
        return peakCrossings(argIn[1], argIn[2], argIn[3]);
    }
    if (operation == L"distance") {
        nargincheck(argIn, 4, 4);
        return peakDistance(argIn[1], argIn[2], argIn[3].asDoubleScalar());
    }
    Error(L"Nelson:signal:findpeaks:unknownOperation", L"Unknown operation \"%s\".",
        operation.c_str());
    return {};
}
//=============================================================================
static const double*
realVectorData(ArrayOf& value, const wchar_t* name)
{
    if (!value.isNumeric() || value.isComplex() || value.isSparse()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", name);
    }
    if (!value.isEmpty() && !value.isVector()) {
        Error(L"Nelson:validators:mustBeVector", L"%s must be a vector.", name);
    }
    value.promoteType(NLS_DOUBLE);
    return static_cast<const double*>(value.dataPointer());
}
//=============================================================================
static std::vector<indexType>
indexVector(const ArrayOf& value, indexType upperBound, const wchar_t* name)
{
    ArrayOf indices = value;
    const double* data = realVectorData(indices, name);
    indexType n = indices.elementCount();
    std::vector<indexType> result(n);
    for (indexType k = 0; k < n; ++k) {
        double v = data[k];
        if (!(v >= 1) || !(v <= static_cast<double>(upperBound)) || std::floor(v) != v) {
            Error(L"Nelson:signal:findpeaks:invalidIndex", L"%s must be an index into the signal.",
                name);
        }
        result[k] = static_cast<indexType>(v) - 1;
    }
    return result;
}
//=============================================================================
static std::vector<indexType>
sortedByPosition(const std::vector<indexType>& positions)
{
    std::vector<indexType> order(positions.size());
    std::iota(order.begin(), order.end(), static_cast<indexType>(0));
    std::stable_sort(order.begin(), order.end(),
        [&positions](indexType a, indexType b) { return positions[a] < positions[b]; });
    return order;
}
//=============================================================================
static ArrayOf
positionOutput(const std::vector<indexType>& positions)
{
    indexType n = positions.size();
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    for (indexType k = 0; k < n; ++k) {
        data[k] = static_cast<double>(positions[k] + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n, 1), data);
}
//=============================================================================
struct BaseEntry
{
    indexType position;
    double sample;
    double lowest;
    indexType first;
    indexType last;
};
//=============================================================================
static ArrayOfVector
peakBases(const ArrayOf& signalArg, const ArrayOf& peaksArg)
{
    ArrayOf signal = signalArg;
    const double* y = realVectorData(signal, L"signal");
    indexType n = signal.elementCount();
    std::vector<indexType> peaks = indexVector(peaksArg, n, L"peakIndices");
    std::vector<indexType> order = sortedByPosition(peaks);
    size_t count = peaks.size();
    std::vector<indexType> leftPos(count);
    std::vector<indexType> rightPos(count);
    std::vector<BaseEntry> stack;
    stack.reserve(64);
    // left to right: the stack holds the samples not yet exceeded, each with
    // the lowest sample between the previous entry and itself.
    size_t next = 0;
    for (indexType i = 0; i < n; ++i) {
        double v = y[i];
        if (std::isnan(v)) {
            while (next < count && peaks[order[next]] == i) {
                leftPos[order[next]] = i;
                ++next;
            }
            continue;
        }
        double lowest = v;
        indexType first = i;
        indexType last = i;
        while (!stack.empty() && stack.back().sample <= v) {
            const BaseEntry& entry = stack.back();
            if (entry.lowest < lowest) {
                lowest = entry.lowest;
                first = entry.first;
                last = entry.last;
            } else if (entry.lowest == lowest) {
                first = entry.first;
            }
            stack.pop_back();
        }
        while (next < count && peaks[order[next]] == i) {
            leftPos[order[next]] = stack.empty() ? last : first;
            ++next;
        }
        stack.push_back({ i, v, lowest, first, last });
    }
    // right to left: same walk, the first lowest sample is always reported.
    stack.clear();
    size_t remaining = count;
    for (indexType i = n; i-- > 0;) {
        double v = y[i];
        if (std::isnan(v)) {
            while (remaining > 0 && peaks[order[remaining - 1]] == i) {
                rightPos[order[remaining - 1]] = i;
                --remaining;
            }
            continue;
        }
        double lowest = v;
        indexType first = i;
        while (!stack.empty() && stack.back().sample <= v) {
            const BaseEntry& entry = stack.back();
            if (entry.lowest < lowest) {
                lowest = entry.lowest;
                first = entry.first;
            }
            stack.pop_back();
        }
        while (remaining > 0 && peaks[order[remaining - 1]] == i) {
            rightPos[order[remaining - 1]] = first;
            --remaining;
        }
        stack.push_back({ i, v, lowest, first, first });
    }
    ArrayOfVector retval;
    retval << positionOutput(leftPos);
    retval << positionOutput(rightPos);
    return retval;
}
//=============================================================================
static ArrayOfVector
peakCrossings(const ArrayOf& signalArg, const ArrayOf& peaksArg, const ArrayOf& referenceArg)
{
    ArrayOf signal = signalArg;
    const double* y = realVectorData(signal, L"signal");
    indexType n = signal.elementCount();
    std::vector<indexType> peaks = indexVector(peaksArg, n, L"peakIndices");
    ArrayOf reference = referenceArg;
    const double* level = realVectorData(reference, L"reference");
    size_t count = peaks.size();
    if (reference.elementCount() != count) {
        Error(L"Nelson:validators:sizeMismatch", L"%s must have the same number of elements as %s.",
            L"reference", L"peakIndices");
    }
    std::vector<indexType> order = sortedByPosition(peaks);
    std::vector<indexType> left(count);
    std::vector<indexType> right(count);
    // a NaN sample stops the walk: it behaves as the lowest possible level.
    const double lowestLevel = -std::numeric_limits<double>::infinity();
    std::vector<indexType> stackPos;
    std::vector<double> stackVal;
    stackPos.reserve(64);
    stackVal.reserve(64);
    // left to right: the stack holds the candidates that are lower than every
    // sample after them, so the levels grow from bottom to top.
    size_t next = 0;
    for (indexType i = 0; i < n; ++i) {
        double w = std::isnan(y[i]) ? lowestLevel : y[i];
        while (next < count && peaks[order[next]] == i) {
            size_t q = order[next];
            double ref = level[q];
            if (!(y[i] > ref)) {
                left[q] = i;
            } else {
                auto it = std::upper_bound(stackVal.begin(), stackVal.end(), ref);
                left[q] = (it == stackVal.begin()) ? 0 : stackPos[(it - stackVal.begin()) - 1];
            }
            ++next;
        }
        while (!stackVal.empty() && stackVal.back() >= w) {
            stackVal.pop_back();
            stackPos.pop_back();
        }
        stackVal.push_back(w);
        stackPos.push_back(i);
    }
    stackVal.clear();
    stackPos.clear();
    size_t remaining = count;
    for (indexType i = n; i-- > 0;) {
        double w = std::isnan(y[i]) ? lowestLevel : y[i];
        while (remaining > 0 && peaks[order[remaining - 1]] == i) {
            size_t q = order[remaining - 1];
            double ref = level[q];
            if (!(y[i] > ref)) {
                right[q] = i;
            } else {
                auto it = std::upper_bound(stackVal.begin(), stackVal.end(), ref);
                right[q] = (it == stackVal.begin()) ? n - 1 : stackPos[(it - stackVal.begin()) - 1];
            }
            --remaining;
        }
        while (!stackVal.empty() && stackVal.back() >= w) {
            stackVal.pop_back();
            stackPos.pop_back();
        }
        stackVal.push_back(w);
        stackPos.push_back(i);
    }
    ArrayOfVector retval;
    retval << positionOutput(left);
    retval << positionOutput(right);
    return retval;
}
//=============================================================================
static ArrayOfVector
peakDistance(const ArrayOf& locationsArg, const ArrayOf& orderArg, double minDistance)
{
    ArrayOf locations = locationsArg;
    if (!locations.isNumeric() || locations.isComplex() || locations.isSparse()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"locations");
    }
    if (!locations.isEmpty() && !locations.isVector()) {
        Error(L"Nelson:validators:mustBeVector", L"%s must be a vector.", L"locations");
    }
    indexType n = locations.elementCount();
    std::vector<indexType> order = indexVector(orderArg, n, L"order");
    logical* keep = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, n));
    std::fill(keep, keep + n, static_cast<logical>(1));
    if (locations.isSingleClass()) {
        distanceKeep<single>(
            static_cast<const single*>(locations.dataPointer()), n, order, minDistance, keep);
    } else {
        locations.promoteType(NLS_DOUBLE);
        distanceKeep<double>(
            static_cast<const double*>(locations.dataPointer()), n, order, minDistance, keep);
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_LOGICAL, Dimensions(n, 1), keep);
    return retval;
}
//=============================================================================
template <class T>
static void
distanceKeep(const T* location, indexType n, const std::vector<indexType>& order,
    double minDistance, logical* keep)
{
    // sorted by location, NaN locations last: they never discard and are never discarded.
    std::vector<indexType> sorted(n);
    std::iota(sorted.begin(), sorted.end(), static_cast<indexType>(0));
    std::stable_sort(sorted.begin(), sorted.end(), [location](indexType a, indexType b) {
        bool nanA = std::isnan(location[a]);
        bool nanB = std::isnan(location[b]);
        if (nanA || nanB) {
            return !nanA && nanB;
        }
        return location[a] < location[b];
    });
    indexType valid = n;
    while (valid > 0 && std::isnan(location[sorted[valid - 1]])) {
        --valid;
    }
    std::vector<indexType> rank(n);
    for (indexType r = 0; r < n; ++r) {
        rank[sorted[r]] = r;
    }
    // next alive sorted slot at or after a slot, with path compression.
    std::vector<indexType> nextAlive(valid + 1);
    std::iota(nextAlive.begin(), nextAlive.end(), static_cast<indexType>(0));
    auto findAlive = [&nextAlive](indexType k) {
        indexType root = k;
        while (nextAlive[root] != root) {
            root = nextAlive[root];
        }
        while (nextAlive[k] != root) {
            indexType following = nextAlive[k];
            nextAlive[k] = root;
            k = following;
        }
        return root;
    };
    auto discard = [&](indexType slot) {
        keep[sorted[slot]] = 0;
        nextAlive[slot] = slot + 1;
    };
    // the distance is measured in the class of the locations and compared as double
    auto within = [minDistance](T a, T b) {
        T d = a - b;
        if (d < 0) {
            d = -d;
        }
        return static_cast<double>(d) <= minDistance;
    };
    for (indexType visited : order) {
        if (!keep[visited]) {
            continue;
        }
        T center = location[visited];
        if (std::isnan(center) || !(0. <= minDistance)) {
            continue;
        }
        indexType r = rank[visited];
        if (std::isinf(center)) {
            // every finite or opposite-infinite location is infinitely far away
            if (!std::isinf(minDistance)) {
                continue;
            }
            for (indexType k = findAlive(0); k < valid; k = findAlive(k + 1)) {
                indexType q = sorted[k];
                if (q == visited
                    || (std::isinf(location[q]) && ((location[q] > 0) == (center > 0)))) {
                    continue;
                }
                discard(k);
            }
            continue;
        }
        // the locations within reach occupy a contiguous run of sorted slots around r
        indexType lo = 0;
        {
            indexType a = 0;
            indexType b = r;
            while (a < b) {
                indexType m = a + (b - a) / 2;
                if (within(location[sorted[m]], center)) {
                    b = m;
                } else {
                    a = m + 1;
                }
            }
            lo = a;
        }
        indexType hi = r;
        {
            indexType a = r + 1;
            indexType b = valid;
            while (a < b) {
                indexType m = a + (b - a) / 2;
                if (within(location[sorted[m]], center)) {
                    a = m + 1;
                } else {
                    b = m;
                }
            }
            hi = a - 1;
        }
        for (indexType k = findAlive(lo); k <= hi; k = findAlive(k + 1)) {
            if (k != r) {
                discard(k);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson::SignalProcessingGateway
//=============================================================================
