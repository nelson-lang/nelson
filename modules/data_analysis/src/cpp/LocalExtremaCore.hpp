//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <type_traits>
#include <vector>
#include "LocalExtrema.hpp"
//=============================================================================
// Local maxima search on one slice whose NaN values were removed.
// K is the ordering key (double, single or int64: minima are searched as the
// maxima of reversed keys), P the prominence type (double, single or uint64).
// A flat run of equal values is one extremum: it is found in O(n) with a
// monotonic stack that gives, for each run, the nearest strictly higher run
// on each side and the lowest value in between (the prominence basis).
//=============================================================================
namespace Nelson {
//=============================================================================
template <class K, class P> struct LocalExtremaWork
{
    // slice without NaN: keys, positions in the slice, +Inf markers.
    std::vector<K> key;
    std::vector<indexType> position;
    std::vector<unsigned char> infinite;
    // sample points of the kept elements (only for windows and separation).
    std::vector<double> x;
    // runs of equal finite values (an infinite value is its own run).
    std::vector<indexType> runStart;
    std::vector<indexType> runEnd;
    std::vector<unsigned char> candidate;
    std::vector<P> prominence;
    std::vector<K> leftMin;
    std::vector<K> rightMin;
    std::vector<std::ptrdiff_t> leftBound;
    std::vector<std::ptrdiff_t> rightBound;
    std::vector<indexType> stack;
    std::vector<K> stackMin;
    // runs kept as extrema, in increasing order.
    std::vector<indexType> selected;
    std::vector<indexType> order;
};
//=============================================================================
template <class K>
inline K
localExtremaNoValue()
{
    return std::numeric_limits<K>::has_infinity ? std::numeric_limits<K>::infinity()
                                                : std::numeric_limits<K>::max();
}
//=============================================================================
template <class K, class P>
inline P
localExtremaDifference(K high, K low)
{
    if constexpr (std::is_integral<K>::value) {
        return (P)((uint64_t)high - (uint64_t)low);
    } else {
        return (P)(high - low);
    }
}
//=============================================================================
template <class K, class P>
inline P
localExtremaMagnitude(K value)
{
    if constexpr (std::is_integral<K>::value) {
        return value < 0 ? (P)((uint64_t)0 - (uint64_t)value) : (P)value;
    } else {
        return (P)std::fabs(value);
    }
}
//=============================================================================
template <class K, class P>
inline K
runValue(const LocalExtremaWork<K, P>& w, indexType run)
{
    return w.key[w.runStart[run]];
}
//=============================================================================
template <class K, class P>
static void
localExtremaRuns(LocalExtremaWork<K, P>& w)
{
    indexType m = (indexType)w.key.size();
    w.runStart.resize(m);
    w.runEnd.resize(m);
    indexType runs = 0;
    for (indexType k = 0; k < m; ++k) {
        bool extend = (k > 0) && !w.infinite[k] && !w.infinite[k - 1] && (w.key[k] == w.key[k - 1]);
        if (extend) {
            w.runEnd[runs - 1] = k;
        } else {
            w.runStart[runs] = k;
            w.runEnd[runs] = k;
            runs++;
        }
    }
    w.runStart.resize(runs);
    w.runEnd.resize(runs);
}
//=============================================================================
template <class K, class P>
static indexType
localExtremaCandidates(LocalExtremaWork<K, P>& w)
{
    indexType runs = (indexType)w.runStart.size();
    w.candidate.assign(runs, 0);
    indexType count = 0;
    for (indexType r = 0; r < runs; ++r) {
        bool isCandidate = w.infinite[w.runStart[r]] != 0;
        if (!isCandidate && r > 0 && r + 1 < runs) {
            K v = runValue(w, r);
            isCandidate = (runValue(w, r - 1) < v) && (runValue(w, r + 1) < v);
        }
        if (isCandidate) {
            w.candidate[r] = 1;
            count++;
        }
    }
    return count;
}
//=============================================================================
// For each candidate run: nearest strictly higher run on one side (bound, -1
// or runs when none) and lowest value of the runs in between (sideMin).
template <class K, class P>
static void
localExtremaScanSide(LocalExtremaWork<K, P>& w, bool fromLeft, std::vector<K>& sideMin,
    std::vector<std::ptrdiff_t>& bound)
{
    indexType runs = (indexType)w.runStart.size();
    sideMin.resize(runs);
    bound.resize(runs);
    w.stack.clear();
    w.stackMin.clear();
    for (indexType t = 0; t < runs; ++t) {
        indexType r = fromLeft ? t : runs - 1 - t;
        K v = runValue(w, r);
        K lowest = localExtremaNoValue<K>();
        while (!w.stack.empty() && runValue(w, w.stack.back()) <= v) {
            lowest = std::min(lowest, std::min(runValue(w, w.stack.back()), w.stackMin.back()));
            w.stack.pop_back();
            w.stackMin.pop_back();
        }
        if (w.candidate[r]) {
            sideMin[r] = lowest;
            if (w.stack.empty()) {
                bound[r] = fromLeft ? -1 : (std::ptrdiff_t)runs;
            } else {
                bound[r] = (std::ptrdiff_t)w.stack.back();
            }
        }
        w.stack.push_back(r);
        w.stackMin.push_back(lowest);
    }
}
//=============================================================================
// Lowest value on the left of run r inside [lo, ...]; false when no run of
// that side lies in the window.
template <class K, class P>
static bool
localExtremaLeftInWindow(const LocalExtremaWork<K, P>& w, indexType r, double lo, K& lowest)
{
    if (w.x[w.runEnd[r - 1]] < lo) {
        return false;
    }
    indexType first = (indexType)(w.leftBound[r] + 1);
    if (w.x[w.runEnd[first]] >= lo) {
        lowest = w.leftMin[r];
        return true;
    }
    lowest = localExtremaNoValue<K>();
    for (indexType q = r; q > first && w.x[w.runEnd[q - 1]] >= lo; --q) {
        lowest = std::min(lowest, runValue(w, q - 1));
    }
    return true;
}
//=============================================================================
template <class K, class P>
static bool
localExtremaRightInWindow(
    const LocalExtremaWork<K, P>& w, indexType r, double hi, bool rightOpen, K& lowest)
{
    auto inside = [&](indexType q) {
        double xq = w.x[w.runStart[q]];
        return rightOpen ? (xq < hi) : (xq <= hi);
    };
    if (!inside(r + 1)) {
        return false;
    }
    indexType last = (indexType)(w.rightBound[r] - 1);
    if (inside(last)) {
        lowest = w.rightMin[r];
        return true;
    }
    lowest = localExtremaNoValue<K>();
    for (indexType q = r + 1; q <= last && inside(q); ++q) {
        lowest = std::min(lowest, runValue(w, q));
    }
    return true;
}
//=============================================================================
template <class K, class P>
static void
localExtremaProminence(LocalExtremaWork<K, P>& w, const LocalExtremaOptions& options)
{
    indexType runs = (indexType)w.runStart.size();
    w.prominence.assign(runs, (P)0);
    localExtremaScanSide(w, true, w.leftMin, w.leftBound);
    localExtremaScanSide(w, false, w.rightMin, w.rightBound);
    for (indexType r = 0; r < runs; ++r) {
        if (!w.candidate[r]) {
            continue;
        }
        K v = runValue(w, r);
        if (w.infinite[w.runStart[r]]) {
            w.prominence[r] = std::numeric_limits<P>::infinity();
            continue;
        }
        if (!options.hasWindow) {
            w.prominence[r]
                = localExtremaDifference<K, P>(v, std::max(w.leftMin[r], w.rightMin[r]));
            continue;
        }
        K left = v;
        K right = v;
        bool hasLeft
            = localExtremaLeftInWindow(w, r, w.x[w.runStart[r]] - options.windowBefore, left);
        bool hasRight = localExtremaRightInWindow(
            w, r, w.x[w.runEnd[r]] + options.windowAfter, options.windowRightOpen, right);
        if (hasLeft && hasRight) {
            w.prominence[r] = localExtremaDifference<K, P>(v, std::max(left, right));
        } else if (hasLeft || hasRight) {
            w.prominence[r] = localExtremaDifference<K, P>(v, hasLeft ? left : right);
        } else {
            w.prominence[r] = localExtremaMagnitude<K, P>(v);
        }
    }
}
//=============================================================================
// An extremum closer than minSeparation to a kept extremum on its left with a
// prominence at least as large, or to any extremum on its right with a larger
// prominence, is removed. Infinite extrema are always kept. Both neighborhoods
// are sliding windows: their maxima come from monotonic queues.
template <class K, class P>
static void
localExtremaSeparation(LocalExtremaWork<K, P>& w, double minSeparation, bool flatLast)
{
    const std::vector<indexType>& s = w.selected;
    indexType n = (indexType)s.size();
    // with 'last', the left end of a finite extremum is its previous sample.
    auto lo = [&](indexType i) {
        indexType start = w.runStart[s[i]];
        return (flatLast && !w.infinite[start]) ? w.x[start - 1] : w.x[start];
    };
    auto hi = [&](indexType i) { return w.x[w.runEnd[s[i]]]; };
    auto prom = [&](indexType i) { return w.prominence[s[i]]; };
    std::vector<unsigned char> kept(n, 1);
    std::vector<indexType> leftQueue;
    std::vector<indexType> rightQueue;
    size_t leftHead = 0;
    size_t rightHead = 0;
    indexType left = 0;
    indexType right = 0;
    for (indexType i = 0; i < n; ++i) {
        while ((lo(i) - hi(left)) >= minSeparation) {
            left++;
        }
        while (leftHead < leftQueue.size() && leftQueue[leftHead] < left) {
            leftHead++;
        }
        right = std::max(i, right);
        while ((right + 1 < n) && ((lo(right + 1) - hi(i)) < minSeparation)) {
            right++;
            while (rightQueue.size() > rightHead && prom(rightQueue.back()) <= prom(right)) {
                rightQueue.pop_back();
            }
            rightQueue.push_back(right);
        }
        while (rightHead < rightQueue.size() && rightQueue[rightHead] <= i) {
            rightHead++;
        }
        if (!w.infinite[w.runStart[s[i]]]) {
            bool leftWins = leftHead < leftQueue.size() && prom(leftQueue[leftHead]) >= prom(i);
            bool rightWins = rightHead < rightQueue.size() && prom(rightQueue[rightHead]) > prom(i);
            kept[i] = (leftWins || rightWins) ? 0 : 1;
        }
        if (kept[i]) {
            while (leftQueue.size() > leftHead && prom(leftQueue.back()) <= prom(i)) {
                leftQueue.pop_back();
            }
            leftQueue.push_back(i);
        }
    }
    indexType count = 0;
    for (indexType i = 0; i < n; ++i) {
        if (kept[i]) {
            w.selected[count++] = s[i];
        }
    }
    w.selected.resize(count);
}
//=============================================================================
// Keeps the maxNumber most prominent extrema, the first one on ties.
template <class K, class P>
static void
localExtremaMostProminent(LocalExtremaWork<K, P>& w, indexType maxNumber)
{
    w.order = w.selected;
    auto before = [&](indexType a, indexType b) {
        P pa = w.prominence[a];
        P pb = w.prominence[b];
        return (pa > pb) || (!(pb > pa) && a < b);
    };
    std::nth_element(w.order.begin(), w.order.begin() + maxNumber, w.order.end(), before);
    w.order.resize(maxNumber);
    std::sort(w.order.begin(), w.order.end());
    w.selected.swap(w.order);
}
//=============================================================================
template <class K, class P>
static void
localExtremaSlice(LocalExtremaWork<K, P>& w, const LocalExtremaOptions& options, P minProminence)
{
    w.selected.clear();
    w.runStart.clear();
    w.runEnd.clear();
    w.candidate.clear();
    if (w.key.size() < 3) {
        return;
    }
    localExtremaRuns(w);
    indexType count = localExtremaCandidates(w);
    if (count == 0) {
        return;
    }
    bool filterByProminence = options.minProminence > 0;
    bool needProminence = options.wantProminence || filterByProminence
        || (options.minSeparation > 0) || (options.maxNumExtrema < (double)count);
    if (needProminence) {
        localExtremaProminence(w, options);
    }
    indexType runs = (indexType)w.runStart.size();
    for (indexType r = 0; r < runs; ++r) {
        if (w.candidate[r] && (!filterByProminence || w.prominence[r] >= minProminence)) {
            w.selected.push_back(r);
        }
    }
    if (options.minSeparation > 0 && w.selected.size() > 1) {
        localExtremaSeparation(
            w, options.minSeparation, options.flatSelection == LocalExtremaFlatSelection::Last);
    }
    if (options.maxNumExtrema < (double)w.selected.size()) {
        localExtremaMostProminent(w, (indexType)options.maxNumExtrema);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
