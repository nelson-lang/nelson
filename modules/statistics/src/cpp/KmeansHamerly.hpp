//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Hamerly triangle-inequality acceleration for squared Euclidean assignment.
// Like Elkan it is exact: per-iteration labels match assignPoints. Unlike Elkan
// it keeps a single lower bound per point (distance to the second closest
// centroid), so bound maintenance is O(n) per iteration instead of O(n*k).
//=============================================================================
struct HamerlyState
{
    bool enabled { true };
    bool initialized { false };
    indexType k { 0 };
    std::vector<double> upper; // Euclidean upper bound to the assigned centroid.
    std::vector<double> lower; // Euclidean lower bound to the closest other centroid.
    std::vector<indexType> assign; // zero-based assigned cluster per row.
    std::vector<double> assignedSq; // exact squared distance to the assigned centroid.
    std::vector<double> snapshot; // centroid positions when the bounds were last valid.
};
//=============================================================================
inline std::vector<double>
nearestOtherCenter(const std::vector<double>& centers, indexType k, indexType cols)
{
    std::vector<double> nearest(k, std::numeric_limits<double>::infinity());
    for (indexType a = 0; a < k; ++a) {
        for (indexType b = a + 1; b < k; ++b) {
            double d = std::sqrt(centerCenterSquared(centers, k, cols, a, b));
            nearest[a] = std::min(nearest[a], d);
            nearest[b] = std::min(nearest[b], d);
        }
    }
    return nearest;
}
//=============================================================================
// Closest centroid (lowest index on ties, like assignPoints) plus the squared
// distance to the second closest centroid, used to seed the lower bound.
inline void
closestTwo(const KmeansData& data, const std::vector<double>& centers, indexType k, indexType r,
    std::vector<double>& distances, indexType& bestCluster, double& bestSq, double& secondSq)
{
    bestSq = std::numeric_limits<double>::infinity();
    secondSq = std::numeric_limits<double>::infinity();
    bestCluster = 0;
    for (indexType cluster = 0; cluster < k; ++cluster) {
        double dsq = distanceToCenter(data, centers, k, r, cluster, Distance::SQUAREDEUCLIDEAN);
        distances[r + cluster * data.rows] = dsq;
        if (dsq < bestSq) {
            secondSq = bestSq;
            bestSq = dsq;
            bestCluster = cluster;
        } else if (dsq < secondSq) {
            secondSq = dsq;
        }
    }
}
//=============================================================================
void
hamerlyFullAssign(HamerlyState& state, const KmeansData& data, std::vector<double>& centers,
    indexType k, std::vector<double>& labels, std::vector<double>& distances,
    std::vector<double>& sumd)
{
    indexType rows = data.rows;
    state.k = k;
    state.upper.assign(rows, 0.0);
    state.lower.assign(rows, 0.0);
    state.assign.assign(rows, 0);
    state.assignedSq.assign(rows, 0.0);
    std::fill(sumd.begin(), sumd.end(), 0.0);
    for (indexType r = 0; r < rows; ++r) {
        indexType bestCluster = 0;
        double bestSq = 0.0;
        double secondSq = 0.0;
        closestTwo(data, centers, k, r, distances, bestCluster, bestSq, secondSq);
        state.assign[r] = bestCluster;
        state.assignedSq[r] = bestSq;
        state.upper[r] = std::sqrt(bestSq);
        state.lower[r] = std::sqrt(secondSq);
        labels[r] = static_cast<double>(bestCluster + 1);
        sumd[bestCluster] += bestSq;
    }
    state.snapshot = centers;
    state.initialized = true;
}
//=============================================================================
void
hamerlyIncrementalAssign(HamerlyState& state, const KmeansData& data, std::vector<double>& centers,
    indexType k, std::vector<double>& labels, std::vector<double>& distances,
    std::vector<double>& sumd)
{
    indexType rows = data.rows;
    indexType cols = data.cols;
    std::vector<double> shift(k, 0.0);
    double maxShift = 0.0;
    double secondShift = 0.0;
    indexType maxShiftCluster = 0;
    for (indexType cluster = 0; cluster < k; ++cluster) {
        double s = centerShift(centers, state.snapshot, k, cols, cluster);
        shift[cluster] = s;
        if (s > maxShift) {
            secondShift = maxShift;
            maxShift = s;
            maxShiftCluster = cluster;
        } else if (s > secondShift) {
            secondShift = s;
        }
    }
    std::vector<double> nearest = nearestOtherCenter(centers, k, cols);
    for (indexType r = 0; r < rows; ++r) {
        indexType a = state.assign[r];
        state.upper[r] += shift[a];
        double drop = (a == maxShiftCluster) ? secondShift : maxShift;
        double next = state.lower[r] - drop;
        state.lower[r] = next > 0.0 ? next : 0.0;
    }
    std::fill(sumd.begin(), sumd.end(), 0.0);
    for (indexType r = 0; r < rows; ++r) {
        indexType a = state.assign[r];
        double threshold = std::max(0.5 * nearest[a], state.lower[r]);
        // Strict comparison keeps the lowest-index tie-break identical to assignPoints.
        if (state.upper[r] < threshold) {
            distances[r + a * rows] = state.assignedSq[r];
            sumd[a] += state.assignedSq[r];
            continue;
        }
        // Tighten the upper bound, then re-test before a full scan.
        double assignedSq = distanceToCenter(data, centers, k, r, a, Distance::SQUAREDEUCLIDEAN);
        state.upper[r] = std::sqrt(assignedSq);
        state.assignedSq[r] = assignedSq;
        if (state.upper[r] < threshold) {
            distances[r + a * rows] = assignedSq;
            sumd[a] += assignedSq;
            continue;
        }
        indexType bestCluster = 0;
        double bestSq = 0.0;
        double secondSq = 0.0;
        closestTwo(data, centers, k, r, distances, bestCluster, bestSq, secondSq);
        state.assign[r] = bestCluster;
        state.assignedSq[r] = bestSq;
        state.upper[r] = std::sqrt(bestSq);
        state.lower[r] = std::sqrt(secondSq);
        distances[r + bestCluster * rows] = bestSq;
        labels[r] = static_cast<double>(bestCluster + 1);
        sumd[bestCluster] += bestSq;
    }
    // Points kept by the skip never touch labels above, so refresh them here.
    for (indexType r = 0; r < rows; ++r) {
        labels[r] = static_cast<double>(state.assign[r] + 1);
    }
    state.snapshot = centers;
}
//=============================================================================
void
hamerlyAssign(HamerlyState& state, const KmeansData& data, const Options& options,
    std::vector<double>& centers, indexType k, std::vector<double>& labels,
    std::vector<double>& distances, std::vector<double>& sumd)
{
    if (!state.enabled) {
        assignPoints(data, options, centers, k, labels, distances, sumd);
        return;
    }
    if (!centersAllFinite(centers)) {
        state.enabled = false;
        state.initialized = false;
        assignPoints(data, options, centers, k, labels, distances, sumd);
        return;
    }
    if (!state.initialized || state.k != k) {
        hamerlyFullAssign(state, data, centers, k, labels, distances, sumd);
    } else {
        hamerlyIncrementalAssign(state, data, centers, k, labels, distances, sumd);
    }
    if (options.emptyAction == EmptyAction::SINGLETON && hasEmptyCluster(labels, k, data.rows)) {
        assignPoints(data, options, centers, k, labels, distances, sumd);
        state.initialized = false;
    }
}
//=============================================================================
