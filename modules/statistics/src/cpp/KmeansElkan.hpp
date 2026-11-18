//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Elkan triangle-inequality acceleration for the squared Euclidean batch
// assignment. It is an exact optimization: the per-iteration labels it
// produces are identical to assignPoints, so kmeans results are unchanged.
// It only prunes distance computations between a point and centroids that the
// triangle inequality proves cannot be the nearest.
//=============================================================================
struct ElkanState
{
    bool enabled { true };
    bool initialized { false };
    indexType k { 0 };
    std::vector<double> upper; // Euclidean upper bound on the distance to the assigned centroid.
    std::vector<double> lower; // Euclidean lower bounds, laid out row-major as [row * k + cluster].
    std::vector<indexType> assign; // zero-based assigned cluster per row.
    std::vector<double> assignedSq; // exact squared distance to the assigned centroid.
    std::vector<double> snapshot; // centroid positions when the bounds were last valid.
};
//=============================================================================
inline bool
centersAllFinite(const std::vector<double>& centers)
{
    for (double value : centers) {
        if (!std::isfinite(value)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
inline bool
hasEmptyCluster(const std::vector<double>& labels, indexType k, indexType rows)
{
    std::vector<indexType> counts(k, 0);
    for (indexType r = 0; r < rows; ++r) {
        ++counts[static_cast<indexType>(labels[r]) - 1];
    }
    for (indexType cluster = 0; cluster < k; ++cluster) {
        if (counts[cluster] == 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
inline double
centerCenterSquared(
    const std::vector<double>& centers, indexType k, indexType cols, indexType a, indexType b)
{
    double s = 0.0;
    for (indexType c = 0; c < cols; ++c) {
        double d = centers[a + c * k] - centers[b + c * k];
        s += d * d;
    }
    return s;
}
//=============================================================================
inline double
centerShift(const std::vector<double>& centers, const std::vector<double>& snapshot, indexType k,
    indexType cols, indexType cluster)
{
    double s = 0.0;
    for (indexType c = 0; c < cols; ++c) {
        double d = centers[cluster + c * k] - snapshot[cluster + c * k];
        s += d * d;
    }
    return std::sqrt(s);
}
//=============================================================================
inline std::vector<double>
halfNearestCenter(const std::vector<double>& centers, indexType k, indexType cols)
{
    std::vector<double> half(k, std::numeric_limits<double>::infinity());
    for (indexType a = 0; a < k; ++a) {
        for (indexType b = a + 1; b < k; ++b) {
            double d = std::sqrt(centerCenterSquared(centers, k, cols, a, b));
            half[a] = std::min(half[a], d);
            half[b] = std::min(half[b], d);
        }
    }
    for (indexType a = 0; a < k; ++a) {
        half[a] *= 0.5;
    }
    return half;
}
//=============================================================================
void
elkanFullAssign(ElkanState& state, const KmeansData& data, std::vector<double>& centers,
    indexType k, std::vector<double>& labels, std::vector<double>& distances,
    std::vector<double>& sumd)
{
    indexType rows = data.rows;
    state.k = k;
    state.upper.assign(rows, 0.0);
    state.lower.assign(rows * k, 0.0);
    state.assign.assign(rows, 0);
    state.assignedSq.assign(rows, 0.0);
    std::fill(sumd.begin(), sumd.end(), 0.0);
    for (indexType r = 0; r < rows; ++r) {
        double best = std::numeric_limits<double>::infinity();
        indexType bestCluster = 0;
        for (indexType cluster = 0; cluster < k; ++cluster) {
            double dsq = distanceToCenter(data, centers, k, r, cluster, Distance::SQUAREDEUCLIDEAN);
            distances[r + cluster * rows] = dsq;
            state.lower[r * k + cluster] = std::sqrt(dsq);
            if (dsq < best) {
                best = dsq;
                bestCluster = cluster;
            }
        }
        state.assign[r] = bestCluster;
        state.assignedSq[r] = best;
        state.upper[r] = std::sqrt(best);
        labels[r] = static_cast<double>(bestCluster + 1);
        sumd[bestCluster] += best;
    }
    state.snapshot = centers;
    state.initialized = true;
}
//=============================================================================
void
elkanIncrementalAssign(ElkanState& state, const KmeansData& data, std::vector<double>& centers,
    indexType k, std::vector<double>& labels, std::vector<double>& distances,
    std::vector<double>& sumd)
{
    indexType rows = data.rows;
    indexType cols = data.cols;
    std::vector<double> shift(k, 0.0);
    for (indexType cluster = 0; cluster < k; ++cluster) {
        shift[cluster] = centerShift(centers, state.snapshot, k, cols, cluster);
    }
    std::vector<double> halfMin = halfNearestCenter(centers, k, cols);
    for (indexType r = 0; r < rows; ++r) {
        indexType a = state.assign[r];
        state.upper[r] += shift[a];
        for (indexType cluster = 0; cluster < k; ++cluster) {
            double next = state.lower[r * k + cluster] - shift[cluster];
            state.lower[r * k + cluster] = next > 0.0 ? next : 0.0;
        }
    }
    std::fill(sumd.begin(), sumd.end(), 0.0);
    for (indexType r = 0; r < rows; ++r) {
        indexType a = state.assign[r];
        // Tighten the upper bound to the exact distance to the current assigned centroid.
        double assignedSq = distanceToCenter(data, centers, k, r, a, Distance::SQUAREDEUCLIDEAN);
        double assignedDist = std::sqrt(assignedSq);
        state.upper[r] = assignedDist;
        state.lower[r * k + a] = assignedDist;
        state.assignedSq[r] = assignedSq;
        if (state.upper[r] < halfMin[a]) {
            // No other centroid can be strictly closer, so the assignment is unchanged.
            distances[r + a * rows] = assignedSq;
            sumd[a] += assignedSq;
            continue;
        }
        // Scan clusters in index order with a strict comparison so ties resolve to the
        // lowest index, exactly like assignPoints. Pruned clusters are proven to be
        // strictly farther than the assigned one, so they can never be the nearest.
        double best = std::numeric_limits<double>::infinity();
        indexType bestCluster = a;
        for (indexType cluster = 0; cluster < k; ++cluster) {
            double dsq;
            if (cluster == a) {
                dsq = assignedSq;
            } else {
                if (state.lower[r * k + cluster] > state.upper[r]) {
                    continue;
                }
                dsq = distanceToCenter(data, centers, k, r, cluster, Distance::SQUAREDEUCLIDEAN);
                state.lower[r * k + cluster] = std::sqrt(dsq);
            }
            if (dsq < best) {
                best = dsq;
                bestCluster = cluster;
            }
        }
        state.assign[r] = bestCluster;
        state.assignedSq[r] = best;
        state.upper[r] = std::sqrt(best);
        distances[r + bestCluster * rows] = best;
        labels[r] = static_cast<double>(bestCluster + 1);
        sumd[bestCluster] += best;
    }
    // Points kept by the fast skip never touch labels above, so refresh them here.
    for (indexType r = 0; r < rows; ++r) {
        labels[r] = static_cast<double>(state.assign[r] + 1);
    }
    state.snapshot = centers;
}
//=============================================================================
void
elkanAssign(ElkanState& state, const KmeansData& data, const Options& options,
    std::vector<double>& centers, indexType k, std::vector<double>& labels,
    std::vector<double>& distances, std::vector<double>& sumd)
{
    if (!state.enabled) {
        assignPoints(data, options, centers, k, labels, distances, sumd);
        return;
    }
    if (!centersAllFinite(centers)) {
        // Drop empty-action produced NaN centroids: fall back for the rest of the run.
        state.enabled = false;
        state.initialized = false;
        assignPoints(data, options, centers, k, labels, distances, sumd);
        return;
    }
    if (!state.initialized || state.k != k) {
        elkanFullAssign(state, data, centers, k, labels, distances, sumd);
    } else {
        elkanIncrementalAssign(state, data, centers, k, labels, distances, sumd);
    }
    if (options.emptyAction == EmptyAction::SINGLETON && hasEmptyCluster(labels, k, data.rows)) {
        // The singleton repair reads the full distance matrix, so recompute it exactly.
        assignPoints(data, options, centers, k, labels, distances, sumd);
        state.initialized = false;
    }
}
//=============================================================================
