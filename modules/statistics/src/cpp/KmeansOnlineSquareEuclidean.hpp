//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// This file is part of Nelson.
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
indexType
onlineUpdateSquareEuclidean(const KmeansData& data, std::vector<double>& centers, indexType k,
    std::vector<double>& labels, const std::vector<indexType>& initialCounts, double total)
{
    std::vector<indexType> counts = initialCounts;
    std::vector<double> sums(k * data.cols, 0.0);
    std::vector<double> sumSquares(k, 0.0);
    for (indexType r = 0; r < data.rows; ++r) {
        indexType cluster = static_cast<indexType>(labels[r]) - 1;
        double rowSquare = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            double x = at(data.x, data.rows, r, c);
            sums[cluster + c * k] += x;
            rowSquare += x * x;
        }
        sumSquares[cluster] += rowSquare;
    }
    auto clusterCost = [&sums, &sumSquares, k, &data](indexType cluster, indexType count) {
        if (count == 0) {
            return 0.0;
        }
        double norm = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            double value = sums[cluster + c * k];
            norm += value * value;
        }
        return sumSquares[cluster] - norm / static_cast<double>(count);
    };
    auto movedClusterCost = [&sums, &sumSquares, k, &data](
                                indexType cluster, indexType count, indexType row, double sign) {
        if (count == 0) {
            return 0.0;
        }
        double norm = 0.0;
        double rowSquare = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            double x = at(data.x, data.rows, row, c);
            double value = sums[cluster + c * k] + sign * x;
            norm += value * value;
            rowSquare += x * x;
        }
        return sumSquares[cluster] + sign * rowSquare - norm / static_cast<double>(count);
    };
    auto updateCenter
        = [&sums, k, &data](std::vector<double>& values, indexType cluster, indexType count) {
              for (indexType c = 0; c < data.cols; ++c) {
                  double value = count == 0 ? std::numeric_limits<double>::quiet_NaN()
                                            : sums[cluster + c * k] / static_cast<double>(count);
                  setCenterAt(values, k, cluster, c, value);
              }
          };
    double withinTotal = 0.0;
    for (indexType cluster = 0; cluster < k; ++cluster) {
        withinTotal += clusterCost(cluster, counts[cluster]);
    }
    indexType moved = 0;
    for (indexType r = 0; r < data.rows; ++r) {
        indexType oldCluster = static_cast<indexType>(labels[r]) - 1;
        if (counts[oldCluster] <= 1) {
            continue;
        }
        for (indexType cluster = 0; cluster < k; ++cluster) {
            if (cluster == oldCluster) {
                continue;
            }
            double currentCost = clusterCost(oldCluster, counts[oldCluster])
                + clusterCost(cluster, counts[cluster]);
            double candidateCost = movedClusterCost(oldCluster, counts[oldCluster] - 1, r, -1.0)
                + movedClusterCost(cluster, counts[cluster] + 1, r, 1.0);
            double candidateTotal = withinTotal - currentCost + candidateCost;
            if (candidateTotal < total) {
                for (indexType c = 0; c < data.cols; ++c) {
                    double x = at(data.x, data.rows, r, c);
                    sums[oldCluster + c * k] -= x;
                    sums[cluster + c * k] += x;
                    sumSquares[oldCluster] -= x * x;
                    sumSquares[cluster] += x * x;
                }
                --counts[oldCluster];
                ++counts[cluster];
                updateCenter(centers, oldCluster, counts[oldCluster]);
                updateCenter(centers, cluster, counts[cluster]);
                labels[r] = static_cast<double>(cluster + 1);
                withinTotal = candidateTotal;
                total = candidateTotal;
                ++moved;
                break;
            }
        }
    }
    return moved;
}
//=============================================================================
