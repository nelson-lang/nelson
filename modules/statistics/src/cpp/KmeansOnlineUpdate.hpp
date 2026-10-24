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
#include "KmeansOnlineSquareEuclidean.hpp"
//=============================================================================
indexType
onlineUpdate(const KmeansData& data, const Options& options, std::vector<double>& centers,
    indexType k, std::vector<double>& labels, std::vector<double>& distances)
{
    std::vector<double> sumd(k, 0.0);
    assignPoints(data, options, centers, k, labels, distances, sumd);
    std::vector<indexType> counts(k, 0);
    for (double label : labels) {
        ++counts[static_cast<indexType>(label) - 1];
    }
    if (options.distance == Distance::SQUAREDEUCLIDEAN) {
        double total = std::accumulate(sumd.begin(), sumd.end(), 0.0);
        return onlineUpdateSquareEuclidean(data, centers, k, labels, counts, total);
    }
    auto assignedTotal
        = [&data, &options, k](const std::vector<double>& mu, const std::vector<double>& z) {
              double total = 0.0;
              for (indexType r = 0; r < data.rows; ++r) {
                  indexType cluster = static_cast<indexType>(z[r]) - 1;
                  total += distanceToCenter(data, mu, k, r, cluster, options.distance);
              }
              return total;
          };
    double total = assignedTotal(centers, labels);
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
            std::vector<double> trialLabels = labels;
            std::vector<double> trialCenters = centers;
            std::vector<double> trialDistances = distances;
            trialLabels[r] = static_cast<double>(cluster + 1);
            recomputeCenters(data, options, trialCenters, k, trialLabels, trialDistances);
            double trialTotal = assignedTotal(trialCenters, trialLabels);
            if (trialTotal < total) {
                labels.swap(trialLabels);
                centers.swap(trialCenters);
                distances.swap(trialDistances);
                --counts[oldCluster];
                ++counts[cluster];
                total = trialTotal;
                ++moved;
                break;
            }
        }
    }
    return moved;
}
//=============================================================================
