%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function td = compress(td)
  st = struct(td);
  if isempty(st.totalWeight) || st.totalWeight == 0
    return;
  end
  if isempty(st.means) || numel(st.means) <= 1
    return;
  end
  
  % Sort centroids by mean value
  [st.means, idx] = sort(st.means);
  st.weights = st.weights(idx);
  
  % Preallocate for speed - use the maximum possible size
  n = numel(st.means);
  mergedMeans   = zeros(n, 1);
  mergedWeights = zeros(n, 1);
  idxMerged = 1;
  
  curMean   = st.means(1);
  curWeight = st.weights(1);
  cumulative = 0;
  
  % Pre-calculate constants for speed optimization
  totalWeightFactor = 4 * st.totalWeight / st.compression;
  invTotalWeight = 1 / st.totalWeight;
  
  % Vectorized compression - first pass
  for i = 2:n
    q = cumulative * invTotalWeight;
    maxWeight = totalWeightFactor * q * (1 - q);
    
    if curWeight + st.weights(i) <= maxWeight
      % Merge centroids - optimized weighted average
      newWeight = curWeight + st.weights(i);
      % Use incremental update formula to avoid redundant multiplication
      curMean   = curMean + (st.means(i) - curMean) * st.weights(i) / newWeight;
      curWeight = newWeight;
    else
      % Store current centroid and start new one
      mergedMeans(idxMerged)   = curMean;
      mergedWeights(idxMerged) = curWeight;
      idxMerged = idxMerged + 1;
      
      curMean   = st.means(i);
      curWeight = st.weights(i);
    end
    cumulative = cumulative + st.weights(i);
  end
  
  % Store the last centroid
  mergedMeans(idxMerged)   = curMean;
  mergedWeights(idxMerged) = curWeight;
  
  % Trim to actual size
  mergedMeans = mergedMeans(1:idxMerged);
  mergedWeights = mergedWeights(1:idxMerged);
  
  % Second pass: Limit the number of centroids to ceil(compression)
  maxCentroids = ceil(st.compression);
  if numel(mergedMeans) > maxCentroids
    % Simple approach: merge smallest adjacent pairs
    numToRemove = numel(mergedMeans) - maxCentroids;
    
    for j = 1:numToRemove
      if numel(mergedMeans) <= 1
        break;
      end
      
      % Find smallest adjacent pair sum (vectorized)
      adjSums = mergedWeights(1:end-1) + mergedWeights(2:end);
      [dummy, minIdx] = min(adjSums);
      
      % Merge the pair
      w1 = mergedWeights(minIdx);
      w2 = mergedWeights(minIdx + 1);
      m1 = mergedMeans(minIdx);
      m2 = mergedMeans(minIdx + 1);
      
      newWeight = w1 + w2;
      newMean = (m1 * w1 + m2 * w2) / newWeight;
      
      % Replace first element with merged result
      mergedMeans(minIdx) = newMean;
      mergedWeights(minIdx) = newWeight;
      
      % Remove second element
      mergedMeans(minIdx + 1) = [];
      mergedWeights(minIdx + 1) = [];
    end
  end
  
  td = feval('@tdigest/create_tdigest', st.compression, st.totalWeight, mergedMeans, mergedWeights);
end