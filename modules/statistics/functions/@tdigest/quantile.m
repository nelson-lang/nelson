%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function qval = quantile(td, q)
  if ~isvector(q)
    msg = _("Input quantiles must be a vector.");
    error(msg);
  end
  mustBeGreaterThanOrEqual(q, 0);
  mustBeLessThanOrEqual(q, 1);
  q = q(:); % Ensure q is a column vector
  st = struct(td);
  if isempty(st.means)
    qval = NaN(size(q));
    return;
  end
  
  % Sort centroids by mean value to ensure proper ordering
  [sorted_means, idx] = sort(st.means);
  sorted_weights = st.weights(idx);
  
  % Handle special case of single centroid
  if length(sorted_means) == 1
    qval = repmat(sorted_means(1), size(q));
    return;
  end
  
  % Create interpolation points for t-digest quantile estimation
  % We use cumulative weights to define the quantile positions
  cumw = cumsum(sorted_weights);
  
  % Quantile positions at the center of each centroid's weight range
  centroid_positions = (cumw - sorted_weights/2) / st.totalWeight;
  
  % Add boundary points for extrapolation
  quantile_positions = [0; centroid_positions; 1];
  interp_values = [sorted_means(1); sorted_means; sorted_means(end)];
  
  % Interpolate to find quantile values
  qval = interp1(quantile_positions, interp_values, q, 'linear');
  
  % Manual extrapolation for values outside the range
  below_range = q < 0;
  above_range = q > 1;
  
  % For values below range, use the minimum value
  qval(below_range) = sorted_means(1);
  
  % For values above range, use the maximum value
  qval(above_range) = sorted_means(end);
  
  qval = qval(:); % Ensure qval is a column vector
end
