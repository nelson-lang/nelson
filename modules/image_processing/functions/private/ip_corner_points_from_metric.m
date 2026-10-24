%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function points = ip_corner_points_from_metric(metric, minQuality, roi)
  metric(~isfinite(metric)) = 0;
  metric(metric < 0) = 0;
  maxMetric = max(metric(:));
  if maxMetric <= 0
    points = ip_feature_points_struct([], []);
    return
  end
  mask = metric >= maxMetric * minQuality;
  mask = ip_apply_corner_roi(mask, roi);
  [rows, cols] = size(metric);
  loc = [];
  vals = [];
  for r = 2:rows - 1
    for c = 2:cols - 1
      value = metric(r, c);
      if mask(r, c) && value > 0 && value == max(max(metric(r - 1:r + 1, c - 1:c + 1)))
        loc = [loc; c r];
        vals = [vals; value];
      end
    end
  end
  points = ip_feature_points_struct(loc, vals);
end
%=============================================================================
