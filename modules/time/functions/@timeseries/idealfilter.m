%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = idealfilter(obj, interval)
  if numel(interval) ~= 2
    error('Nelson:time:filterIntervalContainTwoLimits', 'Filter interval must contain two limits.');
  end
  y = timeseriesDataAsColumns(obj);
  if isempty(y)
    return
  end
  if min(interval) <= 0 && max(interval) >= 1
    obj.Data_ = y;
    return
  end
  kernel = ones(3, 1) ./ 3;
  padded = [y(1, :); y; y(end, :)];
  filtered = zeros(size(y));
  for k = 1:size(y, 1)
    filtered(k, :) = kernel.' * padded(k:k + 2, :);
  end
  obj.Data_ = filtered;
end
%=============================================================================
