%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function points = ip_feature_points_struct(location, metric)
  if isempty(location)
    points = struct('Location', zeros(0, 2), 'Metric', zeros(0, 1), 'Count', 0);
    return
  end
  [metric, order] = sort(metric(:), 'descend');
  points = struct('Location', location(order, :), 'Metric', metric, 'Count', numel(metric));
end
%=============================================================================
