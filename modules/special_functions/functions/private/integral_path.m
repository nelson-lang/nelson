%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Path of integration from a to b through waypoints:
% real problem: [a, b] split at the waypoints lying strictly inside it,
% complex problem: piecewise linear contour through the waypoints in order.
%=============================================================================
function points = integral_path(a, b, waypoints)
  if isreal(a) && isreal(b) && isreal(waypoints)
    a = double(a);
    b = double(b);
    inside = waypoints(waypoints > min(a, b) & waypoints < max(a, b));
    inside = unique(inside);
    if a > b
      inside = fliplr(inside);
    end
    points = [a, inside, b];
  else
    points = [double(a), waypoints, double(b)];
  end
end
%=============================================================================
