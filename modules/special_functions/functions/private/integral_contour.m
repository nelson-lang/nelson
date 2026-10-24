%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Integrates fun along the piecewise linear path points(1), points(2), ...
% Real segments use the adaptive rule directly (infinite ends allowed),
% complex segments are parameterized over [0, 1].
%=============================================================================
function [q, errbnd] = integral_contour(fun, points, reltol, abstol, arrayValued, maxIntervals)
  if nargin < 6
    maxIntervals = [];
  end
  if any(imag(points) ~= 0) && ~all(isfinite(points))
    error('Nelson:integral:nonFiniteContourError', _('Contour endpoints and waypoints must be finite.'));
  end
  q = 0;
  errbnd = 0;
  for k = 1:numel(points) - 1
    a = points(k);
    b = points(k + 1);
    if imag(a) == 0 && imag(b) == 0
      [part, err] = integral_gk(fun, real(a), real(b), reltol, abstol, arrayValued, maxIntervals);
    else
      dz = b - a;
      pathFun = @(t) fun(a + dz .* t) .* dz;
      [part, err] = integral_gk(pathFun, 0, 1, reltol, abstol, arrayValued, maxIntervals);
    end
    q = q + part;
    errbnd = errbnd + err;
  end
end
%=============================================================================
