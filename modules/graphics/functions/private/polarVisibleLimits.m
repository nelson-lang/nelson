%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [xLimits, yLimits] = polarVisibleLimits(thetaLim, radius, thetaAxisUnits, zeroOffset, dirSign)
  if nargin < 3
    thetaAxisUnits = 'degrees';
  end
  if nargin < 4
    zeroOffset = 0;
  end
  if nargin < 5
    dirSign = 1;
  end
  if radius <= 0 || ~isfinite(radius)
    radius = 1;
  end
  fullCircle = 360;
  if strcmp(thetaAxisUnits, 'radians')
    fullCircle = 2 * pi;
  end
  % A full circle fills the symmetric box whatever the orientation convention.
  if abs(abs(thetaLim(2) - thetaLim(1)) - fullCircle) < 1e-12
    xLimits = [-radius radius];
    yLimits = [-radius radius];
    return
  end
  % Sample the visible arc and map each data angle through the same convention
  % as the grid and data, so the box fits the wedge after rotation/direction.
  theta = linspace(thetaLim(1), thetaLim(2), 361);
  if strcmp(thetaAxisUnits, 'degrees')
    theta = theta * pi / 180;
  end
  angle = zeroOffset + dirSign .* theta;
  x = [0 radius * cos(angle)];
  y = [0 radius * sin(angle)];
  xLimits = expandEqualLimits([min(x) max(x)], radius);
  yLimits = expandEqualLimits([min(y) max(y)], radius);
end
%=============================================================================
function limits = expandEqualLimits(limits, radius)
  if abs(limits(2) - limits(1)) < eps
    delta = max(radius * 0.05, eps);
    limits = limits + [-delta delta];
  end
end
%=============================================================================
