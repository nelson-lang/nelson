%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [xLimits, yLimits] = polarVisibleLimits(thetaLim, radius, thetaAxisUnits)
  if nargin < 3
    thetaAxisUnits = 'degrees';
  end
  if radius <= 0 || ~isfinite(radius)
    radius = 1;
  end
  fullCircle = 360;
  if strcmp(thetaAxisUnits, 'radians')
    fullCircle = 2 * pi;
  end
  if abs(abs(thetaLim(2) - thetaLim(1)) - fullCircle) < 1e-12
    xLimits = [-radius radius];
    yLimits = [-radius radius];
    return
  end

  theta = visibleBoundaryAngles(thetaLim, thetaAxisUnits);
  if strcmp(thetaAxisUnits, 'degrees')
    theta = theta * pi / 180;
  end
  x = [0 radius * cos(theta)];
  y = [0 radius * sin(theta)];
  xLimits = expandEqualLimits([min(x) max(x)], radius);
  yLimits = expandEqualLimits([min(y) max(y)], radius);
end
%=============================================================================
function angles = visibleBoundaryAngles(thetaLim, thetaAxisUnits)
  angles = thetaLim;
  cardinal = -720:90:720;
  if strcmp(thetaAxisUnits, 'radians')
    cardinal = cardinal * pi / 180;
  end
  cardinal = cardinal(cardinal > thetaLim(1) & cardinal < thetaLim(2));
  angles = unique([angles cardinal]);
end
%=============================================================================
function limits = expandEqualLimits(limits, radius)
  if abs(limits(2) - limits(1)) < eps
    delta = max(radius * 0.05, eps);
    limits = limits + [-delta delta];
  end
end
%=============================================================================
