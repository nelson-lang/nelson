%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function camlookat(varargin)
  % camlookat, camlookat(ax), camlookat(obj)
  % Points the camera at an object (or the current axes contents) and moves
  % the camera so the object fills the view, keeping the viewing direction.
  narginchk(0, 1);
  nargoutchk(0, 0);
  if (nargin == 0)
    ax = gca();
    target = ax;
  else
    h = varargin{1};
    if (~(isscalar(h) && isgraphics(h)))
      error('Nelson:graphics:invalidInputArguments', 'A graphics object expected.');
    end
    if (isgraphics(h, 'axes'))
      ax = h;
      target = h;
    else
      ax = ancestor(h, 'axes');
      target = h;
    end
  end
  b = __camObjectBounds__(target);
  center = [(b(1) + b(2)) / 2, (b(3) + b(4)) / 2, (b(5) + b(6)) / 2];
  radius = 0.5 * norm([b(2) - b(1), b(4) - b(3), b(6) - b(5)]);
  if (radius == 0 || ~isfinite(radius))
    radius = 1;
  end
  pos = get(ax, 'CameraPosition');
  oldTarget = get(ax, 'CameraTarget');
  viewDir = pos - oldTarget;
  dist = norm(viewDir);
  if (dist == 0 || ~isfinite(dist))
    viewDir = [0, 0, 1];
  else
    viewDir = viewDir / dist;
  end
  va = get(ax, 'CameraViewAngle');
  halfAngle = max(min(va, 179), 1) * pi / 360;
  newDistance = radius / sin(halfAngle);
  set(ax, 'CameraTarget', center);
  set(ax, 'CameraPosition', center + viewDir * newDistance);
end
%=============================================================================
