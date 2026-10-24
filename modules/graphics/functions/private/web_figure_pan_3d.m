%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_pan_3d(ax, request)
  % Camera navigation uses scaled axes coordinates, including log decades.
  position = get(ax, 'CameraPosition');
  target = get(ax, 'CameraTarget');
  direction = ones(1, 3);
  for k = 1:3
    if strcmp(get(ax, [char('X' + k - 1) 'Dir']), 'reverse')
      direction(k) = -1;
    end
  end
  forward = (target - position) .* direction;
  distance = norm(forward);
  if distance == 0 || ~isfinite(distance)
    return;
  end
  forward = forward / distance;
  right = cross(forward, get(ax, 'CameraUpVector') .* direction);
  if norm(right) == 0
    right = [1 0 0];
  else
    right = right / norm(right);
  end
  delta = distance * 0.2 * (-request.dxFrac * right + request.dyFrac * cross(right, forward)) .* direction;
  args = {};
  for k = 1:3
    dimension = char('X' + k - 1);
    limits = get(ax, [dimension 'Lim']);
    if strcmp(get(ax, [dimension 'Scale']), 'log')
      limits = 10 .^ (log10(limits) + delta(k));
    else
      limits = limits + delta(k);
    end
    if any(~isfinite(limits)) || limits(1) >= limits(2) || ...
        (strcmp(get(ax, [dimension 'Scale']), 'log') && limits(1) <= 0)
      error('Nelson:graphics:invalidPanLimits', 'Pan exceeds the finite axes range.');
    end
    args = [args {[dimension 'Lim'], limits}];
  end
  % Validate every dimension before changing the camera or the viewing volume.
  set(ax, args{:}, 'CameraPosition', position + delta, 'CameraTarget', target + delta);
end
