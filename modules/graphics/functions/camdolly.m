%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function camdolly(varargin)
  % camdolly(dx, dy, dz), camdolly(ax, dx, dy, dz)
  % camdolly(..., coordsys) coordsys 'camera' (default) or 'data'.
  % camdolly(..., coordsys, units) units 'normalized' (default) or 'data'.
  % Moves both the camera position and the camera target.
  narginchk(3, 6);
  nargoutchk(0, 0);
  [ax, args] = __camAxesArgs__(varargin);
  if (numel(args) < 3)
    error('Nelson:graphics:invalidInputArguments', 'dx, dy and dz expected.');
  end
  dx = args{1};
  dy = args{2};
  dz = args{3};
  if (~isDollyScalar(dx) || ~isDollyScalar(dy) || ~isDollyScalar(dz))
    error('Nelson:graphics:invalidCamOffset', 'dx, dy and dz must be real scalars.');
  end
  coordsys = 'camera';
  if (numel(args) >= 4)
    coordsys = lower(char(args{4}));
  end
  units = 'normalized';
  if (numel(args) >= 5)
    units = lower(char(args{5}));
  end
  pos = get(ax, 'CameraPosition');
  tgt = get(ax, 'CameraTarget');
  up = get(ax, 'CameraUpVector');
  viewDir = tgt - pos;
  distance = norm(viewDir);
  if (distance == 0 || ~isfinite(distance))
    distance = 1;
  end
  if (strcmp(units, 'normalized'))
    scale = distance;
  else
    scale = 1;
  end
  if (strcmp(coordsys, 'data'))
    delta = scale * [dx, dy, dz];
  else
    forward = viewDir / distance;
    right = cross(forward, up);
    rn = norm(right);
    if (rn == 0 || ~isfinite(rn))
      right = [1, 0, 0];
    else
      right = right / rn;
    end
    realUp = cross(right, forward);
    delta = scale * (dx * right + dy * realUp + dz * forward);
  end
  set(ax, 'CameraPosition', pos + delta);
  set(ax, 'CameraTarget', tgt + delta);
end
%=============================================================================
function tf = isDollyScalar(v)
  tf = isnumeric(v) && isscalar(v) && isreal(v) && isfinite(v);
end
%=============================================================================
