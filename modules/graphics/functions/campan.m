%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function campan(varargin)
  % campan(dtheta, dphi), campan(ax, dtheta, dphi)
  % campan(..., coordsys) with coordsys 'data' (default) or 'camera'.
  % Rotates the camera target around the camera position: dtheta is the
  % horizontal rotation and dphi the vertical rotation, both in degrees.
  narginchk(2, 4);
  nargoutchk(0, 0);
  [ax, args] = __camAxesArgs__(varargin);
  if (numel(args) < 2)
    error('Nelson:graphics:invalidInputArguments', 'Horizontal and vertical angles expected.');
  end
  dtheta = args{1};
  dphi = args{2};
  if (~(isnumeric(dtheta) && isscalar(dtheta) && isreal(dtheta) && isfinite(dtheta)) ...
      || ~(isnumeric(dphi) && isscalar(dphi) && isreal(dphi) && isfinite(dphi)))
    error('Nelson:graphics:invalidCamAngle', 'Rotation angles must be real scalars.');
  end
  coordsys = 'data';
  if (numel(args) >= 3)
    coordsys = lower(char(args{3}));
  end
  pos = get(ax, 'CameraPosition');
  tgt = get(ax, 'CameraTarget');
  up = get(ax, 'CameraUpVector');
  viewDir = tgt - pos;
  right = cross(viewDir, up);
  if (strcmp(coordsys, 'camera'))
    horizontalAxis = up;
  else
    horizontalAxis = [0, 0, 1];
  end
  offset = tgt - pos;
  offset = __camRotateVector__(offset, horizontalAxis, dtheta);
  right = __camRotateVector__(right, horizontalAxis, dtheta);
  offset = __camRotateVector__(offset, right, dphi);
  set(ax, 'CameraTarget', pos + offset);
end
%=============================================================================
