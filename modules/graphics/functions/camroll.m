%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function camroll(varargin)
  % camroll(dtheta), camroll(ax, dtheta)
  % Rotates the camera up vector about the viewing axis by dtheta degrees.
  narginchk(1, 2);
  nargoutchk(0, 0);
  [ax, args] = __camAxesArgs__(varargin);
  if (numel(args) ~= 1)
    error('Nelson:graphics:invalidInputArguments', 'A single roll angle expected.');
  end
  dtheta = args{1};
  if (~(isnumeric(dtheta) && isscalar(dtheta) && isreal(dtheta) && isfinite(dtheta)))
    error('Nelson:graphics:invalidRollAngle', 'Roll angle must be a real scalar.');
  end
  pos = get(ax, 'CameraPosition');
  tgt = get(ax, 'CameraTarget');
  up = get(ax, 'CameraUpVector');
  viewAxis = tgt - pos;
  set(ax, 'CameraUpVector', __camRotateVector__(up, viewAxis, dtheta));
end
%=============================================================================
