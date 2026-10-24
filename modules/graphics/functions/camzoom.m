%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function camzoom(varargin)
  % camzoom(zf), camzoom(ax, zf)
  % zf > 1 zooms in (narrows the camera view angle), zf < 1 zooms out.
  narginchk(1, 2);
  nargoutchk(0, 0);
  [ax, args] = __camAxesArgs__(varargin);
  if (numel(args) ~= 1)
    error('Nelson:graphics:invalidInputArguments', 'A single zoom factor expected.');
  end
  zf = args{1};
  if (~(isnumeric(zf) && isscalar(zf) && isreal(zf) && zf > 0 && isfinite(zf)))
    error('Nelson:graphics:invalidZoomFactor', 'Zoom factor must be a positive real scalar.');
  end
  % The zoom factor scales the projected size, i.e. the tangent of the half
  % view angle, not the angle itself.
  va = get(ax, 'CameraViewAngle');
  newva = 2 * atand(tand(va / 2) / zf);
  newva = min(max(newva, 1e-6), 180 - 1e-6);
  set(ax, 'CameraViewAngle', newva);
end
%=============================================================================
