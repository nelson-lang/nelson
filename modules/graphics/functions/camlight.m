%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = camlight(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  [ax, h, mode, az, el, style] = parseCamlightInputs(inputArguments);
  if strcmp(mode, 'left')
    az = -30; el = 30;
  elseif strcmp(mode, 'right')
    az = 30; el = 30;
  elseif strcmp(mode, 'headlight')
    az = 0; el = 0;
  elseif ~strcmp(mode, 'angle')
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  % camlight places a light near the camera, offset by az (azimuth) and el
  % (elevation) from the camera-to-target direction, so it moves with the
  % camera. The headlight sits exactly at the camera position. The light is
  % local by default; an 'infinite' style makes it directional.
  pos = camlightPosition(ax, az, el);
  if isempty(h)
    h = light(ax, 'Position', pos, 'Style', style);
  else
    set(h, 'Position', pos, 'Style', style);
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function pos = camlightPosition(ax, az, el)
  cp = ax.CameraPosition(:)';
  ct = ax.CameraTarget(:)';
  up = ax.CameraUpVector(:)';
  v = cp - ct;
  forward = ct - cp;
  if norm(forward) < eps
    forward = [0 0 -1];
  end
  forward = forward / norm(forward);
  right = cross(forward, up);
  if norm(right) < eps
    right = cross(forward, [0 1 0]);
    if norm(right) < eps
      right = cross(forward, [1 0 0]);
    end
  end
  right = right / norm(right);
  upn = cross(right, forward);
  v = (rotationAboutAxis(right, -el) * v')';
  v = (rotationAboutAxis(upn, az) * v')';
  pos = ct + v;
end
%=============================================================================
function R = rotationAboutAxis(axisVec, degrees)
  a = axisVec / norm(axisVec);
  t = degrees * pi / 180;
  c = cos(t); s = sin(t); C = 1 - c;
  x = a(1); y = a(2); z = a(3);
  R = [c + x*x*C,   x*y*C - z*s, x*z*C + y*s; ...
       y*x*C + z*s, c + y*y*C,   y*z*C - x*s; ...
       z*x*C - y*s, z*y*C + x*s, c + z*z*C];
end
%=============================================================================
function [ax, h, mode, az, el, style] = parseCamlightInputs(args)
  ax = gca();
  h = [];
  mode = 'right';
  az = 0;
  el = 0;
  style = 'local';
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  elseif ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'light')
    h = args{1};
    ax = ancestor(h, 'axes');
    args = args(2:end);
  end
  % An optional trailing 'local' or 'infinite' selects the light style.
  if ~isempty(args) && ischar(args{end}) && any(strcmpi(args{end}, {'local', 'infinite'}))
    style = lower(args{end});
    args = args(1:end - 1);
  end
  if ~isempty(args)
    if length(args) == 1 && ischar(args{1})
      mode = lower(args{1});
    elseif length(args) == 2 && isnumeric(args{1}) && isscalar(args{1}) ...
        && isnumeric(args{2}) && isscalar(args{2})
      az = args{1};
      el = args{2};
      mode = 'angle';
    else
      error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  end
end
%=============================================================================
