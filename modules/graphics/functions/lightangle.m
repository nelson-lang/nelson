%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lightangle(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  [ax, h, az, el] = parseLightAngleInputs(inputArguments);
  pos = angleToPosition(ax, az, el);
  if isempty(h)
    h = light(ax, 'Position', pos, 'Style', 'infinite');
  else
    set(h, 'Position', pos);
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function [ax, h, az, el] = parseLightAngleInputs(args)
  ax = gca();
  h = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  elseif ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'light')
    h = args{1};
    ax = ancestor(h, 'axes');
    args = args(2:end);
  end
  if length(args) ~= 2 || ~isnumeric(args{1}) || ~isscalar(args{1}) ...
      || ~isnumeric(args{2}) || ~isscalar(args{2})
    error('Nelson:lightangle:azimuthAndElevationExpected', 'Azimuth and elevation expected.');
  end
  az = args{1};
  el = args{2};
end
%=============================================================================
function pos = angleToPosition(ax, az, el)
  az = az * pi / 180;
  el = el * pi / 180;
  dataAspectRatio = ax.DataAspectRatio;
  if numel(dataAspectRatio) ~= 3 || any(~isfinite(dataAspectRatio)) || any(dataAspectRatio <= 0)
    dataAspectRatio = [1 1 1];
  end
  xScale = hypot(dataAspectRatio(1), dataAspectRatio(3));
  yScale = hypot(dataAspectRatio(2), dataAspectRatio(3));
  zScale = xScale / dataAspectRatio(1);
  pos = [xScale * cos(el) * sin(az), -yScale * cos(el) * cos(az), zScale * sin(el)];
end
%=============================================================================
