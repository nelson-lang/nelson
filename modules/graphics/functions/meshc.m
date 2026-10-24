%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = meshc(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseSurfaceParent(args);
  validateMeshcZArgument(args);
  saveHold = ishold(ax);
  firstProperty = find(cellfun('isclass', args, 'char'), 1);
  if isempty(firstProperty)
    firstProperty = length(args) + 1;
  end
  defaultProperties = {'FaceColor', 'w', 'EdgeColor', 'flat', ...
    'FaceLighting', 'none', 'EdgeLighting', 'flat'};
  % The defaults come before the caller properties so that a property set by
  % the caller wins over the meshc default.
  surfaceArgs = [args(1:firstProperty - 1), defaultProperties, args(firstProperty:end)];
  s = surface(ax, surfaceArgs{:});
  hold(ax, 'on');
  [~, c] = contour(ax, s.XData, s.YData, s.ZData, 'ZLocation', 'zmin');
  applyMeshXYTicks(ax, s.XData, s.YData);
  applyMeshZLimits(ax, s.ZData);
  if ~saveHold
    hold(ax, 'off');
  end
  view(ax, 3);
  grid(ax, 'on');
  if nargout == 1
    varargout{1} = [s; c];
  end
end
%=============================================================================
function [ax, args] = parseSurfaceParent(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = newplot(args{1});
    args = args(2:end);
  else
    [parent, args] = extractParentProperty(args);
    if isempty(parent)
      ax = newplot();
    else
      ax = newplot(parent);
    end
  end
end
%=============================================================================
function [parent, args] = extractParentProperty(args)
  parent = [];
  k = 1;
  while k < numel(args)
    name = args{k};
    if (ischar(name) || isStringScalar(name)) && strcmpi(convertStringsToChars(name), 'Parent')
      value = args{k + 1};
      if ~isscalar(value) || ~isgraphics(value, 'axes')
        error('Nelson:graphics:invalidAxesParent', 'Parent must be an axes handle.');
      end
      parent = value;
      args(k:k + 1) = [];
      return
    end
    k = k + 1;
  end
end
%=============================================================================
function validateMeshcZArgument(args)
  if isempty(args)
    return
  end
  if numel(args) >= 3 && isnumeric(args{1}) && isnumeric(args{2}) && isnumeric(args{3})
    z = args{3};
  else
    z = args{1};
  end
  if isnumeric(z) && min(size(z)) == 1
    error('Nelson:meshc:invalidSurfaceZ', 'The surface Z must contain more than one row or column.');
  end
end
%=============================================================================
function applyMeshXYTicks(ax, xData, yData)
  xTicks = meshcLinearTicks(xData);
  if ~isempty(xTicks)
    ax.XTick = xTicks;
  end
  yTicks = meshcLinearTicks(yData);
  if ~isempty(yTicks)
    ax.YTick = yTicks;
  end
end
%=============================================================================
function ticks = meshcLinearTicks(data)
  values = data(isfinite(data));
  if isempty(values)
    ticks = [];
    return
  end
  limits = [min(values(:)) max(values(:))];
  span = limits(2) - limits(1);
  if ~isfinite(span) || span <= 0
    ticks = [];
    return
  end
  step = meshcNiceStep(span / 4);
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  if first > last
    ticks = [];
  else
    ticks = first:step:last;
  end
end
%=============================================================================
function applyMeshZLimits(ax, zData)
  zValues = zData(isfinite(zData));
  if isempty(zValues)
    return
  end
  limits = [min(zValues(:)) max(zValues(:))];
  if limits(1) == limits(2)
    limits = limits + [-0.5 0.5];
  end
  zlim(ax, limits);
  ticks = meshcZTicks(limits);
  if ~isempty(ticks)
    ax.ZTick = ticks;
  end
end
%=============================================================================
function ticks = meshcZTicks(limits)
  span = limits(2) - limits(1);
  if ~isfinite(span) || span <= 0
    ticks = [];
    return
  end
  step = meshcNiceStep(span / 10);
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  if first > last
    ticks = [];
  else
    ticks = first:step:last;
  end
end
%=============================================================================
function step = meshcNiceStep(rawStep)
  magnitude = 10 ^ floor(log10(rawStep));
  scaled = rawStep / magnitude;
  if scaled <= 1.2
    multiplier = 1;
  elseif scaled <= 2.5
    multiplier = 2;
  elseif scaled <= 5
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * magnitude;
end
%=============================================================================