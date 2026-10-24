%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rotate(h, direction, alpha, origin)
  narginchk(3, 4);
  if isempty(h)
    return
  end
  u = directionAxis(direction);
  angle = alpha * pi / 180;
  R = rodriguesMatrix(u, angle);
  if nargin < 4
    origin = defaultOrigin(h);
  end
  if numel(origin) ~= 3
    error('Nelson:rotate:invalidOrigin', 'ORIGIN must be a three-element vector.');
  end
  for k = 1:numel(h)
    rotateOneObject(h(k), R, origin);
  end
end
%=============================================================================
function u = directionAxis(direction)
  if numel(direction) == 2
    az = direction(1) * pi / 180;
    el = direction(2) * pi / 180;
    u = [cos(el) * cos(az), cos(el) * sin(az), sin(el)];
  elseif numel(direction) == 3
    u = direction(:)';
  else
    error('Nelson:rotate:invalidDirection', ...
      'DIRECTION must be a two- or three-element vector.');
  end
  nrm = norm(u);
  if nrm == 0
    error('Nelson:rotate:zeroDirection', 'DIRECTION must be nonzero.');
  end
  u = u / nrm;
end
%=============================================================================
function R = rodriguesMatrix(u, angle)
  c = cos(angle);
  s = sin(angle);
  C = 1 - c;
  x = u(1);
  y = u(2);
  z = u(3);
  R = [c + x ^ 2 * C, x * y * C - z * s, x * z * C + y * s; ...
    y * x * C + z * s, c + y ^ 2 * C, y * z * C - x * s; ...
    z * x * C - y * s, z * y * C + x * s, c + z ^ 2 * C];
end
%=============================================================================
function origin = defaultOrigin(h)
  ax = ancestor(h(1), 'axes');
  if isempty(ax)
    origin = [0, 0, 0];
    return
  end
  origin = [mean(get(ax, 'XLim')), mean(get(ax, 'YLim')), mean(get(ax, 'ZLim'))];
end
%=============================================================================
function rotateOneObject(obj, R, origin)
  xData = get(obj, 'XData');
  yData = get(obj, 'YData');
  zData = get(obj, 'ZData');
  hasZ = ~isempty(zData);
  % A surface may store vector XData/YData against a matrix ZData; expand them
  % to the full grid so all three arrays share one shape.
  if hasZ && ~isequal(size(xData), size(zData)) ...
      && isvector(xData) && isvector(yData) && ~isvector(zData)
    [xData, yData] = meshgrid(xData, yData);
  end
  if ~hasZ
    zData = zeros(size(xData));
  end
  sz = size(xData);
  P = [xData(:)' - origin(1); yData(:)' - origin(2); zData(:)' - origin(3)];
  P = R * P;
  xNew = reshape(P(1, :), sz) + origin(1);
  yNew = reshape(P(2, :), sz) + origin(2);
  zNew = reshape(P(3, :), sz) + origin(3);
  set(obj, 'XData', xNew, 'YData', yNew);
  if hasZ
    set(obj, 'ZData', zNew);
  end
end
%=============================================================================
