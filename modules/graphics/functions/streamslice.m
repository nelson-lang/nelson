%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = streamslice(varargin)
  narginchk(2, 12);
  nargoutchk(0, 2);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [fieldArgs, density, arrows] = parseStreamSliceOptions(args);
  [vertices, arrowVertices] = streamSliceVertices(fieldArgs, density, arrows);
  if nargout >= 2
    varargout{1} = vertices;
    varargout{2} = arrowVertices;
    return
  end
  streamVertices = [vertices(:); arrowVertices(:)];
  if isempty(parent)
    h = streamline(streamVertices);
  else
    h = streamline(parent, streamVertices);
  end
  updateStreamSliceAxes(h, fieldArgs);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [fieldArgs, density, arrows] = parseStreamSliceOptions(args)
  density = 1;
  arrows = true;
  while ~isempty(args) && ischar(args{end})
    option = lower(args{end});
    if startsWith(option, 'no')
      arrows = false;
    elseif startsWith(option, 'ar')
      arrows = true;
    end
    args = args(1:end - 1);
  end
  if ~isempty(args) && isnumeric(args{end}) && isscalar(args{end}) ...
      && any(numel(args) == [3 5 7 10])
    density = double(args{end});
    args = args(1:end - 1);
  end
  if density <= 0 || ~isfinite(density)
    error('Nelson:streamslice:invalidDensity', 'Density must be a positive scalar.');
  end
  fieldArgs = args;
end
%=============================================================================
function [vertices, arrowVertices] = streamSliceVertices(args, density, arrows)
  switch numel(args)
    case 2
      U = args{1};
      V = args{2};
      validateSliceVectorComponents(U, V);
      [X, Y] = meshgrid(1:size(U, 2), 1:size(U, 1));
      [vertices, arrowVertices] = __streamslice2d__(X, Y, U, V, density, logical(arrows));
    case 4
      X = args{1};
      Y = args{2};
      U = args{3};
      V = args{4};
      validateSliceVectorComponents(U, V);
      [vertices, arrowVertices] = __streamslice2d__(X, Y, U, V, density, logical(arrows));
    case 6
      U = args{1};
      V = args{2};
      W = args{3};
      [X, Y, Z] = meshgrid(1:size(U, 2), 1:size(U, 1), 1:size(U, 3));
      [vertices, arrowVertices] = verticesFromStreamline({X, Y, Z, U, V, W, args{4}, args{5}, args{6}});
    case 9
      [vertices, arrowVertices] = verticesFromStreamline(args);
    otherwise
      if numel(args) < 2
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      if numel(args) > 9
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function updateStreamSliceAxes(h, args)
  [xLimits, yLimits, ok] = streamSlice2DBounds(args);
  if ~ok || isempty(h)
    return
  end
  ax = h(1).Parent;
  ax.XLim = xLimits;
  ax.YLim = streamSliceYAxisLimits(yLimits);
end
%=============================================================================
function limits = streamSliceYAxisLimits(yLimits)
  limits = yLimits;
  if yLimits(1) < 0 && yLimits(2) > 0
    limits(2) = limits(2) + 0.5;
  end
end
%=============================================================================
function [xLimits, yLimits, ok] = streamSlice2DBounds(args)
  ok = false;
  xLimits = [];
  yLimits = [];
  if numel(args) == 2
    U = args{1};
    xLimits = [1, size(U, 2)];
    yLimits = [1, size(U, 1)];
  elseif numel(args) == 4
    X = args{1};
    Y = args{2};
    xLimits = [min(X(:)), max(X(:))];
    yLimits = [min(Y(:)), max(Y(:))];
  else
    return
  end
  ok = all(isfinite([xLimits, yLimits])) && diff(xLimits) > 0 && diff(yLimits) > 0;
end
%=============================================================================
function validateSliceVectorComponents(U, V)
  if ~isnumeric(U) || ~isnumeric(V) || ~isequal(size(U), size(V)) || numel(U) < 4
    error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
  end
end
%=============================================================================
function [vertices, arrowVertices] = verticesFromStreamline(args)
  [h, state] = streamlineBase(args{:});
  vertices = cell(numel(h), 1);
  arrowVertices = cell(numel(h), 1);
  for k = 1:numel(h)
    vertices{k} = lineVertices(h(k));
    arrowVertices{k} = arrowFromVertices(vertices{k});
  end
  delete(h);
end
%=============================================================================
function vertices = lineVertices(h)
  x = h.XData(:);
  y = h.YData(:);
  if isprop(h, 'ZData') && ~isempty(h.ZData)
    z = h.ZData(:);
    vertices = [x y z];
  else
    vertices = [x y];
  end
  vertices = vertices(all(isfinite(vertices), 2), :);
end
%=============================================================================
function arrow = arrowFromVertices(vertices)
  if size(vertices, 1) < 3
    arrow = zeros(0, size(vertices, 2));
    return
  end
  dim = size(vertices, 2);
  distances = sqrt(sum(diff(vertices, 1, 1) .^ 2, 2));
  totalLength = sum(distances);
  if totalLength <= 0
    arrow = zeros(0, size(vertices, 2));
    return
  end
  if dim ~= 2
    last = size(vertices, 1);
    arrow = [vertices(max(1, last - 1), :); vertices(last, :)];
    return
  end
  cumulative = [0; cumsum(distances)];
  idx = find(cumulative >= 0.55 * totalLength, 1);
  idx = min(max(idx, 2), size(vertices, 1) - 1);
  tangent = vertices(idx + 1, :) - vertices(idx - 1, :);
  tangentLength = norm(tangent);
  if tangentLength == 0
    arrow = zeros(0, dim);
    return
  end
  tangent = tangent / tangentLength;
  normal = [-tangent(2), tangent(1)];
  span = norm(max(vertices, [], 1) - min(vertices, [], 1));
  arrowLength = max(0.05 * span, 0.08);
  arrowLength = min(arrowLength, 0.18);
  theta = pi / 7;
  tip = vertices(idx, :);
  left = tip - arrowLength * (cos(theta) * tangent - sin(theta) * normal);
  right = tip - arrowLength * (cos(theta) * tangent + sin(theta) * normal);
  arrow = [left; tip; right];
end
%=============================================================================
