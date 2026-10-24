%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [k, av] = convhull(varargin)
  nargoutchk(0, 2);
  narginchk(1, 5);
  [points, options] = parseConvhullInputs(varargin{:});
  if isempty(options)
    [facets, measure] = convhulln(points);
  else
    [facets, measure] = convhulln(points, options);
  end
  if size(points, 2) == 2
    k = orderHullEdges(facets);
  else
    k = facets;
  end
  if nargout == 0
    if size(points, 2) ~= 2
      error('Nelson:geometry:plottingSupportedTwoDimensionalInputs', 'Plotting is only supported for two-dimensional inputs.');
    end
    plot(points(k, 1), points(k, 2));
    return
  end
  if nargout > 1
    av = measure;
  end
end
%=============================================================================
function [points, options] = parseConvhullInputs(varargin)
  options = [];
  if length(varargin) >= 2 && isTextScalar(varargin{end - 1}) ...
      && strcmpi(char(varargin{end - 1}), 'Simplify')
    varargin = varargin(1:end - 2);
  elseif length(varargin) >= 2 && isTextScalar(varargin{end}) ...
      && startsWith(lower(char(varargin{end})), 'simplify')
    varargin = varargin(1:end - 1);
  else
    last = varargin{end};
    if ischar(last) || isStringScalar(last) || iscell(last)
      options = last;
      varargin = varargin(1:end - 1);
    end
  end
  if length(varargin) == 1
    points = varargin{1};
  elseif length(varargin) == 2
    points = [varargin{1}(:), varargin{2}(:)];
  elseif length(varargin) == 3
    points = [varargin{1}(:), varargin{2}(:), varargin{3}(:)];
  else
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
  if ~isnumeric(points) || size(points, 2) < 2
    error('Nelson:geometry:pointsNumericMatrixLeastTwoColumns', '%s must be a numeric matrix with at least two columns.', 'Points');
  end
end
%=============================================================================
function tf = isStringScalar(value)
  tf = isa(value, 'string') && isscalar(value);
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function order = orderHullEdges(edges)
  if isempty(edges)
    order = [];
    return
  end
  order = edges(1, :);
  edges(1, :) = [];
  while ~isempty(edges)
    last = order(end);
    idx = find(edges(:, 1) == last | edges(:, 2) == last, 1);
    if isempty(idx)
      break
    end
    if edges(idx, 1) == last
      order(end + 1) = edges(idx, 2);
    else
      order(end + 1) = edges(idx, 1);
    end
    edges(idx, :) = [];
    if order(end) == order(1)
      break
    end
  end
  if order(end) ~= order(1)
    order(end + 1) = order(1);
  end
  order = order(:);
end
%=============================================================================
