%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = triplot(varargin)
  nargoutchk(0, 1);
  narginchk(1, inf);
  inputArguments = convertStringToCharArgs(varargin);
  [tri, x, y, lineArguments] = parseTriplotInputs(inputArguments{:});
  [xData, yData] = triangulationLineData(tri, x, y);
  lineArguments = normalizeLineArguments(lineArguments);
  h = line(xData, yData, lineArguments{:});
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function [tri, x, y, lineArguments] = parseTriplotInputs(varargin)
  lineArguments = {};
  if isTriangulationObject(varargin{1})
    obj = varargin{1};
    points = obj.Points;
    tri = obj.ConnectivityList;
    if size(points, 2) < 2
      error('Nelson:triplot:invalidTriangulationPoints', 'Triangulation points must have at least two columns.');
    end
    x = points(:, 1);
    y = points(:, 2);
    lineArguments = varargin(2:end);
    return
  end
  if nargin < 3
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  tri = varargin{1};
  x = varargin{2};
  y = varargin{3};
  lineArguments = varargin(4:end);
end
%=============================================================================
function tf = isTriangulationObject(value)
  tf = isa(value, 'triangulation') || isa(value, 'delaunayTriangulation');
end
%=============================================================================
function lineArguments = normalizeLineArguments(lineArguments)
  if ~isempty(lineArguments) && isText(lineArguments{1}) ...
      && ~isValidGraphicsProperty('line', lineArguments{1})
    [lineStyle, color, marker, msg] = colstyle(lineArguments{1}, 'plot', false);
    if ~isempty(msg)
      error('Nelson:triplot:invalidLineSpec', msg.message);
    end
    lineArguments(1) = [];
    lineArguments = completeLineProperties(color, marker, lineStyle, lineArguments);
  else
    lineArguments = completeLineProperties([], 'none', '-', lineArguments);
  end
end
%=============================================================================
function lineArguments = completeLineProperties(color, marker, lineStyle, propertiesList)
  if isempty(color)
    color = getColorAndUpdateIndex(gca());
  end
  if isempty(marker)
    marker = 'none';
  end
  if isempty(lineStyle)
    if strcmp(marker, 'none')
      lineStyle = '-';
    else
      lineStyle = 'none';
    end
  end
  lineArguments = propertiesList;
  if ~hasProperty(lineArguments, 'Marker')
    lineArguments = [{'Marker', marker}, lineArguments];
  end
  if ~hasProperty(lineArguments, 'LineStyle')
    lineArguments = [{'LineStyle', lineStyle}, lineArguments];
  end
  if ~hasProperty(lineArguments, 'Color')
    lineArguments = [{'Color', color}, lineArguments];
  end
  if ~hasProperty(lineArguments, 'MarkerEdgeColor')
    lineArguments = [{'MarkerEdgeColor', color}, lineArguments];
  end
end
%=============================================================================
function tf = hasProperty(arguments, name)
  tf = false;
  for k = 1:2:length(arguments)
    if isText(arguments{k}) && strcmpi(arguments{k}, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = isText(value)
  tf = ischar(value) || (isa(value, 'string') && isscalar(value));
end
%=============================================================================
function [xData, yData] = triangulationLineData(tri, x, y)
  tri = double(tri);
  x = x(:);
  y = y(:);
  if size(tri, 2) ~= 3
    error('Nelson:graphics:invalidTriangleConnectivity', 'Triangle connectivity must have three columns.');
  end
  if any(tri(:) < 1) || any(tri(:) > length(x)) || any(tri(:) ~= fix(tri(:)))
    error('Nelson:graphics:triangleConnectivityContainsInvalid', 'Triangle connectivity contains invalid point indices.');
  end
  edges = [tri(:, [1 2]); tri(:, [2 3]); tri(:, [3 1])];
  edges = unique(sort(edges, 2), 'rows');
  n = size(edges, 1);
  xData = NaN(3 * n, 1);
  yData = NaN(3 * n, 1);
  for k = 1:n
    offset = 3 * (k - 1);
    ids = edges(k, :);
    xData(offset + (1:2)) = x(ids);
    yData(offset + (1:2)) = y(ids);
  end
end
%=============================================================================
