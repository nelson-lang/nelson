%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = trisurf(varargin)
  nargoutchk(0, 1);
  narginchk(1, inf);
  inputArguments = convertStringToCharArgs(varargin);
  [go, tri, x, y, z, c, propertiesList] = parseTrisurfInputs(inputArguments{:});
  if isempty(go)
    go = graphicsParentProperty(propertiesList);
  end
  go = newplotTarget(go);
  previousNextPlot = go.NextPlot;
  go.NextPlot = 'add';
  vertices = [x(:), y(:), z(:)];
  faceValues = [];
  if isempty(c)
    faceValues = faceColorValues(tri, z);
  else
    faceValues = faceColorValues(tri, c);
  end
  if ~hasProperty(propertiesList, 'EdgeColor')
    propertiesList = [propertiesList, {'EdgeColor', go.XColor}];
  end
  if ~hasProperty(propertiesList, 'FaceVertexCData')
    propertiesList = [propertiesList, {'FaceVertexCData', mapFaceValuesToColors(go, faceValues)}];
  end
  if ~hasProperty(propertiesList, 'FaceColor')
    propertiesList = [propertiesList, {'FaceColor', 'flat'}];
  end
  h = patch(go, 'Faces', tri, 'Vertices', vertices, propertiesList{:});
  go.NextPlot = previousNextPlot;
  if strcmp(previousNextPlot, 'replaceall') || strcmp(previousNextPlot, 'replace')
    view(go, 3);
    grid(go, 'on');
  elseif strcmp(previousNextPlot, 'replacechildren')
    view(go, 3);
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function [go, tri, x, y, z, c, propertiesList] = parseTrisurfInputs(varargin)
  go = [];
  c = [];
  propertiesList = {};
  if isscalar(varargin{1}) && isgraphics(varargin{1}, 'axes')
    go = varargin{1};
    varargin = varargin(2:end);
  end
  if isTriangulationObject(varargin{1})
    obj = varargin{1};
    points = obj.Points;
    tri = obj.ConnectivityList;
    if size(points, 2) ~= 3
      error('Nelson:trisurf:threeDimensionalTriangulationExpected', 'Three-dimensional triangulation expected.');
    end
    x = points(:, 1);
    y = points(:, 2);
    z = points(:, 3);
    varargin = varargin(2:end);
  else
    if length(varargin) < 4
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
    end
    tri = double(varargin{1});
    x = varargin{2};
    y = varargin{3};
    z = varargin{4};
    varargin = varargin(5:end);
  end
  if ~isempty(varargin)
    if ~isText(varargin{1}) || isempty(canonicalPatchPropertyName(varargin{1}))
      c = varargin{1};
      varargin = varargin(2:end);
    end
  end
  propertiesList = canonicalizePatchPropertyPairs(varargin);
  validateTriangulation(tri, x, y, z);
  % Color data is one scalar value per vertex; it is flattened and mapped to
  % faces by indexing, so its shape (row, column or a matrix matching z) does
  % not matter, only that it has one entry per vertex.
  if ~isempty(c) && numel(c) ~= numel(z)
    error('Nelson:dimagree:colorDataSizeZ', 'Color data must have the same size as Z.');
  end
end
%=============================================================================
function propertyPairs = canonicalizePatchPropertyPairs(propertyPairs)
  if mod(length(propertyPairs), 2) ~= 0
    error('Nelson:graphics:nameValuePair', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(propertyPairs)
    canonicalName = canonicalPatchPropertyName(propertyPairs{k});
    if isempty(canonicalName)
      error('Nelson:graphics:invalidPropertyName', 'Invalid property name.');
    end
    propertyPairs{k} = canonicalName;
  end
end
%=============================================================================
function canonicalName = canonicalPatchPropertyName(name)
  canonicalName = '';
  if ~isText(name)
    return
  end
  names = {'AlphaDataMapping', 'AmbientStrength', 'BackFaceLighting', 'BeingDeleted', ...
    'BusyAction', 'CData', 'CDataMapping', 'Children', 'CreateFcn', 'DeleteFcn', ...
    'DiffuseStrength', 'DisplayName', 'EdgeAlpha', 'EdgeColor', 'EdgeLighting', ...
    'FaceAlpha', 'FaceColor', 'FaceLighting', 'Faces', 'FaceVertexCData', ...
    'Interruptible', 'LineStyle', 'LineWidth', 'Marker', 'MarkerEdgeColor', ...
    'MarkerFaceColor', 'MarkerSize', 'Parent', 'SpecularColorReflectance', ...
    'SpecularExponent', 'SpecularStrength', 'Tag', 'Type', 'UserData', ...
    'VertexNormals', 'Vertices', 'Visible', 'XData', 'XDataMode', 'YData', ...
    'YDataMode', 'ZData'};
  matches = {};
  for k = 1:length(names)
    if startsWith(lower(names{k}), lower(name))
      matches{end + 1} = names{k};
    end
  end
  if length(matches) == 1
    canonicalName = matches{1};
  elseif length(matches) > 1
    error('Nelson:trisurf:ambiguousPatchPropertyName', 'Ambiguous patch property name.');
  end
end
%=============================================================================
function tf = isTriangulationObject(value)
  tf = isa(value, 'triangulation') || isa(value, 'delaunayTriangulation');
end
%=============================================================================
function validateTriangulation(tri, x, y, z)
  if size(tri, 2) ~= 3
    error('Nelson:graphics:invalidTriangleConnectivity', 'Triangle connectivity must have three columns.');
  end
  if ~isequal(size(x), size(y)) || ~isequal(size(x), size(z))
    error('Nelson:dimagree:coordinateArraysSize', 'Coordinate arrays must have the same size.');
  end
  x = x(:);
  if any(tri(:) < 1) || any(tri(:) > length(x)) || any(tri(:) ~= fix(tri(:)))
    error('Nelson:graphics:triangleConnectivityContainsInvalid', 'Triangle connectivity contains invalid point indices.');
  end
end
%=============================================================================
function values = faceColorValues(tri, data)
  data = data(:);
  values = mean(data(tri), 2);
end
%=============================================================================
function colors = mapFaceValuesToColors(ax, scalars)
  scalars = scalars(:);
  colors = colormap(ax);
  if isempty(colors)
    colors = colormap();
  end
  if isempty(colors)
    colors = jet(64);
  end
  minScalar = min(scalars);
  maxScalar = max(scalars);
  if minScalar == maxScalar
    maxScalar = minScalar + 1;
  end
  ax.CLim = [minScalar, maxScalar];
  nbColors = size(colors, 1);
  indices = ceil((scalars - minScalar) ./ (maxScalar - minScalar) .* nbColors);
  indices(indices < 1) = 1;
  indices(indices > nbColors) = nbColors;
  colors = colors(indices, :);
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
