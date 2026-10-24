%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fimplicit3(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  if isempty(args) || ~isa(args{1}, 'function_handle')
    error('Nelson:graphics:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  fun = args{1};
  args = args(2:end);
  [range, rangeSet, meshDensity, styleProperties, objectProperties] = parseFimplicit3Options(args);
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'axes')
    ax = newplot(parent);
  else
    ax = ancestor(parent, 'axes');
  end
  effectiveMeshDensity = max(2, ceil(sqrt(2) * meshDensity));
  [faces, vertices, edgeXData, edgeYData, edgeZData] = ...
    __fimplicit3_mesh__(fun, range, effectiveMeshDensity);
  rangeProperties = {};
  if rangeSet
    rangeProperties = {'XRange', range(1:2), 'YRange', range(3:4), 'ZRange', range(5:6)};
  end
  defaultColorProperties = fimplicit3DefaultColorProperties(styleProperties, objectProperties, vertices);
  h = __fimplicit3__('Parent', ax, 'Function', fun, rangeProperties{:}, ...
    'MeshDensity', meshDensity, 'Faces', faces, 'Vertices', vertices, ...
    defaultColorProperties{:}, styleProperties{:}, objectProperties{:});
  edgeLineProperties = fimplicit3EdgeLineProperties(styleProperties, objectProperties);
  if ~isempty(edgeXData) && fimplicit3DrawEdgeLine(edgeLineProperties)
    line(edgeXData, edgeYData, edgeZData, 'Parent', ax, edgeLineProperties{:});
  end
  view(ax, 3);
  grid(ax, 'on');
  axis(ax, 'tight');
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function properties = fimplicit3DefaultColorProperties(styleProperties, objectProperties, vertices)
  properties = {};
  if hasFimplicit3Property(objectProperties, 'FaceVertexCData')
    return
  end
  faceColor = fimplicit3FaceColor(styleProperties, objectProperties);
  if isempty(faceColor)
    properties = {'FaceColor', 'interp', 'FaceVertexCData', vertices(:, 3)};
  elseif (ischar(faceColor) || isStringScalar(faceColor)) && strcmpi(char(faceColor), 'interp')
    properties = {'FaceVertexCData', vertices(:, 3)};
  end
end
%=============================================================================
function value = fimplicit3FaceColor(styleProperties, objectProperties)
  value = fimplicit3PropertyValue(objectProperties, 'FaceColor');
  if isempty(value)
    value = fimplicit3PropertyValue(styleProperties, 'FaceColor');
  end
end
%=============================================================================
function value = fimplicit3PropertyValue(properties, name)
  value = [];
  for k = 1:2:numel(properties)
    if strcmpi(char(properties{k}), name)
      value = properties{k + 1};
      return
    end
  end
end
%=============================================================================
function tf = hasFimplicit3Property(properties, name)
  tf = false;
  for k = 1:2:numel(properties)
    if strcmpi(char(properties{k}), name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function properties = fimplicit3EdgeLineProperties(styleProperties, objectProperties)
  properties = {'Color', [0 0 0], 'LineStyle', '-', 'LineWidth', 0.5, ...
    'Marker', 'none', 'HandleVisibility', 'off'};
  properties = appendFimplicit3EdgeLineProperties(properties, styleProperties);
  for k = 1:2:numel(objectProperties)
    name = char(objectProperties{k});
    value = objectProperties{k + 1};
    if strcmpi(name, 'EdgeColor')
      properties = setFimplicit3LineProperty(properties, 'Color', value);
    elseif any(strcmpi(name, {'LineStyle', 'LineWidth', 'Marker', ...
        'MarkerEdgeColor', 'MarkerFaceColor', 'MarkerSize', 'Visible'}))
      properties = setFimplicit3LineProperty(properties, name, value);
    end
  end
end
%=============================================================================
function properties = appendFimplicit3EdgeLineProperties(properties, styleProperties)
  for k = 1:2:numel(styleProperties)
    name = char(styleProperties{k});
    if strcmpi(name, 'LineStyle') || strcmpi(name, 'Marker')
      properties = setFimplicit3LineProperty(properties, name, styleProperties{k + 1});
    end
  end
end
%=============================================================================
function properties = setFimplicit3LineProperty(properties, name, value)
  for k = 1:2:numel(properties)
    if strcmpi(char(properties{k}), name)
      properties{k + 1} = value;
      return
    end
  end
  properties = [properties, {name, value}];
end
%=============================================================================
function tf = fimplicit3DrawEdgeLine(properties)
  tf = true;
  for k = 1:2:numel(properties)
    name = char(properties{k});
    value = properties{k + 1};
    if (strcmpi(name, 'Color') || strcmpi(name, 'LineStyle')) ...
        && (ischar(value) || isStringScalar(value)) && strcmpi(char(value), 'none')
      tf = false;
      return
    end
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [range, rangeSet, meshDensity, styleProperties, objectProperties] = parseFimplicit3Options(args)
  range = [-5 5 -5 5 -5 5];
  rangeSet = false;
  meshDensity = 35;
  styleProperties = {};
  lineSpecFound = false;
  while ~isempty(args)
    if ~lineSpecFound && (ischar(args{1}) || isStringScalar(args{1}))
      [lineStyle, color, marker, msg] = colstyle(args{1}, 'plot', false);
      if isempty(msg)
        lineSpecFound = true;
        args(1) = [];
        styleProperties = appendLineSpecProperties(styleProperties, lineStyle, color, marker);
        continue
      end
    end
    if ~rangeSet && isnumeric(args{1}) && isreal(args{1}) ...
        && (numel(args{1}) == 2 || numel(args{1}) == 6)
      range = normalizeRange(args{1});
      rangeSet = true;
      args(1) = [];
      continue
    end
    break
  end
  objectProperties = args;
  if mod(numel(objectProperties), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(objectProperties)
    if ~isFimplicit3PropertyName(objectProperties{k})
      error('Nelson:graphics:invalidPropertyName', 'Invalid property name.');
    end
    if strcmpi(char(objectProperties{k}), 'MeshDensity')
      meshDensity = objectProperties{k + 1};
    end
  end
  validateMeshDensity(meshDensity);
end
%=============================================================================
function styleProperties = appendLineSpecProperties(styleProperties, lineStyle, color, marker)
  if ~isempty(lineStyle)
    styleProperties = [styleProperties, {'LineStyle', lineStyle}];
  end
  if ~isempty(color)
    styleProperties = [styleProperties, {'FaceColor', color}];
  end
  if ~isempty(marker)
    styleProperties = [styleProperties, {'Marker', marker}];
  end
end
%=============================================================================
function range = normalizeRange(value)
  value = double(value(:)');
  if numel(value) == 2
    range = [value(1), value(2), value(1), value(2), value(1), value(2)];
  elseif numel(value) == 6
    range = value;
  else
    error('Nelson:fimplicit3:invalidRange:rangeTwoSixElements', 'Range must have two or six elements.');
  end
  if range(1) >= range(2) || range(3) >= range(4) || range(5) >= range(6) || any(~isfinite(range))
    error('Nelson:graphics:rangeNotIncreasing', 'Range values must be increasing and finite.');
  end
end
%=============================================================================
function validateMeshDensity(meshDensity)
  if ~isnumeric(meshDensity) || ~isscalar(meshDensity) || meshDensity < 2 || meshDensity ~= fix(meshDensity)
    error('Nelson:graphics:invalidMeshDensity', 'MeshDensity must be a positive integer greater than one.');
  end
end
%=============================================================================
function tf = isFimplicit3PropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  name = lower(char(value));
  names = {'alphadata', 'alphadatamapping', 'ambientstrength', 'annotation', ...
    'backfacelighting', 'beingdeleted', 'busyaction', 'buttondownfcn', 'cdata', ...
    'cdatamapping', 'children', 'clipping', 'contextmenu', 'createfcn', ...
    'deletefcn', 'diffusestrength', 'displayname', 'edgealpha', 'edgecolor', ...
    'edgelighting', 'facealpha', 'facecolor', 'facelighting', 'function', ...
    'facenormals', 'facenormalsmode', 'faces', 'facevertexalphadata', ...
    'facevertexcdata', 'facevertexcdatamode', 'handlevisibility', 'hittest', ...
    'interruptible', 'linejoin', 'linestyle', 'linewidth', 'marker', ...
    'markeredgecolor', 'markerfacecolor', 'markersize', 'meshdensity', ...
    'parent', 'pickableparts', 'selected', 'selectionhighlight', 'seriesindex', ...
    'specularcolorreflectance', 'specularexponent', 'specularstrength', 'tag', ...
    'userdata', 'vertexnormals', 'vertexnormalsmode', 'vertices', 'visible', ...
    'xdata', 'xrange', 'xrangemode', 'ydata', 'yrange', 'yrangemode', 'zdata', ...
    'zrange', 'zrangemode'};
  tf = any(strcmp(name, names));
end
%=============================================================================
