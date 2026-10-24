%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fmesh(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseParent(args);
  [functions, args] = parseFunctions(args);
  [range, rangeSet, args] = parseRange(args);
  [meshDensity, args] = parseMeshDensity(args);
  validateObjectProperties(args);
  if isempty(ax)
    ax = newplot();
  else
    ax = newplot(ax);
  end
  [xData, yData, zData] = sampleMesh(functions, range, meshDensity);
  rangeProperties = {};
  if rangeSet
    rangeProperties = {'XRange', range(1:2), 'YRange', range(3:4)};
  end
  h = __fsurf__('Parent', ax, 'Function', functions{end}, rangeProperties{:}, ...
    'MeshDensity', meshDensity, 'XData', xData, 'YData', yData, 'ZData', zData, ...
    'FaceColor', [1 1 1], 'EdgeColor', 'interp', args{:});
  view(ax, 3);
  grid(ax, 'on');
  if range(1) < range(2)
    xlim(ax, range(1:2));
  end
  if range(3) < range(4)
    ylim(ax, range(3:4));
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [ax, args] = parseParent(args)
  ax = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  end
  k = 1;
  while k <= numel(args) - 1
    if ~(ischar(args{k}) || isStringScalar(args{k}))
      k = k + 1;
      continue
    end
    if strcmpi(char(args{k}), 'Parent')
      parentValue = args{k + 1};
      if ~(isscalar(parentValue) && isgraphics(parentValue, 'axes'))
        error('Nelson:graphics:invalidAxesParent', 'Parent must be an axes handle.');
      end
      ax = parentValue;
      args(k:k + 1) = [];
      continue
    end
    k = k + 2;
  end
end
%=============================================================================
function [functions, args] = parseFunctions(args)
  if isempty(args)
    error('Nelson:graphics:functionExpected', 'Function expected.');
  end
  if isnumeric(args{1}) && isscalar(args{1}) && isreal(args{1})
    functions = args(1);
    args = args(2:end);
    return
  end
  if ~isa(args{1}, 'function_handle')
    error('Nelson:graphics:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  if numel(args) >= 3 && isa(args{2}, 'function_handle') && isa(args{3}, 'function_handle')
    functions = args(1:3);
    args = args(4:end);
    return
  end
  if numel(args) >= 2 && isa(args{2}, 'function_handle')
    error('Nelson:graphics:tooManyFunctions', 'Too many functions.');
  end
  functions = args(1);
  args = args(2:end);
end
%=============================================================================
function [range, rangeSet, args] = parseRange(args)
  range = [-5 5 -5 5];
  rangeSet = false;
  if isempty(args)
    return
  end
  if isnumeric(args{1}) && isreal(args{1}) && isvector(args{1})
    value = double(args{1}(:)');
    if numel(value) == 2
      range = [value(1), value(2), value(1), value(2)];
    elseif numel(value) == 4
      range = value;
    else
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', mat2str(args{1}));
    end
    if range(1) > range(2) || range(3) > range(4) || any(~isfinite(range))
      error('Nelson:graphics:rangeNotIncreasing', 'Range values must be increasing and finite.');
    end
    rangeSet = true;
    args = args(2:end);
  end
end
%=============================================================================
function validateObjectProperties(args)
  if isempty(args)
    return
  end
  if mod(numel(args), 2) ~= 0
    if (ischar(args{1}) || isStringScalar(args{1})) && ~isFmeshPropertyName(args{1})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(args{1}));
    end
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(args)
    if ~isFmeshPropertyName(args{k})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(args{k}));
    end
  end
end
%=============================================================================
function tf = isFmeshPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  name = lower(char(value));
  names = {'alignvertexcenters', 'alphadata', 'alphadatamapping', 'ambientstrength', ...
    'annotation', 'backfacelighting', 'beingdeleted', 'busyaction', 'buttondownfcn', ...
    'cdata', 'cdatamapping', 'cdatamode', 'cdatasource', 'children', 'clipping', ...
    'contextmenu', 'createfcn', 'datatiptemplate', 'deletefcn', 'diffusestrength', ...
    'displayname', 'edgealpha', 'edgecolor', 'edgelighting', 'facealpha', ...
    'facecolor', 'facelighting', 'facenormals', 'facenormalsmode', 'function', ...
    'handlevisibility', 'hittest', 'interpolation', 'interruptible', 'linestyle', ...
    'linewidth', 'marker', 'markeredgecolor', 'markerfacecolor', 'markersize', ...
    'maxrenderedresolution', 'meshdensity', 'meshstyle', 'parent', 'pickableparts', ...
    'selected', 'selectionhighlight', 'showcontours', 'specularcolorreflectance', ...
    'specularexponent', 'specularstrength', 'tag', 'type', 'userdata', ...
    'vertexnormals', 'vertexnormalsmode', 'visible', 'xdata', 'xdatamode', ...
    'xdatasource', 'xrange', 'xrangemode', 'ydata', 'ydatamode', 'ydatasource', ...
    'yrange', 'yrangemode', 'zdata', 'zdatasource'};
  tf = any(strcmp(name, names));
end
%=============================================================================
function [meshDensity, args] = parseMeshDensity(args)
  meshDensity = 35;
  k = 1;
  while k <= length(args) - 1
    if ~(ischar(args{k}) || isStringScalar(args{k}))
      break
    end
    name = lower(char(args{k}));
    if strcmp(name, 'meshdensity')
      meshDensity = args{k + 1};
      args(k:k + 1) = [];
    else
      k = k + 2;
    end
  end
  if ~isnumeric(meshDensity) || ~isscalar(meshDensity) || meshDensity < 2 || meshDensity ~= fix(meshDensity)
    error('Nelson:graphics:invalidMeshDensity', 'MeshDensity must be a positive integer greater than one.');
  end
end
%=============================================================================
function [xData, yData, zData] = sampleMesh(functions, range, meshDensity)
  u = linspace(range(1), range(2), meshDensity);
  v = linspace(range(3), range(4), meshDensity);
  [uData, vData] = meshgrid(u, v);
  if numel(functions) == 1
    xData = uData;
    yData = vData;
    zData = evaluateFunction(functions{1}, uData, vData);
  else
    xData = evaluateFunction(functions{1}, uData, vData);
    yData = evaluateFunction(functions{2}, uData, vData);
    zData = evaluateFunction(functions{3}, uData, vData);
  end
end
%=============================================================================
function values = evaluateFunction(fun, xData, yData)
  if isnumeric(fun) && isscalar(fun) && isreal(fun)
    values = fun + zeros(size(xData));
    return
  end
  try
    values = feval(fun, xData, yData);
  catch
    values = scalarEvaluate(fun, xData, yData);
  end
  if isscalar(values)
    values = values + zeros(size(xData));
  end
  if ~isnumeric(values) || ~isreal(values)
    error('Nelson:graphics:invalidFunctionOutput', 'Function must return real numeric values.');
  end
  if ~isequal(size(values), size(xData))
    values = scalarEvaluate(fun, xData, yData);
  end
end
%=============================================================================
function values = scalarEvaluate(fun, xData, yData)
  values = zeros(size(xData));
  for k = 1:numel(xData)
    values(k) = feval(fun, xData(k), yData(k));
  end
end
%=============================================================================
