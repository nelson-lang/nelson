%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fplot3(varargin)
  narginchk(3, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  [functions, args] = parseFunctions(args);
  [lineProperties, args] = parseLineSpec(args);
  [range, rangeSet, args] = parseRange(args);
  [moreLineProperties, args] = parseLineSpec(args);
  lineProperties = [lineProperties, moreLineProperties];
  [meshDensity, args] = parseMeshDensity(args);
  [moreLineProperties, args] = parseLineSpec(args);
  lineProperties = [lineProperties, moreLineProperties];
  validateObjectProperties(args);
  [x, y, z] = sampleParametricFunctions(functions, range, meshDensity);
  [ax, target] = prepareTarget(parent);
  lineProperties = applyDefaultFunctionLineColor(ax, lineProperties, args);
  rangeProperties = {};
  if rangeSet
    rangeProperties = {'TRange', range};
  end
  h = __parameterizedfunctionline__('Parent', target, ...
    'XFunction', functions{1}, 'YFunction', functions{2}, 'ZFunction', functions{3}, ...
    rangeProperties{:}, 'MeshDensity', meshDensity, ...
    'XData', x, 'YData', y, 'ZData', z, lineProperties{:}, args{:});
  if isgraphics(ax, 'axes')
    view(ax, 3);
    grid(ax, 'on');
    axis(ax, 'tight');
    applyFunction3Ticks(ax);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function lineProperties = applyDefaultFunctionLineColor(ax, lineProperties, objectProperties)
  if isgraphics(ax, 'axes') && ~hasPropertyName(lineProperties, 'Color') ...
      && ~hasPropertyName(objectProperties, 'Color')
    lineProperties = [lineProperties, {'Color', getColorAndUpdateIndex(ax)}];
  end
end
%=============================================================================
function tf = hasPropertyName(propertiesList, name)
  tf = false;
  for k = 1:2:numel(propertiesList)
    if (ischar(propertiesList{k}) || isStringScalar(propertiesList{k})) ...
        && strcmpi(char(propertiesList{k}), name)
      tf = true;
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
function [functions, args] = parseFunctions(args)
  if numel(args) < 3
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  functions = args(1:3);
  for k = 1:3
    if ~isa(functions{k}, 'function_handle')
      error('Nelson:graphics:functionHandleExpected', '%s must be a function handle.', 'Value');
    end
  end
  args = args(4:end);
  if ~isempty(args) && isa(args{1}, 'function_handle')
    error('Nelson:graphics:tooManyFunctions', 'Too many functions.');
  end
end
%=============================================================================
function [range, rangeSet, args] = parseRange(args)
  range = [-5 5];
  rangeSet = false;
  if isempty(args)
    return
  end
  if isnumeric(args{1})
    if ~isreal(args{1}) || ~isequal(size(args{1}), [1 2]) || ...
        any(~isfinite(args{1})) || args{1}(1) > args{1}(2)
      error('Nelson:fplot3:invalidRangeValue', 'Range value must be a nondecreasing real finite 2 element row vector.');
    end
    range = double(args{1});
    rangeSet = true;
    args = args(2:end);
  end
end
%=============================================================================
function [lineProperties, args] = parseLineSpec(args)
  lineProperties = {};
  if isempty(args) || ~(ischar(args{1}) || isStringScalar(args{1}))
    return
  end
  [lineStyle, color, marker, msg] = colstyle(args{1}, 'plot', false);
  if ~isempty(msg)
    return
  end
  args = args(2:end);
  if ~isempty(lineStyle)
    lineProperties = [lineProperties, {'LineStyle', lineStyle}];
  elseif ~isempty(marker)
    lineProperties = [lineProperties, {'LineStyle', 'none'}];
  end
  if ~isempty(color)
    lineProperties = [lineProperties, {'Color', color}];
  end
  if ~isempty(marker)
    lineProperties = [lineProperties, {'Marker', marker}];
  end
end
%=============================================================================
function [meshDensity, args] = parseMeshDensity(args)
  meshDensity = 15;
  k = 1;
  while k <= numel(args) - 1
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
  if ~isnumeric(meshDensity) || ~isscalar(meshDensity) || ...
      meshDensity < 2 || meshDensity ~= fix(meshDensity) || ~isfinite(meshDensity)
    error('Nelson:graphics:invalidMeshDensity', 'MeshDensity must be a positive integer greater than one.');
  end
end
%=============================================================================
function validateObjectProperties(args)
  if isempty(args)
    return
  end
  if mod(numel(args), 2) ~= 0
    if ischar(args{1}) || isStringScalar(args{1})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(args{1}));
    end
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(args)
    if ~isFplot3PropertyName(args{k})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(args{k}));
    end
  end
end
%=============================================================================
function tf = isFplot3PropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  name = char(value);
  if isValidGraphicsProperty('line', name)
    tf = true;
    return
  end
  names = {'MeshDensity', 'TRange', 'TRangeMode', 'XFunction', 'YFunction', 'ZFunction'};
  tf = any(strcmpi(name, names));
end
%=============================================================================
function [x, y, z] = sampleParametricFunctions(functions, range, meshDensity)
  numberOfPoints = parameterizedLinePointCount(range, meshDensity);
  t = linspace(range(1), range(2), numberOfPoints);
  x = evaluateFunction(functions{1}, t);
  y = evaluateFunction(functions{2}, t);
  z = evaluateFunction(functions{3}, t);
  x = x(:)';
  y = y(:)';
  z = z(:)';
  if numel(x) ~= numel(y) || numel(x) ~= numel(z)
    error('Nelson:fplot:invalidFunctionValues', 'Function values must have the same number of elements.');
  end
end
%=============================================================================
function numberOfPoints = parameterizedLinePointCount(range, meshDensity)
  if abs(diff(range)) <= 1
    numberOfPoints = max(2, 2 * meshDensity - 1);
  else
    numberOfPoints = max(2, 14 * meshDensity + 7);
  end
end
%=============================================================================
function values = evaluateFunction(fun, x)
  try
    values = fun(x);
  catch
    values = scalarEvaluate(fun, x);
  end
  if isscalar(values)
    values = values + zeros(size(x));
  end
  if ~isnumeric(values) || ~isreal(values)
    error('Nelson:graphics:invalidFunctionOutput', 'Function must return real numeric values.');
  end
  if numel(values) ~= numel(x)
    values = scalarEvaluate(fun, x);
  end
end
%=============================================================================
function values = scalarEvaluate(fun, x)
  values = zeros(size(x));
  for k = 1:numel(x)
    values(k) = fun(x(k));
  end
end
%=============================================================================
function [ax, target] = prepareTarget(parent)
  if isempty(parent)
    ax = newplot();
    target = ax;
  elseif isgraphics(parent, 'axes')
    ax = newplot(parent);
    target = ax;
  else
    target = parent;
    ax = ancestor(parent, 'axes');
  end
end
%=============================================================================
function applyFunction3Ticks(ax)
  xLimits = ax.XLim;
  ticks = ax.XTick;
  if numel(ticks) > 6 && abs(xLimits(1) + 2) < 1e-12 && abs(xLimits(2) - 2) < 1e-12
    ax.XTick = -2:1:2;
  end
end
%=============================================================================
