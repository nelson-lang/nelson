%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fpolarplot(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseTargetPolarAxes(args);
  [fun, args] = parseFunction(args);
  [lineProperties, args] = parseLineSpec(args);
  [thetaLimits, rangeSet, args] = parseThetaLimits(args);
  [moreLineProperties, args] = parseLineSpec(args);
  lineProperties = [lineProperties, moreLineProperties];
  [meshDensity, args] = parseMeshDensity(args);
  [moreLineProperties, args] = parseLineSpec(args);
  lineProperties = [lineProperties, moreLineProperties];
  validateObjectProperties(args);
  theta = linspace(thetaLimits(1), thetaLimits(2), numberOfPoints(meshDensity, fun));
  radius = evaluateFunction(fun, theta);
  rangeProperties = {};
  if rangeSet
    rangeProperties = {'XRange', thetaLimits};
  end
  ax = preparePolarAxes(ax);
  lineProperties = applyDefaultPolarFunctionColor(ax, lineProperties, args);
  h = __functionline__('Parent', ax, 'Function', fun, ...
    rangeProperties{:}, 'MeshDensity', meshDensity, ...
    'XData', [], 'YData', [], 'ThetaData', theta, 'RData', radius, ...
    lineProperties{:}, args{:});
  state = polarGetState(ax, false);
  state.DataHandles = [state.DataHandles; h(:)];
  polarSetState(ax, state);
  polarRefresh(ax, state, false);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [ax, args] = parseTargetPolarAxes(args)
  ax = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    error('Nelson:graphics:invalidParentInput', 'Parent input must be a polaraxes object.');
  end
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'polaraxes')
    ax = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [fun, args] = parseFunction(args)
  if isempty(args)
    error('Nelson:graphics:functionExpected', 'Function expected.');
  end
  fun = args{1};
  args = args(2:end);
  if ~(isa(fun, 'function_handle') || isConstantFunction(fun))
    error('Nelson:graphics:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
end
%=============================================================================
function tf = isConstantFunction(value)
  tf = isnumeric(value) && isscalar(value) && isreal(value);
end
%=============================================================================
function [limits, rangeSet, args] = parseThetaLimits(args)
  limits = [0 2 * pi];
  rangeSet = false;
  if isempty(args)
    return
  end
  if isnumeric(args{1})
    if ~isreal(args{1}) || ~isequal(size(args{1}), [1 2]) || ...
        any(~isfinite(args{1})) || args{1}(1) >= args{1}(2)
      error('Nelson:fplot:invalidRangeValue', 'Range value must be an increasing real finite 2 element row vector.');
    end
    limits = double(args{1});
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
    lineProperties = [lineProperties, {'Color', color, 'MarkerEdgeColor', color}];
  end
  if ~isempty(marker)
    lineProperties = [lineProperties, {'Marker', marker}];
  end
end
%=============================================================================
function [meshDensity, args] = parseMeshDensity(args)
  meshDensity = 23;
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
    if ~isFpolarplotPropertyName(args{k})
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(args{k}));
    end
  end
end
%=============================================================================
function tf = isFpolarplotPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  name = char(value);
  if isValidGraphicsProperty('line', name)
    tf = true;
    return
  end
  names = {'Function', 'MeshDensity', 'XRange', 'XRangeMode'};
  tf = any(strcmpi(name, names));
end
%=============================================================================
function lineProperties = applyDefaultPolarFunctionColor(ax, lineProperties, objectProperties)
  if ~hasPropertyName(lineProperties, 'Color') && ~hasPropertyName(objectProperties, 'Color')
    color = getColorAndUpdateIndex(ax);
    lineProperties = [lineProperties, {'Color', color, 'MarkerEdgeColor', color}];
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
function n = numberOfPoints(meshDensity, fun)
  if isConstantFunction(fun)
    n = max(2, 29 * meshDensity - 8);
  else
    n = max(2, 35 * meshDensity - 2);
  end
end
%=============================================================================
function values = evaluateFunction(fun, theta)
  if isConstantFunction(fun)
    values = fun + zeros(size(theta));
    return
  end
  try
    values = feval(fun, theta);
  catch
    values = scalarEvaluate(fun, theta);
  end
  if ~isnumeric(values) || ~isreal(values)
    error('Nelson:graphics:invalidFunctionOutput', 'Function must return real numeric values.');
  end
  if isscalar(values)
    values = values + zeros(size(theta));
  end
  if numel(values) ~= numel(theta)
    values = scalarEvaluate(fun, theta);
  end
  values = values(:)';
end
%=============================================================================
function values = scalarEvaluate(fun, theta)
  values = zeros(size(theta));
  for k = 1:numel(theta)
    values(k) = feval(fun, theta(k));
  end
end
%=============================================================================
function ax = preparePolarAxes(ax)
  if isempty(ax)
    ax = polaraxes();
    return
  end
  newplot(ax);
  polarSetState(ax, polarDefaultState());
end
%=============================================================================
