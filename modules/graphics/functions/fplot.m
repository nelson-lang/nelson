%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fplot(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 2);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  [functions, args] = parseFunctions(args);
  [lineProperties, args] = parseLineSpec(args);
  [range, rangeSet, args] = parseRange(args);
  [moreLineProperties, args] = parseLineSpec(args);
  lineProperties = [lineProperties, moreLineProperties];
  [meshDensity, args] = parseMeshDensity(args, numel(functions));
  [moreLineProperties, args] = parseLineSpec(args);
  lineProperties = [lineProperties, moreLineProperties];
  validateObjectProperties(args, numel(functions) == 2);
  outputOnly = nargout == 2;
  [xData, yData] = sampleFunctions(functions, range, meshDensity, outputOnly);
  if nargout == 2
    varargout{1} = xData;
    varargout{2} = yData;
    return
  end
  [ax, target] = prepareTarget(parent);
  lineProperties = applyDefaultFunctionLineColor(ax, lineProperties, args);
  if numel(functions) == 1
    rangeProperties = {};
    if rangeSet
      rangeProperties = {'XRange', range};
    end
    h = __functionline__('Parent', target, 'Function', functions{1}, ...
      rangeProperties{:}, 'MeshDensity', meshDensity, ...
      'XData', xData, 'YData', yData, lineProperties{:}, args{:});
  else
    rangeProperties = {};
    if rangeSet
      rangeProperties = {'TRange', range};
    end
    h = __parameterizedfunctionline__('Parent', target, ...
      'XFunction', functions{1}, 'YFunction', functions{2}, ...
      rangeProperties{:}, 'MeshDensity', meshDensity, ...
      'XData', xData, 'YData', yData, lineProperties{:}, args{:});
  end
  if isgraphics(ax, 'axes')
    ax.Box = 'on';
    axis(ax, 'tight');
    if numel(functions) == 1 && range(1) < range(2)
      xlim(ax, range);
    end
    applyFunctionLineAxes(ax, yData, numel(functions) == 1);
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
  if isempty(args)
    error('Nelson:graphics:functionExpected', 'Function expected.');
  end
  if isConstantFunction(args{1})
    functions = args(1);
    args = args(2:end);
    return
  end
  if ~isa(args{1}, 'function_handle')
    error('Nelson:graphics:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  functions = args(1);
  args = args(2:end);
  if ~isempty(args) && isa(args{1}, 'function_handle')
    functions{2} = args{1};
    args = args(2:end);
  end
  if ~isempty(args) && isa(args{1}, 'function_handle')
    error('Nelson:graphics:tooManyFunctions', 'Too many functions.');
  end
end
%=============================================================================
function tf = isConstantFunction(value)
  tf = isnumeric(value) && isscalar(value) && isreal(value);
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
        any(~isfinite(args{1})) || args{1}(1) >= args{1}(2)
      error('Nelson:fplot:invalidRangeValue', 'Range value must be an increasing real finite 2 element row vector.');
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
function [meshDensity, args] = parseMeshDensity(args, numberOfFunctions)
  if numberOfFunctions == 1
    meshDensity = 23;
  else
    meshDensity = 15;
  end
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
function validateObjectProperties(args, isParameterized)
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
    if ~isFplotPropertyName(args{k}, isParameterized)
      error('Nelson:graphics:invalidParameter', 'Invalid parameter ''%s''.', char(args{k}));
    end
  end
end
%=============================================================================
function tf = isFplotPropertyName(value, isParameterized)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  name = char(value);
  if isValidGraphicsProperty('line', name)
    tf = true;
    return
  end
  if isParameterized
    names = {'MeshDensity', 'TRange', 'TRangeMode', 'XFunction', 'YFunction'};
  else
    names = {'Function', 'MeshDensity', 'XRange', 'XRangeMode'};
  end
  tf = any(strcmpi(name, names));
end
%=============================================================================
function [xData, yData] = sampleFunctions(functions, range, meshDensity, outputOnly)
  if numel(functions) == 1
    numberOfPoints = functionLinePointCount(functions{1}, range, meshDensity, outputOnly);
    xData = linspace(range(1), range(2), numberOfPoints);
    yData = evaluateFunction(functions{1}, xData);
  else
    [xData, yData] = sampleParametric(functions{1}, functions{2}, range, meshDensity);
  end
  xData = xData(:)';
  yData = yData(:)';
  if numel(xData) ~= numel(yData)
    error('Nelson:fplot:invalidFunctionValues', 'Function values must have the same number of elements.');
  end
end
%=============================================================================
function [xData, yData] = sampleParametric(xFunction, yFunction, range, meshDensity)
  % Sample a parametric curve with adaptive refinement: start from a coarse
  % uniform grid in the parameter, then repeatedly bisect the parameter
  % intervals whose midpoint deviates from the straight chord (measured in
  % normalised data coordinates) by more than a tolerance. This concentrates
  % vertices where the curve bends and leaves straight stretches sparse, rather
  % than sampling the parameter uniformly.
  numberOfPoints = max(2, meshDensity + 1);
  tData = linspace(range(1), range(2), numberOfPoints);
  xData = evaluateFunction(xFunction, tData);
  yData = evaluateFunction(yFunction, tData);
  tData = tData(:)';
  xData = xData(:)';
  yData = yData(:)';
  tolerance = 1e-4;
  maxPoints = max(numberOfPoints, 28 * meshDensity + 15);
  for depth = 1:16
    if numel(tData) < 2 || numel(tData) >= maxPoints
      break
    end
    xSpan = max(xData) - min(xData);
    ySpan = max(yData) - min(yData);
    if xSpan == 0, xSpan = 1; end
    if ySpan == 0, ySpan = 1; end
    tMid = 0.5 * (tData(1:end - 1) + tData(2:end));
    xMid = evaluateFunction(xFunction, tMid);
    yMid = evaluateFunction(yFunction, tMid);
    xMid = xMid(:)';
    yMid = yMid(:)';
    xChord = 0.5 * (xData(1:end - 1) + xData(2:end));
    yChord = 0.5 * (yData(1:end - 1) + yData(2:end));
    deviation = sqrt(((xMid - xChord) / xSpan) .^ 2 + ((yMid - yChord) / ySpan) .^ 2);
    refine = deviation > tolerance;
    if ~any(refine)
      break
    end
    [tData, xData, yData] = insertMidpoints(tData, xData, yData, tMid, xMid, yMid, refine);
  end
end
%=============================================================================
function [tOut, xOut, yOut] = insertMidpoints(tData, xData, yData, tMid, xMid, yMid, refine)
  numberOfSegments = numel(tData) - 1;
  numberOfPointsOut = numel(tData) + sum(refine);
  tOut = zeros(1, numberOfPointsOut);
  xOut = zeros(1, numberOfPointsOut);
  yOut = zeros(1, numberOfPointsOut);
  tOut(1) = tData(1);
  xOut(1) = xData(1);
  yOut(1) = yData(1);
  position = 1;
  for k = 1:numberOfSegments
    if refine(k)
      position = position + 1;
      tOut(position) = tMid(k);
      xOut(position) = xMid(k);
      yOut(position) = yMid(k);
    end
    position = position + 1;
    tOut(position) = tData(k + 1);
    xOut(position) = xData(k + 1);
    yOut(position) = yData(k + 1);
  end
end
%=============================================================================
function numberOfPoints = functionLinePointCount(fun, range, meshDensity, outputOnly)
  if outputOnly || isConstantFunction(fun) || abs(diff(range)) <= 1
    numberOfPoints = max(2, 2 * meshDensity - 1);
  else
    numberOfPoints = max(2, 2 * meshDensity + 11);
  end
end
%=============================================================================
function values = evaluateFunction(fun, xData)
  if isConstantFunction(fun)
    values = fun + zeros(size(xData));
    return
  end
  try
    values = feval(fun, xData);
  catch
    values = scalarEvaluate(fun, xData);
  end
  if isscalar(values)
    values = values + zeros(size(xData));
  end
  if ~isnumeric(values) || ~isreal(values)
    error('Nelson:graphics:invalidFunctionOutput', 'Function must return real numeric values.');
  end
  if numel(values) ~= numel(xData)
    values = scalarEvaluate(fun, xData);
  end
end
%=============================================================================
function values = scalarEvaluate(fun, xData)
  values = zeros(size(xData));
  for k = 1:numel(xData)
    values(k) = feval(fun, xData(k));
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
function applyFunctionLineAxes(ax, yData, isSingleFunction)
  ax.ZLim = [-1 1];
  ax.ZTick = [-1 0 1];
  if ~isSingleFunction
    return
  end
  finiteY = yData(isfinite(yData));
  if isempty(finiteY)
    return
  end
  yLimits = ax.YLim;
  ySpan = abs(diff(yLimits));
  if min(finiteY) >= 0 && yLimits(1) > 0 && yLimits(1) <= 1e-3 * max(1, ySpan)
    ticks = ax.YTick;
    ax.YLim = [0 yLimits(2)];
    if ~isempty(ticks) && ticks(1) > 0
      ax.YTick = [0 ticks];
    end
  end
end
%=============================================================================
