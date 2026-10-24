%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = polarplot(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  [ax, inputArguments] = parsePolarAxes(inputArguments);
  [fig, previousDrawLater] = polarBeginDrawLater(ax);
  try
    ax = preparePolarAxes(ax);
    [inputArguments, propertiesList] = splitLineProperties(inputArguments);
    h = gobjects(0, 1);
    showTableLegend = false;
    while ~isempty(inputArguments)
      lineSpec = '';
      if isPolarTableSyntax(inputArguments)
        [theta, radius, lineSpec, thetaVariables, radiusVariables, sourceTable, consumed] = ...
          parsePolarTableArguments(inputArguments);
        tableHandles = plotThetaRadius(ax, theta, radius, lineSpec, propertiesList);
        setPolarTableMetadata(tableHandles, sourceTable, thetaVariables, radiusVariables);
        showTableLegend = showTableLegend || numel(tableHandles) > 1;
        h = [h; tableHandles];
        inputArguments(1:consumed) = [];
      elseif length(inputArguments) == 1
        h = [h; plotRadiusOrComplex(ax, inputArguments{1}, lineSpec, propertiesList)];
        inputArguments(1) = [];
      else
        if ischar(inputArguments{2})
          [ps, cs, ms, msg] = colstyle(inputArguments{2}, 'plot', false);
        else
          msg = 'Invalid LineSpec string.';
        end
        if ischar(inputArguments{2}) && isempty(msg)
          h = [h; plotRadiusOrComplex(ax, inputArguments{1}, inputArguments{2}, propertiesList)];
          inputArguments(1:2) = [];
        elseif length(inputArguments) >= 3 && ischar(inputArguments{3})
          [ps, cs, ms, msg] = colstyle(inputArguments{3}, 'plot', false);
          if isempty(msg)
            lineSpec = inputArguments{3};
            h = [h; plotThetaRadius(ax, inputArguments{1}, inputArguments{2}, lineSpec, propertiesList)];
            inputArguments(1:3) = [];
          else
            h = [h; plotThetaRadius(ax, inputArguments{1}, inputArguments{2}, lineSpec, propertiesList)];
            inputArguments(1:2) = [];
          end
        else
          h = [h; plotThetaRadius(ax, inputArguments{1}, inputArguments{2}, lineSpec, propertiesList)];
          inputArguments(1:2) = [];
        end
      end
    end
    state = polarGetState(ax, false);
    state.DataHandles = [state.DataHandles; h(:)];
    polarSetState(ax, state);
    polarRefresh(ax, state, false);
    if showTableLegend
      legend(ax);
    end
    polarEndDrawLater(fig, previousDrawLater);
  catch exception
    polarEndDrawLater(fig, previousDrawLater);
    error(exception.message);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [ax, args] = parsePolarAxes(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'polaraxes'))
    ax = args{1};
    args(1) = [];
    return
  end
  gr = groot();
  if ~isempty(gr.CurrentFigure)
    ax = gca();
    return
  end
  ax = polaraxes();
end
%=============================================================================
function ax = preparePolarAxes(ax)
  if ~polarIsAxes(ax)
    ax = polarInitializeAxes(ax, false);
  end
  nextPlot = ax.NextPlot;
  state = polarGetState(ax, false);
  if ~strcmp(nextPlot, 'add') && isempty(state.DataHandles) ...
      && numel(allchild(ax)) == numel(state.GridHandles)
    return
  end
  newplot(ax);
  ax = polarInitializeAxes(ax, false);
  if ~strcmp(nextPlot, 'add')
    state = polarGetState(ax, false);
    state.GridHandles = gobjects(0, 1);
    state.DataHandles = gobjects(0, 1);
    polarSetState(ax, state);
  end
end
%=============================================================================
function [args, propertiesList] = splitLineProperties(args)
  nbInputArguments = length(args);
  propertyIndex = 0;
  if nbInputArguments > 2
    propertyIndex = nbInputArguments - 1;
    keepGoing = true;
    while propertyIndex >= 1 && keepGoing
      keepGoing = isLineProperty(args{propertyIndex});
      if keepGoing
        propertyIndex = propertyIndex - 2;
      end
    end
    propertyIndex = propertyIndex + 2;
  end
  propertiesList = {};
  if propertyIndex > 0 && propertyIndex < nbInputArguments
    propertiesList = args(propertyIndex:end);
    args(propertyIndex:end) = [];
  end
end
%=============================================================================
function handles = plotRadiusOrComplex(ax, values, lineSpec, propertiesList)
  if ~isnumeric(values)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if isreal(values)
    radius = values;
    theta = defaultThetaForRadius(radius);
  else
    theta = angle(values);
    radius = abs(values);
  end
  handles = plotThetaRadius(ax, theta, radius, lineSpec, propertiesList);
end
%=============================================================================
function theta = defaultThetaForRadius(radius)
  if isvector(radius)
    n = numel(radius);
    if n <= 1
      theta = zeros(size(radius));
    else
      theta = linspace(0, 2 * pi, n);
      if size(radius, 2) == 1
        theta = theta';
      end
    end
  else
    n = size(radius, 1);
    theta = linspace(0, 2 * pi, n)';
  end
end
%=============================================================================
function handles = plotThetaRadius(ax, theta, radius, lineSpec, propertiesList)
  if ~(isnumeric(theta) && isnumeric(radius))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  [theta, radius] = matchPolarMatrices(theta, radius);
  if isvector(theta)
    theta = theta(:);
  end
  if isvector(radius)
    radius = radius(:);
  end
  handles = gobjects(0, 1);
  for k = 1:size(radius, 2)
    handles(end + 1, 1) = createPolarLine(ax, theta(:, k), radius(:, k), lineSpec, propertiesList);
  end
end
%=============================================================================
function h = createPolarLine(ax, theta, radius, lineSpec, propertiesList)
  lineProperties = propertiesFromLineSpec(lineSpec, propertiesList);
  color = getColorAndUpdateIndex(ax);
  lineStyle = getLineStyleAndUpdateIndex(ax);
  if ~any(strcmp(lineProperties, 'LineStyle'))
    lineProperties = [lineProperties, {'LineStyle', lineStyle}];
  end
  if ~any(strcmp(lineProperties, 'Visible'))
    lineProperties = [lineProperties, {'Visible', 'on'}];
  end
  h = __line__('Parent', ax, 'XData', [], 'YData', [], ...
    'ThetaData', theta, 'RData', radius, 'Color', color, ...
    lineProperties{:});
end
%=============================================================================
function properties = propertiesFromLineSpec(lineSpec, propertiesList)
  if isempty(lineSpec)
    properties = propertiesList;
    return
  end
  [ps, cs, ms, msg] = colstyle(lineSpec, 'plot', false);
  if ~isempty(msg)
    error('Nelson:polarplot:invalidLineSpec', msg);
  end
  if isempty(ps)
    ps = 'none';
  end
  if strcmp(cs, '')
    properties = {'Marker', ms, 'LineStyle', ps, propertiesList{:}};
  else
    properties = {'Color', cs, 'Marker', ms, 'LineStyle', ps, propertiesList{:}};
  end
end
%=============================================================================
function tf = isPolarTableSyntax(args)
  tf = length(args) >= 3 && (istable(args{1}) || istimetable(args{1})) ...
    && isTableVariableSelector(args{2}) && isTableVariableSelector(args{3});
end
%=============================================================================
function tf = isTableVariableSelector(value)
  tf = ischar(value) || iscellstr(value) || isstring(value);
  tf = tf && ~isempty(value);
end
%=============================================================================
function [theta, radius, lineSpec, thetaVariables, radiusVariables, sourceTable, consumed] = ...
    parsePolarTableArguments(args)
  sourceTable = args{1};
  thetaVariables = normalizeTableVariableNames(args{2});
  radiusVariables = normalizeTableVariableNames(args{3});
  theta = tableVariablesData(sourceTable, thetaVariables);
  radius = tableVariablesData(sourceTable, radiusVariables);
  lineSpec = '';
  consumed = 3;
  if length(args) >= 4 && ischar(args{4})
    [ps, cs, ms, msg] = colstyle(args{4}, 'plot', false);
    if isempty(msg)
      lineSpec = args{4};
      consumed = 4;
    end
  end
end
%=============================================================================
function names = normalizeTableVariableNames(names)
  if ischar(names)
    names = {names};
    return
  end
  if isstring(names)
    names = cellstr(names(:)');
    return
  end
  if ~iscellstr(names)
    error('Nelson:graphics:invalidVariableNames', 'Variable names must be character vectors or strings.');
  end
  names = names(:)';
end
%=============================================================================
function values = tableVariablesData(tbl, variableNames)
  values = [];
  for k = 1:length(variableNames)
    value = tableVariableData(tbl, variableNames{k});
    if isvector(value)
      value = value(:);
    end
    if isempty(values)
      values = value;
    elseif size(values, 1) == size(value, 1)
      values = [values, value];
    else
      error('Nelson:graphics:invalidTableVariables', 'Table variables must have compatible row counts.');
    end
  end
end
%=============================================================================
function value = tableVariableData(tbl, variableName)
  validateTableVariableName(variableName, tbl.Properties.VariableNames);
  value = tbl.(variableName);
  if ~(isnumeric(value) || islogical(value))
    error('Nelson:graphics:numericTableVariableExpected', 'Numeric table variable expected.');
  end
  value = double(value);
end
%=============================================================================
function validateTableVariableName(name, variableNames)
  if ~any(strcmp(name, variableNames))
    error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
  end
end
%=============================================================================
function setPolarTableMetadata(handles, sourceTable, thetaVariables, radiusVariables)
  for k = 1:numel(handles)
    thetaVariable = tableVariableForHandle(thetaVariables, k);
    radiusVariable = tableVariableForHandle(radiusVariables, k);
    if isprop(handles(k), 'SourceTable')
      handles(k).SourceTable = sourceTable;
    end
    if isprop(handles(k), 'ThetaVariable')
      handles(k).ThetaVariable = thetaVariable;
    end
    if isprop(handles(k), 'RVariable')
      handles(k).RVariable = radiusVariable;
    end
    if isprop(handles(k), 'DisplayName') && isempty(handles(k).DisplayName)
      handles(k).DisplayName = radiusVariable;
    end
  end
end
%=============================================================================
function name = tableVariableForHandle(names, index)
  if length(names) == 1
    name = names{1};
  else
    name = names{min(index, length(names))};
  end
end
%=============================================================================
function [a, b] = matchPolarMatrices(a, b)
  if isvector(a) && ~isvector(b)
    if length(a) == size(b, 1)
      a = repmat(a(:), [1, size(b, 2)]);
      return
    elseif length(a) == size(b, 2)
      b = b';
      a = repmat(a(:)', [size(b, 2), 1])';
      return
    else
      error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
    end
  end
  if ~isvector(a) && isvector(b)
    if length(b) == size(a, 1)
      b = repmat(b(:), [1, size(a, 2)]);
    elseif length(b) == size(a, 2)
      a = a';
      b = repmat(b(:)', [size(a, 2), 1])';
    else
      error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
    end
  end
  if ~isequal(size(a), size(b))
    error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
  end
end
%=============================================================================
