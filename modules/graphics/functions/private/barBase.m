%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = barBase(orientation, varargin)
  args = varargin;
  [parent, args] = parseParent(args);
  target = prepareParent(parent);
  [x, y, labels, displayNames, width, mode, color, properties, isDate, isCategoricalAxis] = parseBarArguments(args);
  mode = effectiveStringProperty(properties, 'BarLayout', mode);
  width = effectiveNumericProperty(properties, 'BarWidth', width);
  baseValue = effectiveScalarProperty(properties, 'BaseValue', 0);
  updatePositionLimits = isBarAxesLimitAuto(target, orientation);
  updateValueLimits = isBarValueAxisLimitAuto(target, orientation);
  h = createBars(target, orientation, x, y, displayNames, width, mode, color, baseValue, properties);
  applyBarAxesState(target, orientation, x, y, labels, width, mode, size(y, 2), baseValue, ...
    updatePositionLimits, updateValueLimits, isCategoricalAxis);
  if isDate
    % The bar positions are serial date numbers; switch the position axis
    % ('x' for vertical bars, 'y' for horizontal ones) into date mode.
    if strcmp(orientation, 'horizontal')
      applyDatetimeAxis(target, 'y');
    else
      applyDatetimeAxis(target, 'x');
    end
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1}(1);
    args = args(2:end);
  else
    [parent, args] = graphicsExtractParentProperty(args);
  end
end
%=============================================================================
function target = prepareParent(parent)
  if isempty(parent)
    target = newplot();
  elseif isgraphics(parent, 'axes')
    target = newplot(parent);
  elseif isgraphics(parent, 'hggroup')
    target = parent;
  else
    error('Nelson:graphics:invalidParent', 'Parent must be an axes or hggroup object.');
  end
end
%=============================================================================
function [x, y, labels, displayNames, width, mode, color, properties, isDate, isCategoricalAxis] = parseBarArguments(args)
  [args, properties] = splitProperties(args);
  if ~isempty(args) && (istable(args{1}) || istimetable(args{1}))
    [x, y, labels, displayNames, isDate, isCategoricalAxis, args] = parseTableBarArguments(args);
    [args, mode, color] = splitStyleArguments(args);
    [args, width] = splitWidthArgument(args);
  else
    [args, mode, color] = splitStyleArguments(args);
    [args, width] = splitWidthArgument(args);
    [x, y, labels, displayNames, isDate, isCategoricalAxis, args] = parseArrayBarArguments(args);
  end
  if ~isempty(args)
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if numel(x) ~= size(y, 1)
    error('Nelson:barBase:invalidXLength', 'X length must match the number of rows in Y.');
  end
end
%=============================================================================
function [x, y, labels, displayNames, isDate, isCategoricalAxis, args] = parseArrayBarArguments(args)
  if isempty(args) || numel(args) > 2
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  labels = [];
  displayNames = {};
  isDate = false;
  isCategoricalAxis = false;
  if numel(args) == 1
    y = numericMatrix(args{1}, 'Y');
    x = 1:size(y, 1);
  else
    [x, labels, isDate] = positionVector(args{1});
    isCategoricalAxis = iscategorical(args{1}) || isstring(args{1});
    y = numericMatrixForPositions(args{2}, 'Y', x);
    if iscategorical(args{1})
      [x, y] = orderCategoricalBars(x, y);
    end
  end
  args = {};
end
%=============================================================================
function [x, y] = orderCategoricalBars(x, y)
  % Place bars in category order: sort by category code so positions become
  % 1:N and Y is reordered to match categories(), as expected.
  [x, perm] = sort(x(:).');
  y = y(perm, :);
end
%=============================================================================
function [x, y, labels, displayNames, isDate, isCategoricalAxis, args] = parseTableBarArguments(args)
  tbl = args{1};
  args = args(2:end);
  if isempty(args)
    error('Nelson:graphics:tableVariableExpected', 'Table variable arguments expected.');
  end
  xIsCategorical = false;
  isCategoricalAxis = false;
  isDate = false;
  if numel(args) >= 2 && isTableVariableSelector(tbl, args{2})
    xNames = graphicsTableVariableNames(tbl, args{1});
    yNames = graphicsTableVariableNames(tbl, args{2});
    args = args(3:end);
    if numel(xNames) ~= 1
      error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
    end
    xValue = tbl.(xNames{1});
    xIsCategorical = iscategorical(xValue);
    isCategoricalAxis = xIsCategorical || isstring(xValue);
    [x, labels, isDate] = positionVector(xValue);
  else
    yNames = graphicsTableVariableNames(tbl, args{1});
    args = args(2:end);
    labels = [];
    x = [];
  end
  y = tableNumericMatrix(tbl, yNames);
  displayNames = yNames;
  if isempty(x)
    x = 1:size(y, 1);
  end
  if xIsCategorical
    [x, y] = orderCategoricalBars(x, y);
  end
end
%=============================================================================
function tf = isTableVariableSelector(tbl, selector)
  try
    graphicsTableVariableNames(tbl, selector);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function data = tableNumericMatrix(tbl, names)
  data = [];
  for k = 1:numel(names)
    value = numericMatrix(tbl.(names{k}), names{k});
    if size(value, 2) ~= 1
      error('Nelson:validators:mustBeVector', '%s must be a vector.', names{k});
    end
    data = [data, value];
  end
end
%=============================================================================
function [args, properties] = splitProperties(args)
  firstProperty = [];
  for k = 1:numel(args)
    if isBarPropertyName(args{k})
      firstProperty = k;
      break
    end
  end
  if isempty(firstProperty)
    properties = {};
  else
    properties = args(firstProperty:end);
    args = args(1:firstProperty - 1);
    if mod(numel(properties), 2) ~= 0
      error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
    end
  end
end
%=============================================================================
function [args, mode, color] = splitStyleArguments(args)
  mode = 'grouped';
  color = [];
  while ~isempty(args) && (ischar(args{end}) || isStringScalar(args{end}))
    value = char(args{end});
    if strcmpi(value, 'grouped') || strcmpi(value, 'stacked')
      mode = lower(value);
    elseif isColorString(value)
      color = getColorShortName(value);
    else
      error('Nelson:barBase:unrecognizedOption', 'Unknown option.');
    end
    args(end) = [];
  end
end
%=============================================================================
function [args, width] = splitWidthArgument(args)
  width = 0.8;
  if ~isempty(args) && isnumeric(args{end}) && isscalar(args{end}) && numel(args) > 1
    width = args{end};
    args(end) = [];
  end
  validatePositiveFiniteScalar(width, sprintf(_('%s must be a positive finite scalar.'), 'Width'));
end
%=============================================================================
function y = numericMatrix(value, name)
  if ~isnumeric(value) || ~isreal(value)
    error('Nelson:graphics:mustBeRealMatrix', '%s must be a real numeric matrix.', name);
  end
  y = double(value);
  if isvector(y)
    y = y(:);
  end
end
%=============================================================================
function y = numericMatrixForPositions(value, name, x)
  y = numericMatrix(value, name);
  if isvector(value)
    if isscalar(x)
      y = double(value(:).');
    end
    return
  end
  if numel(x) ~= size(y, 1) && numel(x) == size(y, 2)
    y = y.';
  end
end
%=============================================================================
function [x, labels, isDate] = positionVector(value)
  labels = [];
  isDate = false;
  if isa(value, 'datetime')
    % Bars are positioned at the serial date numbers; the matching axis is put
    % into date mode by the caller (see barBase / applyDatetimeAxis).
    x = datetimeToSerial(value);
    x = x(:).';
    isDate = true;
    return
  end
  if iscategorical(value)
    % Labels follow the category order (as returned by categories()), not the
    % data order. Bars are positioned at the category codes so the caller can
    % reorder Y into category order (see orderCategoricalBars).
    cats = categories(value);
    labels = cats(:).';
    x = double(value(:).');
    return
  end
  if isstring(value)
    labels = cellstr(value(:)');
    x = 1:numel(labels);
    return
  end
  if iscell(value)
    labels = cellstr(value(:)');
    x = 1:numel(labels);
    return
  end
  if ~(isnumeric(value) && isreal(value))
    error('Nelson:barBase:invalidX', 'X must be numeric, categorical, or a string array.');
  end
  x = double(value(:)');
end
%=============================================================================
function h = createBars(parent, orientation, x, y, displayNames, width, mode, color, baseValue, properties)
  ax = barAxes(parent);
  nSeries = size(y, 2);
  h = gobjects(1, nSeries);
  positiveBases = baseValue * ones(size(y, 1), 1);
  negativeBases = baseValue * ones(size(y, 1), 1);
  for seriesIndex = 1:nSeries
    if strcmp(mode, 'stacked')
      [baseData, positiveBases, negativeBases] = stackedBaseData(y(:, seriesIndex), positiveBases, negativeBases);
    else
      baseData = baseValue * ones(size(y, 1), 1);
    end
    constructorArgs = {'Parent', parent, 'XData', x, 'YData', y(:, seriesIndex).', ...
      'BarLayout', mode, 'BarWidth', width, 'BaseValue', baseValue, ...
      'NumPeers', nSeries, 'SeriesIndex', seriesIndex, 'BarBaseData', baseData.'};
    if strcmp(orientation, 'horizontal')
      constructorArgs = [constructorArgs, {'Horizontal', 'on'}];
    end
    objectProperties = properties;
    if hasProperty(objectProperties, 'FaceColor') && ~hasProperty(objectProperties, 'FaceColorMode')
      objectProperties = [objectProperties, {'FaceColorMode', 'manual'}];
    end
    if ~hasProperty(objectProperties, 'FaceColor')
      constructorArgs = [constructorArgs, {'FaceColor', seriesColor(ax, color)}];
      if ~isempty(color)
        constructorArgs = [constructorArgs, {'FaceColorMode', 'manual'}];
      end
    end
    if ~hasProperty(objectProperties, 'DisplayName') && seriesIndex <= numel(displayNames)
      constructorArgs = [constructorArgs, {'DisplayName', displayNames{seriesIndex}}];
    end
    h(seriesIndex) = __bar__(constructorArgs{:}, objectProperties{:});
  end
end
%=============================================================================
function [baseData, posBase, negBase] = stackedBaseData(values, posBase, negBase)
  baseData = zeros(size(values));
  for k = 1:numel(values)
    if values(k) >= 0
      baseData(k) = posBase(k);
      posBase(k) = posBase(k) + values(k);
    else
      baseData(k) = negBase(k);
      negBase(k) = negBase(k) + values(k);
    end
  end
end
%=============================================================================
function color = seriesColor(ax, explicitColor)
  if ~isempty(explicitColor)
    color = explicitColor;
  else
    color = getColorAndUpdateIndex(ax);
  end
end
%=============================================================================
function value = effectiveStringProperty(properties, name, fallback)
  value = fallback;
  for k = 1:2:numel(properties)
    if isText(properties{k}) && strcmpi(char(properties{k}), name)
      value = lower(char(properties{k + 1}));
    end
  end
end
%=============================================================================
function value = effectiveNumericProperty(properties, name, fallback)
  value = effectiveScalarProperty(properties, name, fallback);
  validatePositiveFiniteScalar(value, sprintf(_('%s must be a positive finite scalar.'), name));
end
%=============================================================================
function value = effectiveScalarProperty(properties, name, fallback)
  value = fallback;
  for k = 1:2:numel(properties)
    if isText(properties{k}) && strcmpi(char(properties{k}), name)
      value = properties{k + 1};
    end
  end
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value)
    error('Nelson:barBase:mustBeFiniteScalar', '%s must be a finite scalar.', name);
  end
end
%=============================================================================
function validatePositiveFiniteScalar(value, msg)
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value <= 0
    error('Nelson:barBase:mustBePositiveFinite', msg);
  end
end
%=============================================================================
function applyBarAxesState(parent, orientation, x, y, labels, width, mode, nSeries, baseValue, ...
  updatePositionLimits, updateValueLimits, isCategoricalAxis)
  ax = barAxes(parent);
  if isempty(ax)
    return
  end
  ax.Box = 'on';
  useBarTicks = ~isempty(labels) || shouldUseBarTicks(x);
  if strcmp(orientation, 'horizontal')
    if useBarTicks
      ax.YTick = x;
    end
    if ~isempty(labels)
      ax.YTickLabel = labels;
    end
  else
    if useBarTicks
      ax.XTick = x;
    end
    if ~isempty(labels)
      ax.XTickLabel = labels;
    end
  end
  if updatePositionLimits
    limits = barPositionLimits(x, width, mode, nSeries, labels);
    if strcmp(orientation, 'horizontal')
      ax.YLim = limits;
    else
      ax.XLim = limits;
    end
  end


  if updateValueLimits
    [limits, ticks] = barValueLimits(y, mode, baseValue);
    if strcmp(orientation, 'horizontal')
      ax.XLim = limits;
      if strcmp(ax.XTickMode, 'auto')
        ax.XTick = ticks;
      end
    else
      ax.YLim = limits;
      if strcmp(ax.YTickMode, 'auto')
        ax.YTick = ticks;
      end
    end
  end
  if isCategoricalAxis && ~isempty(labels)
    if strcmp(orientation, 'horizontal')
      __axis_categorical__(ax, 'y', labels);
    else
      __axis_categorical__(ax, 'x', labels);
    end
  end
end
%=============================================================================
function limits = barPositionLimits(x, width, mode, nSeries, labels)
  x = x(:);
  x = x(isfinite(x));
  if isempty(x)
    limits = [0 1];
    return
  end
  step = minPositiveStep(x);
  if strcmp(mode, 'grouped') && nSeries > 1 && ~isempty(labels)
    pad = 0.75 * step;
  elseif strcmp(mode, 'grouped') && nSeries > 1
    groupWidth = min(width, nSeries / (nSeries + 1.5));
    pad = (groupWidth / 2 + max(0, 1 - width)) * step;
  else
    pad = (0.8 + width / 2) * step;
  end
  limits = [min(x) - pad, max(x) + pad];
end
%=============================================================================
function [limits, ticks] = barValueLimits(y, mode, baseValue)
  if strcmp(mode, 'stacked')
    positiveBases = baseValue * ones(size(y, 1), 1);
    negativeBases = baseValue * ones(size(y, 1), 1);
    values = [0; baseValue];
    for seriesIndex = 1:size(y, 2)
      for rowIndex = 1:size(y, 1)
        value = y(rowIndex, seriesIndex);
        if value >= 0
          endpoint = positiveBases(rowIndex) + value;
          positiveBases(rowIndex) = endpoint;
        else
          endpoint = negativeBases(rowIndex) + value;
          negativeBases(rowIndex) = endpoint;
        end
        values = [values; endpoint];
      end
    end
  else
    values = [0; baseValue; y(:)];
  end
  values = values(isfinite(values));
  if isempty(values)
    limits = [0 1];
    ticks = [0 1];
    return
  end
  lo = min(values);
  hi = max(values);
  if lo == hi
    limits = [lo - 0.5, hi + 0.5];
    ticks = limits;
    return
  end
  % The data extent [lo, hi] always brackets the bar baseline; snap it outward
  % onto a nice grid so the frozen value limits and ticks match what an
  % automatic axis would settle on for the same range.
  [limits, ticks] = graphicsNiceAxisLimits(lo, hi, 10);
end
%=============================================================================
function tf = shouldUseBarTicks(x)
  x = x(:);
  x = x(isfinite(x));
  tf = ~isempty(x) && all(abs(x - round(x)) < 1e-12);
end
%=============================================================================
function value = minPositiveStep(x)
  values = sort(unique(x(:)));
  if numel(values) < 2
    value = 1;
    return
  end
  dx = diff(values);
  dx = dx(dx > 0 & isfinite(dx));
  if isempty(dx)
    value = 1;
  else
    value = min(dx);
  end
end
%=============================================================================
function res = isBarAxesLimitAuto(parent, orientation)
  ax = barAxes(parent);
  if isempty(ax)
    res = false;
  elseif strcmp(orientation, 'horizontal')
    res = strcmp(ax.YLimMode, 'auto');
  else
    res = strcmp(ax.XLimMode, 'auto');
  end
end
%=============================================================================
function res = isBarValueAxisLimitAuto(parent, orientation)
  ax = barAxes(parent);
  if isempty(ax)
    res = false;
  elseif strcmp(orientation, 'horizontal')
    res = strcmp(ax.XLimMode, 'auto');
  else
    res = strcmp(ax.YLimMode, 'auto');
  end
end
%=============================================================================
function ax = barAxes(parent)
  if isgraphics(parent, 'axes')
    ax = parent;
  else
    ax = ancestor(parent, 'axes');
  end
end
%=============================================================================
function tf = isBarPropertyName(value)
  tf = false;
  if ~isText(value)
    return
  end
  tf = isValidGraphicsProperty('bar', char(value));
end
%=============================================================================
function tf = isColorString(value)
  names = [getColorShortNameList(), getColorNameList()];
  tf = any(strcmpi(value, names));
end
%=============================================================================
function tf = hasProperty(properties, name)
  tf = false;
  for k = 1:2:numel(properties)
    if isText(properties{k}) && strcmpi(char(properties{k}), name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = isText(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
