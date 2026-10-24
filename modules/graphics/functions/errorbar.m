%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = errorbar(varargin)
  narginchk(2, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  [dataArgs, lineProperties, objectProperties, orientation] = parseTrailingArguments(args);
  series = parseDataArguments(dataArgs, orientation);
  target = prepareParent(parent);
  ax = errorbarAxes(target);
  nextPlot = '';
  if ~isempty(ax) && isscalar(ax) && isgraphics(ax, 'axes')
    nextPlot = ax.NextPlot;
  end
  h = gobjects(1, numel(series));
  for k = 1:numel(series)
    color = getColorAndUpdateIndex(ax);
    lineStyle = getLineStyleAndUpdateIndex(ax);
    constructorArgs = {'Parent', target, 'Color', color, 'LineStyle', lineStyle, ...
      'XDataMode', series(k).XDataMode, 'XData', series(k).XData, 'YData', series(k).YData, ...
      'YNegativeDelta', series(k).YNegativeDelta, 'YPositiveDelta', series(k).YPositiveDelta, ...
      'XNegativeDelta', series(k).XNegativeDelta, 'XPositiveDelta', series(k).XPositiveDelta};
    constructorArgs = [constructorArgs, lineProperties, objectProperties];
    h(k) = __errorbar__(constructorArgs{:});
  end
  if ~isempty(ax) && any(strcmp(nextPlot, {'replace', 'replaceall'}))
    ax.Box = 'on';
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
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
function [dataArgs, lineProperties, objectProperties, orientation] = parseTrailingArguments(args)
  firstText = find(cellfun(@isTextArgument, args), 1);
  if isempty(firstText)
    dataArgs = args;
  else
    dataArgs = args(1:firstText - 1);
  end
  lineProperties = {};
  objectProperties = {};
  orientation = '';
  if isempty(firstText)
    return
  end
  tail = args(firstText:end);
  while ~isempty(tail) && isTextArgument(tail{1}) && ~isErrorBarPropertyName(tail{1})
    text = char(tail{1});
    matchedOrientation = matchOrientation(text);
    if ~isempty(matchedOrientation)
      if ~isempty(orientation)
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
      end
      orientation = matchedOrientation;
    else
      if ~isempty(lineProperties)
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
      end
      try
        lineProperties = lineSpecProperties(text);
      catch
        if numel(tail) >= 2
          break
        end
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
      end
    end
    tail = tail(2:end);
  end
  objectProperties = tail;
  if mod(numel(objectProperties), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
end
%=============================================================================
function tf = isTextArgument(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function properties = lineSpecProperties(spec)
  [lineStyle, color, marker, msg] = colstyle(spec, 'plot', false);
  if ~isempty(msg)
    error('Nelson:errorbar:invalidLineSpec', msg);
  end
  properties = {};
  if isempty(lineStyle) && ~isempty(marker)
    lineStyle = 'none';
  end
  if ~isempty(lineStyle)
    properties = [properties, {'LineStyle', lineStyle}];
  end
  if ~isempty(color)
    properties = [properties, {'Color', color}];
  end
  if ~isempty(marker)
    properties = [properties, {'Marker', marker}];
  end
end
%=============================================================================
function orientation = matchOrientation(value)
  valid = {'horizontal', 'vertical', 'both'};
  orientation = '';
  for k = 1:numel(valid)
    if strncmpi(value, valid{k}, max(3, numel(value)))
      orientation = valid{k};
      return
    end
  end
end
%=============================================================================
function series = parseDataArguments(args, orientation)
  if isempty(orientation)
    orientation = 'vertical';
  end
  if numel(args) == 2
    y = numericData(args{1}, 'YData');
    x = [];
    e = numericData(args{2}, 'YDelta');
    [xneg, xpos, yneg, ypos] = orientSymmetricDeltas(e, orientation);
    xMode = 'auto';
  elseif numel(args) == 3
    x = numericData(args{1}, 'XData');
    y = numericData(args{2}, 'YData');
    e = numericData(args{3}, 'YDelta');
    [xneg, xpos, yneg, ypos] = orientSymmetricDeltas(e, orientation);
    xMode = xDataMode(x);
  elseif numel(args) == 4
    x = numericData(args{1}, 'XData');
    y = numericData(args{2}, 'YData');
    neg = numericData(args{3}, 'NegativeDelta');
    pos = numericData(args{4}, 'PositiveDelta');
    [xneg, xpos, yneg, ypos] = orientAsymmetricDeltas(neg, pos, orientation);
    xMode = xDataMode(x);
  elseif numel(args) == 6
    if ~strcmp(orientation, 'vertical')
      error('Nelson:errorbar:invalidUseOfOrientation', 'Invalid use of orientation with six data inputs.');
    end
    x = numericData(args{1}, 'XData');
    y = numericData(args{2}, 'YData');
    yneg = numericData(args{3}, 'YNegativeDelta');
    ypos = numericData(args{4}, 'YPositiveDelta');
    xneg = numericData(args{5}, 'XNegativeDelta');
    xpos = numericData(args{6}, 'XPositiveDelta');
    xMode = xDataMode(x);
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  series = buildErrorBarSeries(x, y, yneg, ypos, xneg, xpos, xMode);
end
%=============================================================================
function mode = xDataMode(x)
  if isempty(x)
    mode = 'auto';
  else
    mode = 'manual';
  end
end
%=============================================================================
function [xneg, xpos, yneg, ypos] = orientSymmetricDeltas(delta, orientation)
  [xneg, xpos, yneg, ypos] = orientAsymmetricDeltas(delta, delta, orientation);
end
%=============================================================================
function [xneg, xpos, yneg, ypos] = orientAsymmetricDeltas(neg, pos, orientation)
  xneg = [];
  xpos = [];
  yneg = [];
  ypos = [];
  switch orientation
    case 'horizontal'
      xneg = neg;
      xpos = pos;
    case 'both'
      xneg = neg;
      xpos = pos;
      yneg = neg;
      ypos = pos;
    otherwise
      yneg = neg;
      ypos = pos;
  end
end
%=============================================================================
function data = numericData(value, name)
  if isempty(value)
    data = [];
    return
  end
  if ~isnumeric(value) || ~isreal(value)
    error('Nelson:errorbar:mustBeRealNumeric', '%s must be real numeric data.', name);
  end
  data = double(value);
end
%=============================================================================
function series = buildErrorBarSeries(x, y, yneg, ypos, xneg, xpos, xMode)
  if shouldSplitVectorByElement(x, y, yneg, ypos, xneg, xpos)
    series = buildPointSeries(x, y, yneg, ypos, xneg, xpos, xMode);
    return
  end
  yColumns = dataColumns(y, 'YData');
  count = numel(yColumns);
  xColumns = xColumnsForSeries(x, yColumns, count, xMode);
  for k = 1:count
    if numel(xColumns{k}) ~= numel(yColumns{k})
      error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'XData', 'YData');
    end
  end
  ynegColumns = deltaColumnsForSeries(yneg, yColumns, count, 'YNegativeDelta');
  yposColumns = deltaColumnsForSeries(ypos, yColumns, count, 'YPositiveDelta');
  xnegColumns = deltaColumnsForSeries(xneg, yColumns, count, 'XNegativeDelta');
  xposColumns = deltaColumnsForSeries(xpos, yColumns, count, 'XPositiveDelta');
  series = repmat(emptySeries(), 1, count);
  for k = 1:count
    series(k).XDataMode = xMode;
    series(k).XData = xColumns{k};
    series(k).YData = yColumns{k};
    series(k).YNegativeDelta = ynegColumns{k};
    series(k).YPositiveDelta = yposColumns{k};
    series(k).XNegativeDelta = xnegColumns{k};
    series(k).XPositiveDelta = xposColumns{k};
  end
end
%=============================================================================
function tf = shouldSplitVectorByElement(x, y, yneg, ypos, xneg, xpos)
  tf = false;
  if isempty(y) || ~isvector(y) || numel(y) <= 1
    return
  end
  if ~isempty(x) && (~isvector(x) || numel(x) ~= numel(y))
    return
  end
  deltas = {yneg, ypos, xneg, xpos};
  for k = 1:numel(deltas)
    delta = deltas{k};
    if ~isempty(delta) && isscalar(delta)
      tf = true;
      return
    end
  end
end
%=============================================================================
function series = buildPointSeries(x, y, yneg, ypos, xneg, xpos, xMode)
  y = y(:).';
  if strcmp(xMode, 'auto')
    x = 1:numel(y);
  else
    x = x(:).';
  end
  count = numel(y);
  series = repmat(emptySeries(), 1, count);
  for k = 1:count
    series(k).XDataMode = xMode;
    series(k).XData = x(k);
    series(k).YData = y(k);
    series(k).YNegativeDelta = pointDelta(yneg, k);
    series(k).YPositiveDelta = pointDelta(ypos, k);
    series(k).XNegativeDelta = pointDelta(xneg, k);
    series(k).XPositiveDelta = pointDelta(xpos, k);
  end
end
%=============================================================================
function value = pointDelta(delta, index)
  if isempty(delta)
    value = [];
  elseif isscalar(delta)
    value = delta;
  else
    delta = delta(:).';
    value = delta(index);
  end
end
%=============================================================================
function st = emptySeries()
  st = struct('XDataMode', 'auto', 'XData', [], 'YData', [], ...
    'YNegativeDelta', [], 'YPositiveDelta', [], 'XNegativeDelta', [], 'XPositiveDelta', []);
end
%=============================================================================
function columns = dataColumns(data, name)
  if isempty(data)
    error('Nelson:errorbar:mustBeRealNumeric', '%s must be real numeric data.', name);
  end
  if isvector(data)
    columns = {data(:)'};
    return
  end
  columns = cell(1, size(data, 2));
  for k = 1:size(data, 2)
    columns{k} = data(:, k).';
  end
end
%=============================================================================
function columns = xColumnsForSeries(x, yColumns, count, xMode)
  if strcmp(xMode, 'auto')
    columns = cell(1, count);
    for k = 1:count
      columns{k} = 1:numel(yColumns{k});
    end
    return
  end
  columns = dataColumns(x, 'XData');
  if numel(columns) == 1 && count > 1
    columns = repmat(columns, 1, count);
  elseif numel(columns) ~= count
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'XData', 'YData');
  end
end
%=============================================================================
function columns = deltaColumnsForSeries(delta, yColumns, count, name)
  columns = cell(1, count);
  if isempty(delta)
    columns(:) = {[]};
    return
  end
  if isscalar(delta)
    columns(:) = {delta};
    return
  end
  if isvector(delta)
    value = delta(:).';
    validateDeltaLength(value, yColumns{1}, name);
    columns(:) = {value};
    return
  end
  if size(delta, 2) ~= count
    error('Nelson:errorbar:invalidLength', '%s must be a scalar or match XData and YData length.', name);
  end
  for k = 1:count
    value = delta(:, k).';
    validateDeltaLength(value, yColumns{k}, name);
    columns{k} = value;
  end
end
%=============================================================================
function validateDeltaLength(delta, y, name)
  if numel(delta) ~= numel(y)
    error('Nelson:errorbar:invalidLength', '%s must be a scalar or match XData and YData length.', name);
  end
end
%=============================================================================
function tf = isErrorBarPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  names = {'AlignVertexCenters', 'CapSize', 'Clipping', 'Color', 'ColorMode', ...
    'LineWidth', 'LineStyle', 'LineStyleMode', 'Marker', 'MarkerMode', ...
    'MarkerEdgeColor', 'MarkerFaceColor', 'MarkerSize', 'XNegativeDelta', ...
    'XNegativeDeltaSource', 'XPositiveDelta', 'XPositiveDeltaSource', ...
    'YNegativeDelta', 'YNegativeDeltaSource', 'YPositiveDelta', ...
    'YPositiveDeltaSource', 'Parent', 'Visible', 'HandleVisibility', ...
    'ButtonDownFcn', 'ContextMenu', 'BusyAction', 'Interruptible', ...
    'CreateFcn', 'DeleteFcn', 'Tag', 'UserData', 'Selected', ...
    'SelectionHighlight', 'HitTest', 'PickableParts', 'DisplayName', ...
    'SeriesIndex', 'XData', 'XDataMode', 'XDataSource', 'YData', ...
    'YDataSource'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
function ax = errorbarAxes(parent)
  if isgraphics(parent, 'axes')
    ax = parent;
  else
    ax = ancestor(parent, 'axes');
  end
end
%=============================================================================
