%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = raincloudplot(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [posArgs, optionArgs] = splitDataAndOptions(args);
  [series, labels] = parseRainCloudData(posArgs);
  ax = graphicsTargetAxes(parent);
  if isempty(parent)
    target = ax;
  else
    target = parent;
  end
  firstIndex = countRainClouds(target) + 1;
  handles = [];
  for k = 1:numel(series)
    s = series(k);
    ctorArgs = {'Parent', target, 'XData', s.XData, 'YData', s.YData, ...
      'SeriesIndex', firstIndex + k - 1};
    if ~isempty(s.SourceTable)
      ctorArgs = [ctorArgs, {'SourceTable', s.SourceTable, ...
        'XVariable', s.XVariable, 'YVariable', s.YVariable}];
    end
    h = __raincloudplot__(ctorArgs{:}, optionArgs{:});
    set(h, 'XDataMode', s.XDataMode, 'YDataMode', s.YDataMode);
    handles = [handles; h];
  end
  applyPositionTicks(ax, handles, labels);
  if nargout == 1
    varargout{1} = handles;
  end
end
%=============================================================================
function [posArgs, optionArgs] = splitDataAndOptions(args)
  if isempty(args)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if istable(args{1}) || istimetable(args{1})
    % name-value arguments come in pairs: the parity gives the selector count.
    if numel(args) >= 3 && rem(numel(args), 2) == 1
      nPos = 3;
    else
      nPos = 2;
    end
    nPos = min(nPos, numel(args));
  else
    nPos = 1;
    while nPos < numel(args) && ~(ischar(args{nPos + 1}) || isStringScalar(args{nPos + 1}))
      nPos = nPos + 1;
    end
  end
  posArgs = args(1:nPos);
  optionArgs = args(nPos + 1:end);
  if rem(numel(optionArgs), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(optionArgs)
    optionArgs{k} = canonicalOptionName(optionArgs{k});
  end
end
%=============================================================================
function [series, labels] = parseRainCloudData(posArgs)
  labels = {};
  if istable(posArgs{1}) || istimetable(posArgs{1})
    [series, labels] = tableSeries(posArgs);
    return
  end
  if numel(posArgs) > 2
    error('Nelson:graphics:tooManyDataArguments', 'Too many data arguments.');
  end
  y = posArgs{end};
  mustBeNumericData(y, 'ydata');
  if numel(posArgs) == 1
    series = matrixSeries(y);
    return
  end
  x = posArgs{1};
  if isvector(y) || isempty(y)
    xs = {x};
    ys = {y};
  elseif isequal(size(x), size(y))
    xs = num2cell(x, 1);
    ys = num2cell(y, 1);
  else
    error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
  end
  [positions, labels] = groupPositions(xs, ys);
  series = repmat(makeSeries([], [], 'manual', 'manual'), numel(ys), 1);
  for k = 1:numel(ys)
    series(k) = makeSeries(positions{k}, ys{k}, 'manual', 'manual');
  end
end
%=============================================================================
function series = matrixSeries(y)
  if isvector(y) || isempty(y)
    series = makeSeries(ones(numel(y), 1), y, 'auto', 'manual');
    return
  end
  series = repmat(makeSeries([], [], 'auto', 'manual'), size(y, 2), 1);
  for k = 1:size(y, 2)
    series(k) = makeSeries(k * ones(size(y, 1), 1), y(:, k), 'auto', 'manual');
  end
end
%=============================================================================
function [series, labels] = tableSeries(posArgs)
  tbl = posArgs{1};
  yNames = graphicsTableVariableNames(tbl, posArgs{end});
  if numel(posArgs) == 2
    xNames = {};
  else
    xNames = graphicsTableVariableNames(tbl, posArgs{2});
  end
  count = max(numel(xNames), numel(yNames));
  if ~isempty(xNames) && numel(xNames) ~= count && numel(xNames) ~= 1
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
  if numel(yNames) ~= count && numel(yNames) ~= 1
    error('Nelson:dimagree:tableVariableCountsMatch', 'Table variable counts must match.');
  end
  ys = cell(1, count);
  xs = cell(1, count);
  for k = 1:count
    ys{k} = tbl.(yNames{min(k, numel(yNames))});
    mustBeNumericData(ys{k}, yNames{min(k, numel(yNames))});
    if ~isempty(xNames)
      xs{k} = tbl.(xNames{min(k, numel(xNames))});
    end
  end
  labels = {};
  if ~isempty(xNames)
    [xs, labels] = groupPositions(xs, ys);
  end
  series = repmat(makeSeries([], [], 'auto', 'auto'), count, 1);
  for k = 1:count
    if isempty(xNames)
      x = ones(numel(ys{k}), 1);
      xName = '';
    else
      x = xs{k};
      xName = xNames{min(k, numel(xNames))};
    end
    series(k) = makeSeries(x, ys{k}, 'auto', 'auto');
    series(k).SourceTable = tbl;
    series(k).XVariable = xName;
    series(k).YVariable = yNames{min(k, numel(yNames))};
  end
end
%=============================================================================
function s = makeSeries(x, y, xMode, yMode)
  s = struct('XData', double(x(:)), 'YData', y(:), 'XDataMode', xMode, ...
    'YDataMode', yMode, 'SourceTable', [], 'XVariable', '', 'YVariable', '');
end
%=============================================================================
function mustBeNumericData(value, name)
  if ~isnumeric(value) || ~isreal(value)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', name);
  end
end
%=============================================================================
function [positions, labels] = groupPositions(xs, ys)
  % Categorical groups of every series share one position per category name,
  % so that series grouped by different variables line up or stack.
  labels = {};
  positions = cell(size(xs));
  for k = 1:numel(xs)
    x = xs{k};
    if numel(x) ~= numel(ys{k})
      error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
    end
    if iscategorical(x)
      cats = categories(x);
      labels = [labels; cats(~ismember(cats, labels))];
      [~, index] = ismember(cellstr(x(:)), labels);
      index(isundefined(x(:))) = NaN;
      positions{k} = double(index);
    elseif (isnumeric(x) && isreal(x)) || islogical(x)
      positions{k} = double(x(:));
    else
      error('Nelson:raincloudplot:invalidGroupData', 'Group data must be numeric or categorical.');
    end
  end
end
%=============================================================================
function count = countRainClouds(target)
  children = target.Children;
  count = 0;
  for k = 1:numel(children)
    count = count + double(isgraphics(children(k), 'raincloudplot'));
  end
end
%=============================================================================
function applyPositionTicks(ax, handles, labels)
  if isempty(handles)
    return
  end
  if strcmp(handles(1).Orientation, 'horizontal')
    axisName = 'Y';
  else
    axisName = 'X';
  end
  if ~isempty(labels)
    set(ax, [axisName, 'Tick'], 1:numel(labels), [axisName, 'TickLabel'], labels);
    return
  end
  positions = [];
  for k = 1:numel(handles)
    positions = [positions; handles(k).XData(:)];
  end
  positions = unique(positions(isfinite(positions)));
  if ~isempty(positions)
    set(ax, [axisName, 'Tick'], positions(:).');
  end
end
%=============================================================================
function name = canonicalOptionName(value)
  valid = {'DensityWidth', 'Orientation', 'FaceColor', 'FaceColorMode', 'FaceAlpha', ...
    'EdgeColor', 'EdgeColorMode', 'LineWidth', 'SeriesIndex', 'Marker', 'MarkerSize', ...
    'MarkerFaceColor', 'MarkerFaceColorMode', 'MarkerEdgeColor', 'MarkerEdgeColorMode', ...
    'DisplayName', 'Visible', 'Clipping', 'HandleVisibility', 'Tag', 'UserData', ...
    'ButtonDownFcn', 'CreateFcn', 'DeleteFcn', 'Interruptible', 'BusyAction', ...
    'PickableParts', 'HitTest', 'Selected', 'SelectionHighlight', 'ContextMenu'};
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  matches = find(strcmpi(char(value), valid));
  if isempty(matches)
    matches = find(strncmpi(char(value), valid, length(char(value))));
  end
  if numel(matches) ~= 1
    error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'raincloudplot');
  end
  name = valid{matches};
end
%=============================================================================
