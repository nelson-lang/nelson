%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = wordcloud(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseCloudParent(args);
  [words, sizes, args, defaultTitle, sourceTable, wordVariable, sizeVariable] = parseWordCloudData(args);
  opts = parseWordCloudOptions(args);
  if ~opts.TitleSpecified && ~isempty(defaultTitle)
    opts.Title = defaultTitle;
  end
  ax = cloudAxes(parent);
  chartParent = ax.Parent;
  if isgraphics(chartParent, 'figure') && isequal(chartParent.Color, groot().DefaultFigureColor)
    chartParent.Color = [1 1 1];
  end
  positionArgs = explicitPositionArgs(opts);
  opts = rmfield(opts, {'TitleSpecified', 'OuterPositionSpecified', 'InnerPositionSpecified', ...
    'PositionSpecified', 'UnitsSpecified', 'PositionConstraintSpecified'});
  h = __wordcloudchart__('AutoParent', 'off', 'Parent', chartParent, ...
    'WordData', words, 'SizeData', sizes, ...
    'MaxDisplayWords', opts.MaxDisplayWords, 'Color', opts.Color, ...
    'HighlightColor', opts.HighlightColor, 'Shape', opts.Shape, 'LayoutNum', opts.LayoutNum, ...
    'SizePower', opts.SizePower, 'FontName', opts.FontName, 'Box', opts.Box, 'Title', opts.Title, ...
    'TitleFontName', opts.TitleFontName, positionArgs{:}, 'SourceTable', sourceTable, ...
    'WordVariable', wordVariable, 'SizeVariable', sizeVariable);
  % wordcloudDrawCloud lays out the words into the axes and installs the resize
  % handler; a later dot-set of a word-cloud property re-runs it through
  % __wordcloud_refresh__ to rebuild the cloud in place.
  textHandles = wordcloudDrawCloud(ax, words, sizes, opts);
  h.UserData = struct('ChartType', 'wordcloud', 'Words', {words}, 'SizeData', sizes, ...
    'Options', opts, 'Axes', ax, 'TextHandles', textHandles);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseCloudParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    error('Nelson:wordcloud:wordCloudChartCannotBeA', 'WordCloudChart cannot be a child of Axes.');
  end
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'figure')
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function ax = cloudAxes(parent)
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'figure')
    ax = axes('Parent', parent);
  else
    error('Nelson:wordcloud:invalidParent', 'Parent must be a figure or axes.')
  end
  ax.Color = [1 1 1];
end
%=============================================================================
function [words, sizes, args, defaultTitle, sourceTable, wordVariable, sizeVariable] = parseWordCloudData(args)
  defaultTitle = '';
  sourceTable = [];
  wordVariable = '';
  sizeVariable = '';
  if isempty(args)
    error('Nelson:wordcloud:wordDataExpected', 'Word data expected.');
  end
  if (istable(args{1}) || istimetable(args{1})) && numel(args) >= 3
    sourceTable = args{1};
    [words, wordVariable] = graphicsTableVariable(sourceTable, args{2});
    [sizes, sizeVariable] = graphicsTableVariable(sourceTable, args{3});
    defaultTitle = sizeVariable;
    args = args(4:end);
  elseif numel(args) >= 2 && ~isWordCloudPropertyName(args{2})
    words = args{1};
    sizes = args{2};
    args = args(3:end);
  else
    [words, sizes] = categoryCounts(args{1});
    args = args(2:end);
  end
  words = graphicsLabelsFromValue(words, -1);
  if ~isnumeric(sizes)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Word sizes');
  end
  sizes = double(sizes(:)');
  if numel(words) ~= numel(sizes)
    error('Nelson:dimagree:wordsSizesNumberElements', 'Words and sizes must have the same number of elements.');
  end
end
%=============================================================================
function [words, counts] = categoryCounts(value)
  labels = graphicsLabelsFromValue(value, -1);
  words = {};
  counts = [];
  for k = 1:numel(labels)
    idx = find(strcmp(labels{k}, words), 1);
    if isempty(idx)
      words{end + 1, 1} = labels{k};
      counts(end + 1, 1) = 1;
    else
      counts(idx) = counts(idx) + 1;
    end
  end
end
%=============================================================================
function opts = parseWordCloudOptions(args)
  opts = struct('MaxDisplayWords', 100, 'Color', [0.380392156862745 0.380392156862745 0.380392156862745], ...
    'HighlightColor', [0.752941176470588 0.298039215686275 0.043137254901961], ...
    'Shape', 'oval', 'LayoutNum', 1, 'SizePower', 0.5, 'FontName', 'Helvetica', ...
    'Box', 'off', 'Title', '', 'TitleFontName', 'Helvetica', ...
    'OuterPosition', [0 0 1 1], 'InnerPosition', [0 0 1 1], ...
    'Position', [0 0 1 1], 'Units', 'normalized', ...
    'PositionConstraint', 'outerposition', 'TitleSpecified', false, ...
    'OuterPositionSpecified', false, 'InnerPositionSpecified', false, ...
    'PositionSpecified', false, 'UnitsSpecified', false, 'PositionConstraintSpecified', false);
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'maxdisplaywords'
        opts.MaxDisplayWords = validatePositiveInteger(value, 'MaxDisplayWords');
      case 'color'
        opts.Color = value;
      case 'highlightcolor'
        opts.HighlightColor = value;
      case 'shape'
        opts.Shape = lower(char(value));
      case 'layoutnum'
        opts.LayoutNum = validatePositiveInteger(value, 'LayoutNum');
      case 'sizepower'
        opts.SizePower = double(value);
      case 'fontname'
        opts.FontName = char(value);
      case 'titlefontname'
        opts.TitleFontName = char(value);
      case 'box'
        opts.Box = lower(char(value));
      case 'title'
        opts.Title = titleText(value);
        opts.TitleSpecified = true;
      case 'outerposition'
        opts.OuterPosition = double(value);
        opts.OuterPositionSpecified = true;
      case 'innerposition'
        opts.InnerPosition = double(value);
        opts.InnerPositionSpecified = true;
      case 'position'
        opts.Position = double(value);
        opts.PositionSpecified = true;
      case 'units'
        opts.Units = lower(char(value));
        opts.UnitsSpecified = true;
      case 'positionconstraint'
        opts.PositionConstraint = lower(char(value));
        opts.PositionConstraintSpecified = true;
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'wordcloud');
    end
  end
end
%=============================================================================
function args = explicitPositionArgs(opts)
  names = {'OuterPosition', 'InnerPosition', 'Position', 'Units', 'PositionConstraint'};
  flags = {'OuterPositionSpecified', 'InnerPositionSpecified', 'PositionSpecified', ...
    'UnitsSpecified', 'PositionConstraintSpecified'};
  args = {};
  for k = 1:numel(names)
    if opts.(flags{k})
      args = [args, {names{k}, opts.(names{k})}];
    end
  end
end
%=============================================================================
function text = titleText(value)
  if iscell(value)
    text = strjoin(cellfun(@char, value(:)', 'UniformOutput', false), newline);
  elseif isnumeric(value)
    text = num2str(value);
  else
    text = char(value);
  end
end
%=============================================================================
function out = validatePositiveInteger(value, name)
  % a name-value option that must be a positive integer scalar
  v = double(value);
  if ~isscalar(v) || ~isfinite(v) || v <= 0 || v ~= floor(v)
    error('Nelson:graphics:mustBePositiveInteger', '%s must be a positive integer.', name);
  end
  out = v;
end
%=============================================================================
function tf = isWordCloudPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  tf = any(strcmpi(char(value), {'MaxDisplayWords', 'Color', 'HighlightColor', 'Shape', ...
    'LayoutNum', 'SizePower', 'FontName', 'Box', 'Title', 'TitleFontName', ...
    'OuterPosition', 'InnerPosition', 'Position', 'Units', 'PositionConstraint'}));
end
%=============================================================================
