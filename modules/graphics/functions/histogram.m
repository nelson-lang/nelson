%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = histogram(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  ax = ancestor(parent, 'axes');
  nextPlot = '';
  if ~isempty(ax) && isscalar(ax) && isgraphics(ax, 'axes')
    nextPlot = ax.NextPlot;
  end
  [constructorArgs, categoricalInfo] = parseHistogramArguments(parent, args);
  h = __histogram__(constructorArgs{:});
  if ~isempty(categoricalInfo)
    applyCategoricalAxes(parent, h, categoricalInfo);
  end
  applyStairsEdgeColor(ax, h, args);
  if ~isempty(ax) && any(strcmp(nextPlot, {'replace', 'replaceall'}))
    ax.Box = 'on';
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = newplotTarget(args{1}(1));
    args = args(2:end);
  else
    parent = newplotTarget(graphicsParentProperty(args));
  end
end
%=============================================================================
function [constructorArgs, categoricalInfo] = parseHistogramArguments(parent, args)
  constructorArgs = {};
  categoricalInfo = [];
  if ~isempty(parent)
    constructorArgs = {'Parent', parent};
  end
  if isempty(args)
    error('Nelson:graphics:dataExpected', 'Data expected.');
  end
  if isPropertyName(args{1})
    dataConsumed = false;
  elseif iscategorical(args{1})
    [constructorArgs, categoricalInfo, args] = parseCategoricalHistogram(constructorArgs, args);
    dataConsumed = true;
  else
    constructorArgs = [constructorArgs, {'Data', args{1}}];
    args = args(2:end);
    dataConsumed = true;
  end
  if dataConsumed && isempty(categoricalInfo) && ~isempty(args) && isnumeric(args{1})
    binArgument = args{1};
    args = args(2:end);
    if isscalar(binArgument)
      constructorArgs = [constructorArgs, {'NumBins', binArgument}];
    else
      constructorArgs = [constructorArgs, {'BinEdges', binArgument}];
    end
  end
  if mod(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  constructorArgs = [constructorArgs, args];
end
%=============================================================================
function [constructorArgs, info, args] = parseCategoricalHistogram(constructorArgs, args)
  X = args{1};
  args = args(2:end);
  allCategories = categories(X);
  allCategories = allCategories(:).';
  allCounts = countcats(X);
  allCounts = allCounts(:).';
  if ~isempty(args) && ~isPropertyName(args{1}) && ...
    (iscellstr(args{1}) || isstring(args{1}) || iscategorical(args{1}))
    if iscategorical(args{1})
      cats = categories(args{1});
    else
      cats = cellstr(args{1});
    end
    cats = cats(:).';
    counts = zeros(1, numel(cats));
    for k = 1:numel(cats)
      idx = find(strcmp(allCategories, cats{k}), 1);
      if ~isempty(idx)
        counts(k) = allCounts(idx);
      end
    end
    args = args(2:end);
  else
    cats = allCategories;
    counts = allCounts;
  end
  n = numel(cats);
  edges = 0.5:1:(n + 0.5);
  constructorArgs = [constructorArgs, {'Categories', cats, ...
    'BinCounts', counts, 'BinEdges', edges}];
  info = struct('categories', {cats}, 'n', n);
end
%=============================================================================
function applyCategoricalAxes(parent, h, info)
  if isgraphics(parent, 'axes')
    ax = parent;
  else
    ax = ancestor(h, 'axes');
  end
  if isempty(ax) || ~isgraphics(ax, 'axes')
    return
  end
  ticks = 1:info.n;
  if strcmp(h.Orientation, 'horizontal')
    ax.YTick = ticks;
    ax.YTickLabel = info.categories;
  else
    ax.XTick = ticks;
    ax.XTickLabel = info.categories;
  end
end
%=============================================================================
function applyStairsEdgeColor(ax, h, args)
  % In 'stairs' DisplayStyle the histogram draws only its outline. Give that
  % outline the histogram's automatic series color (the same ColorOrder color a
  % bar-style histogram uses for its face) instead of the default edge color,
  % unless the user asked for a specific EdgeColor.
  if isempty(ax) || ~isscalar(ax) || ~isgraphics(ax, 'axes')
    return
  end
  if ~strcmp(h.DisplayStyle, 'stairs')
    return
  end
  if hasArgument(args, 'EdgeColor')
    return
  end
  colorOrder = ax.ColorOrder;
  if isempty(colorOrder)
    return
  end
  seriesIndex = 1;
  try
    seriesIndex = round(h.SeriesIndex);
  catch
  end
  nbColors = size(colorOrder, 1);
  colorIndex = mod(seriesIndex - 1, nbColors) + 1;
  h.EdgeColor = colorOrder(colorIndex, :);
end
%=============================================================================
function tf = hasArgument(args, name)
  tf = false;
  for k = 1:numel(args)
    if (ischar(args{k}) || isStringScalar(args{k})) && strcmpi(char(args{k}), name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = isPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  names = {'Data', 'BinCounts', 'NumBins', 'BinEdges', 'BinWidth', ...
    'BinMethod', 'BinLimits', 'Normalization', 'FaceColor', 'EdgeColor', ...
    'LineWidth', 'LineStyle', 'BinCountsMode', 'BinLimitsMode', ...
    'Orientation', 'DisplayStyle', 'FaceAlpha', 'EdgeAlpha', ...
    'DisplayName', 'Visible', 'Tag', 'UserData'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
