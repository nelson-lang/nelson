%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = violinplot(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [posArgs, optionArgs] = splitDataAndOptions(args);
  series = parseViolinData(posArgs, optionArgs);
  ax = graphicsTargetAxes(parent);
  handles = [];
  drawOptions = drawingOptions(optionArgs);
  for k = 1:numel(series)
    violinArgs = [{'Parent', ax, 'XData', series(k).XData, 'YData', series(k).YData}, ...
      optionArgs, series(k).Modes];
    h = __violinplot__(violinArgs{:});
    handles = [handles, h];
  end
  applyViolinAxes(ax, series, drawOptions);
  if nargout == 1
    varargout{1} = handles;
  end
end
%=============================================================================
function [posArgs, optionArgs] = splitDataAndOptions(args)
  firstOption = numel(args) + 1;
  for k = 1:numel(args)
    if isViolinOptionName(args{k}) || (k > 1 && (ischar(args{k}) || isStringScalar(args{k})))
      firstOption = k;
      break
    end
  end
  posArgs = args(1:firstOption - 1);
  optionArgs = normalizeOptions(args(firstOption:end));
  names = optionArgs(1:2:end);
  hasPoints = any(strcmp(names, 'EvaluationPoints'));
  hasDensities = any(strcmp(names, 'DensityValues'));
  if isempty(posArgs)
    if ~(hasPoints && hasDensities)
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
    end
  elseif hasPoints || hasDensities
    error('Nelson:violinplot:densityWithSampleData', '''EvaluationPoints'' and ''DensityValues'' are not supported if sample data is specified.');
  end
end
%=============================================================================
function args = normalizeOptions(args)
  if rem(numel(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(args)
    name = canonicalOptionName(args{k});
    args{k} = name;
  end
end
%=============================================================================
function series = parseViolinData(posArgs, optionArgs)
  if isempty(posArgs)
    series = densitySeries(optionArgs);
  elseif numel(posArgs) == 1
    series = dataOnlySeries(posArgs{1});
  elseif numel(posArgs) == 2
    series = groupedSeries(posArgs{1}, posArgs{2});
  else
    error('Nelson:graphics:tooManyDataArguments', 'Too many data arguments.');
  end
end
%=============================================================================
function series = densitySeries(optionArgs)
  % Precomputed densities: one violin per column of EvaluationPoints.
  points = optionArgs{find(strcmp(optionArgs(1:2:end), 'EvaluationPoints'), 1) * 2};
  if isvector(points)
    count = 1;
  else
    count = size(points, 2);
  end
  series = struct('XData', zeros(1, 0), 'YData', zeros(1, 0), 'Labels', {{}}, ...
    'Positions', 1:count, 'Modes', {{'XDataMode', 'auto', 'YDataMode', 'auto'}});
end
%=============================================================================
function series = dataOnlySeries(y)
  if ~isnumeric(y)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if isvector(y)
    values = y(:);
    series = struct('XData', ones(size(values)), 'YData', values, 'Labels', {{}}, ...
      'Positions', 1, 'Modes', {{}});
    return
  end
  series = repmat(struct('XData', [], 'YData', [], 'Labels', {{}}, 'Positions', [], ...
    'Modes', {{}}), 1, size(y, 2));
  for k = 1:size(y, 2)
    values = y(:, k);
    series(k).Positions = k;
    series(k).XData = k * ones(size(values));
    series(k).YData = values;
  end
end
%=============================================================================
function series = groupedSeries(xgroup, y)
  if ~isnumeric(y)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  values = y(:);
  [x, labels] = groupPositions(xgroup, numel(values));
  series = struct('XData', x(:), 'YData', values, 'Labels', {labels}, ...
    'Positions', x(:), 'Modes', {{}});
end
%=============================================================================
function [x, labels] = groupPositions(xgroup, n)
  labels = {};
  if isnumeric(xgroup)
    if isscalar(xgroup)
      x = repmat(double(xgroup), n, 1);
    else
      x = double(xgroup(:));
    end
  else
    raw = xgroup(:);
    [labels, dummy, x] = unique(raw, 'stable');
    dummy;
  end
  if numel(x) ~= n
    error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
  end
end
%=============================================================================
function applyViolinAxes(ax, series, opts)
  labels = {};
  positions = [];
  for k = 1:numel(series)
    positions = [positions; series(k).Positions(:)];
    if ~isempty(series(k).Labels)
      labels = series(k).Labels;
    end
  end
  positions = unique(positions(isfinite(positions)));
  if ~isempty(positions)
    if strcmp(opts.Orientation, 'horizontal')
      ax.YLim = [min(positions) - 0.5, max(positions) + 0.5];
    else
      ax.XLim = [min(positions) - 0.5, max(positions) + 0.5];
    end
  end
  if ~isempty(labels)
    if strcmp(opts.Orientation, 'horizontal')
      ax.YTick = 1:numel(labels);
      ax.YTickLabel = labels(:);
    else
      ax.XTick = 1:numel(labels);
      ax.XTickLabel = labels(:);
    end
  end
  if strcmp(ax.ZLimMode, 'auto')
    ax.ZLim = [0, 1];
  end
  if strcmp(ax.ZTickMode, 'auto')
    ax.ZTick = [0, 0.5, 1];
  end
  ax.Box = 'off';
end
%=============================================================================
function opts = drawingOptions(args)
  opts = struct('FaceColor', [0.066 0.443 0.745], 'EdgeColor', [0.066 0.443 0.745], ...
    'FaceAlpha', 0.2, 'DensityWidth', 0.9, 'Orientation', 'vertical');
  for k = 1:2:numel(args)
    name = canonicalOptionName(args{k});
    value = args{k + 1};
    switch name
      case 'FaceColor'
        opts.FaceColor = validatecolor(value);
      case 'EdgeColor'
        opts.EdgeColor = validatecolor(value);
      case 'FaceAlpha'
        opts.FaceAlpha = double(value);
      case 'DensityWidth'
        opts.DensityWidth = double(value);
      case 'Orientation'
        opts.Orientation = lower(char(value));
    end
  end
end
%=============================================================================
function tf = isViolinOptionName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  try
    canonicalOptionName(value);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
function name = canonicalOptionName(value)
  valid = {'XData', 'YData', 'XDataMode', 'YDataMode', ...
    'FaceColor', 'FaceAlpha', 'EdgeColor', 'LineStyle', 'LineWidth', ...
    'Clipping', 'DensityWidth', 'ColorGroupWidth', 'Orientation', ...
    'ColorGroupLayout', 'DensityDirection', 'DensityScale', ...
    'FaceColorMode', 'EdgeColorMode', 'ColorGroupWidthMode', ...
    'DensityWidthMode', 'EvaluationPoints', 'EvaluationPointsMode', ...
    'DensityValues', 'DensityValuesMode', 'DisplayName', 'Visible', 'HandleVisibility', ...
    'Tag', 'UserData'};
  matches = find(strcmpi(char(value), valid));
  if isempty(matches)
    matches = find(strncmpi(char(value), valid, length(char(value))));
  end
  if isempty(matches)
    error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'violinplot');
  end
  if numel(matches) > 1
    error('Nelson:violinplot:ambiguousProperty', 'Ambiguous violinplot property.');
  end
  name = valid{matches};
end
%=============================================================================
