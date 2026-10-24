%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = boxplot(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  options = defaultBoxOptions('boxplot');
  [data, groupData, args] = parseBoxplotData(args);
  [options, labels] = parseBoxOptions(options, args);
  [groups, labels] = makeBoxGroups(data, groupData, labels);
  h = boxPlotBase(parent, groups, labels, options);
  if nargout == 1
    varargout{1} = h;
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
function [data, groupData, args] = parseBoxplotData(args)
  if isempty(args) || ~isnumeric(args{1})
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  data = args{1};
  args = args(2:end);
  groupData = [];
  if ~isempty(args) && ~isBoxOptionName(args{1})
    if (ischar(args{1}) || isStringScalar(args{1})) && numel(args) >= 2
      error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'boxplot');
    end
    groupData = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [options, labels] = parseBoxOptions(options, args)
  labels = [];
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'labels'
        labels = labelsFromValue(value);
      case 'orientation'
        options.Orientation = validateOrientation(value);
      case 'widths'
        options.Widths = validatePositiveVector(value, 'Widths');
      case 'whisker'
        options.Whisker = validateNonnegativeScalar(value, 'Whisker');
      case 'symbol'
        options = applyOutlierSymbol(options, value);
      case {'colors', 'color'}
        options.Colors = validateColors(value);
      case 'linewidth'
        options.LineWidth = validatePositiveScalar(value, 'LineWidth');
      case 'showoutliers'
        options.ShowOutliers = validateOnOff(value);
      case 'notch'
        options.Notch = validateOnOff(value);
      case 'positions'
        options.Positions = validatePositions(value);
      case 'boxstyle'
        options.BoxStyle = validateBoxStyle(value);
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'boxplot');
    end
  end
end
%=============================================================================
function [groups, labels] = makeBoxGroups(data, groupData, labels)
  if isempty(groupData)
    data = double(data);
    if isvector(data)
      groups = {data(:)};
    else
      groups = cell(1, size(data, 2));
      for k = 1:size(data, 2)
        groups{k} = data(:, k);
      end
    end
    if isempty(labels)
      labels = defaultLabels(numel(groups));
    end
    validateLabelCount(labels, numel(groups));
    return
  end
  values = double(data(:));
  if numel(values) ~= numel(groupData)
    error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
  end
  groupLabels = labelsFromValue(groupData);
  [uniqueLabels, groupIndex] = stableUniqueLabels(groupLabels);
  groups = cell(1, numel(uniqueLabels));
  for k = 1:numel(uniqueLabels)
    groups{k} = values(groupIndex == k);
  end
  if isempty(labels)
    labels = uniqueLabels;
  end
  validateLabelCount(labels, numel(groups));
end
%=============================================================================
function tf = isBoxOptionName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  names = {'labels', 'orientation', 'widths', 'whisker', 'symbol', 'colors', ...
    'color', 'linewidth', 'showoutliers', 'notch', 'positions', 'boxstyle'};
  tf = any(strcmpi(char(value), names));
end
%=============================================================================
function options = defaultBoxOptions(chartType)
  options = struct('ChartType', chartType, 'Orientation', 'vertical', ...
    'Width', 0.5, 'Whisker', 1.5, 'BoxFaceColor', [0 0.447 0.741], ...
    'BoxFaceAlpha', 1, 'EdgeColor', [0 0.447 0.741], ...
    'MedianColor', [0.85 0.325 0.098], 'WhiskerColor', [0 0.447 0.741], ...
    'WhiskerLineStyle', '--', 'OutlierColor', [1 0 0], 'Symbol', '+', ...
    'ShowOutliers', 'on', 'LineWidth', 0.5, 'Notch', 'off', 'Positions', [], ...
    'BoxStyle', 'outline', 'Colors', [], 'Widths', 0.5);
end
%=============================================================================
function labels = defaultLabels(n)
  labels = cell(n, 1);
  for k = 1:n
    labels{k} = num2str(k);
  end
end
%=============================================================================
function labels = labelsFromValue(value)
  if iscellstr(value)
    labels = value(:);
  elseif isstring(value)
    labels = cellstr(value(:));
  elseif ischar(value)
    labels = cellstr(value);
  elseif isnumeric(value) || islogical(value)
    value = value(:);
    labels = cell(numel(value), 1);
    for k = 1:numel(value)
      labels{k} = num2str(value(k));
    end
  else
    error('Nelson:boxplot:invalidLabels', 'Labels must be character, string, cell string, or numeric values.');
  end
end
%=============================================================================
function [uniqueLabels, index] = stableUniqueLabels(labels)
  uniqueLabels = {};
  index = zeros(numel(labels), 1);
  for k = 1:numel(labels)
    found = 0;
    for j = 1:numel(uniqueLabels)
      if strcmp(labels{k}, uniqueLabels{j})
        found = j;
        break
      end
    end
    if found == 0
      uniqueLabels{end + 1, 1} = labels{k};
      found = numel(uniqueLabels);
    end
    index(k) = found;
  end
end
%=============================================================================
function validateLabelCount(labels, count)
  if numel(labels) ~= count
    error('Nelson:boxplot:invalidNumberOfLabels', 'Number of labels must match number of groups.');
  end
end
%=============================================================================
function value = validateOrientation(value)
  value = lower(char(value));
  if ~(strcmp(value, 'vertical') || strcmp(value, 'horizontal'))
    error('Nelson:boxplot:invalidOrientation', 'Orientation must be ''vertical'' or ''horizontal''.');
  end
end
%=============================================================================
function value = validateOnOff(value)
  value = lower(char(value));
  if ~(strcmp(value, 'on') || strcmp(value, 'off'))
    error('Nelson:graphics:invalidOnOffValue', 'Value must be ''on'' or ''off''.');
  end
end
%=============================================================================
function value = validatePositiveScalar(value, name)
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value <= 0
    error('Nelson:graphics:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
end
%=============================================================================
function value = validatePositiveVector(value, name)
  if ~isnumeric(value) || ~isvector(value) || isempty(value) || any(~isfinite(value(:))) ...
      || any(value(:) <= 0)
    error('Nelson:boxplot:mustBePositiveFiniteVector', '%s must be a positive finite numeric vector.', name);
  end
  value = double(value(:).');
end
%=============================================================================
function value = validateNonnegativeScalar(value, name)
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value < 0
    error('Nelson:boxplot:mustBeNonnegativeFinite', '%s must be a nonnegative finite scalar.', name);
  end
end
%=============================================================================
function color = validateColor(value)
  if isnumeric(value)
    if ~isequal(size(value), [1 3]) || any(value < 0) || any(value > 1)
      error('Nelson:boxplot:invalidColor', '%s must be an RGB triplet.', 'Color');
    end
    color = value;
  else
    color = getColorShortName(char(value));
  end
end
%=============================================================================
function colors = validateColors(value)
  if isnumeric(value)
    if size(value, 2) ~= 3 || isempty(value) || any(value(:) < 0) || any(value(:) > 1)
      error('Nelson:boxplot:invalidColors', 'Colors must be a color string or an RGB matrix.');
    end
    colors = double(value);
  elseif ischar(value) || isStringScalar(value)
    text = char(value);
    if isempty(text)
      error('Nelson:boxplot:invalidColors', 'Colors must be a color string or an RGB matrix.');
    end
    for k = 1:numel(text)
      getColorShortName(text(k));
    end
    colors = text;
  else
    error('Nelson:boxplot:invalidColors', 'Colors must be a color string or an RGB matrix.');
  end
end
%=============================================================================
function value = validatePositions(value)
  if ~isnumeric(value) || ~isvector(value) || isempty(value) || any(~isfinite(value(:)))
    error('Nelson:boxplot:invalidPositions', 'Positions must be a finite numeric vector.');
  end
  value = double(value(:).');
end
%=============================================================================
function value = validateBoxStyle(value)
  value = lower(char(value));
  if ~(strcmp(value, 'filled') || strcmp(value, 'outline'))
    error('Nelson:boxplot:invalidBoxStyle', 'BoxStyle must be ''filled'' or ''outline''.');
  end
end
%=============================================================================
function options = applyOutlierSymbol(options, value)
  [~, color, marker, msg] = colstyle(char(value), 'plot', false);
  if ~isempty(msg)
    error('Nelson:boxplot:invalidLineSpec', msg);
  end
  if ~isempty(marker)
    options.Symbol = marker;
  end
  if ~isempty(color)
    options.OutlierColor = color;
  end
end
%=============================================================================
