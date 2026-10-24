%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = discretize(X, edges, varargin)
  narginchk(2, 6);
  nargoutchk(0, 2);
  if ~isempty(varargin) && isstruct(varargin{end}) && isscalar(varargin{end})
    varargin = [varargin(1:end - 1), namedargs2cell(varargin{end})];
  end
  if isdatetime(X) && localIsTextScalar(edges)
    varargout{1} = localDatetimeUnitDiscretize(X, char(edges), varargin{:});
    return
  end
  if isduration(X) && localIsTextScalar(edges)
    varargout{1} = localDurationUnitDiscretize(X, char(edges), varargin{:});
    return
  end
  if ~isnumeric(X) || ~isnumeric(edges) || ~isvector(edges)
    error('Nelson:data_analysis:numericInputDataNumericEdge', 'Numeric input data and a numeric edge vector are expected.');
  end
  edges = double(edges(:).');
  if isscalar(edges)
    % A scalar second argument is a requested number of bins. Reuse the
    % histcounts bin picker so discretize(X, N) and histcounts(X, N) agree on
    % the automatically chosen edges.
    nbins = edges;
    if ~isfinite(nbins) || nbins < 1 || nbins ~= fix(nbins)
      error('Nelson:data_analysis:numberBinsPositiveInteger', '%s must be a positive integer scalar.', 'Number of bins');
    end
    [~, edges] = histcounts(double(X), nbins);
    edges = double(edges(:).');
  end
  if length(edges) < 2 || any(diff(edges) <= 0)
    error('Nelson:data_analysis:edgesStrictlyIncreasingVector', 'Edges must be a strictly increasing vector.');
  end
  [outputMode, outputValues, includedEdge] = localParseOptions(varargin, false);
  nBins = length(edges) - 1;
  bin = localDiscretizeBins(double(X), edges, includedEdge);
  if strcmp(outputMode, 'indices')
    Y = bin;
  elseif strcmp(outputMode, 'categorical')
    values = localValues(outputValues, nBins);
    Y = localCategoricalBins(bin, values);
  else
    values = localValues(outputValues, nBins);
    Y = localValueBins(bin, values);
  end
  varargout{1} = Y;
  if nargout > 1
    varargout{2} = edges;
  end
end
%=============================================================================
function Y = localDatetimeUnitDiscretize(X, unit, varargin)
  if ~strcmpi(unit, 'month')
    error('Nelson:data_analysis:unsupportedDatetimeDiscretizeUnit', 'Unsupported "%s" option.', char(unit));
  end
  [outputMode, outputValues] = localParseOptions(varargin, true);
  monthIndex = year(X) .* 12 + month(X);
  valid = ~isnat(X);
  bin = NaN(size(X));
  if any(valid(:))
    firstMonth = min(monthIndex(valid));
    lastMonth = max(monthIndex(valid));
    bin(valid) = monthIndex(valid) - firstMonth + 1;
    labels = localMonthLabels(firstMonth:lastMonth);
  else
    labels = {};
  end
  if strcmp(outputMode, 'indices')
    Y = bin;
    return
  end
  if isempty(outputValues)
    values = labels;
  else
    values = localValues(outputValues, length(labels));
  end
  if strcmp(outputMode, 'categorical')
    Y = localCategoricalBins(bin, values);
  else
    Y = localValueBins(bin, values);
  end
end
%=============================================================================
function Y = localDurationUnitDiscretize(X, unit, varargin)
  if ~strcmpi(unit, 'hour')
    error('Nelson:data_analysis:unsupportedDurationDiscretizeUnit', 'Unsupported "%s" option.', char(unit));
  end
  [outputMode, outputValues, includedEdge] = localParseOptions(varargin, true);
  unitSeconds = 3600;
  XSeconds = seconds(X);
  valid = ~isnan(XSeconds);
  bin = NaN(size(XSeconds));
  if any(valid(:))
    firstEdge = floor(min(XSeconds(valid)) / unitSeconds) * unitSeconds;
    lastEdge = ceil(max(XSeconds(valid)) / unitSeconds) * unitSeconds;
    if firstEdge == lastEdge
      lastEdge = firstEdge + unitSeconds;
    end
    edges = firstEdge:unitSeconds:lastEdge;
    bin = localDiscretizeBins(XSeconds, edges, includedEdge);
    labels = localDurationLabels(edges, outputValues, includedEdge);
  else
    labels = {};
  end
  if strcmp(outputMode, 'indices')
    Y = bin;
    return
  end
  if isempty(outputValues) || localIsTextScalar(outputValues)
    values = labels;
  else
    values = localValues(outputValues, length(labels));
  end
  if strcmp(outputMode, 'categorical')
    Y = localCategoricalBins(bin, values);
  else
    Y = localValueBins(bin, values);
  end
end
%=============================================================================
function labels = localDurationLabels(edges, formatValue, includedEdge)
  if isempty(formatValue)
    formatValue = 'hh:mm:ss';
  else
    formatValue = char(formatValue);
  end
  nBins = length(edges) - 1;
  labels = cell(nBins, 1);
  for k = 1:nBins
    if strcmpi(includedEdge, 'right')
      leftBracket = '(';
      rightBracket = ']';
      if k == 1
        leftBracket = '[';
      end
    else
      leftBracket = '[';
      rightBracket = ')';
      if k == nBins
        rightBracket = ']';
      end
    end
    labels{k} = sprintf('%s%s, %s%s', leftBracket, ...
      localFormatDurationEdge(edges(k), formatValue), ...
      localFormatDurationEdge(edges(k + 1), formatValue), rightBracket);
  end
end
%=============================================================================
function text = localFormatDurationEdge(value, formatValue)
  if strcmpi(formatValue, 'm')
    text = sprintf('%g min', value / 60);
  elseif strcmpi(formatValue, 'hh:mm:ss')
    signText = '';
    if value < 0
      signText = '-';
    end
    value = abs(value);
    hoursValue = floor(value / 3600);
    minutesValue = floor((value - hoursValue * 3600) / 60);
    secondsValue = round(value - hoursValue * 3600 - minutesValue * 60);
    text = sprintf('%s%02d:%02d:%02d', signText, hoursValue, minutesValue, secondsValue);
  else
    error('Nelson:data_analysis:unsupportedDurationLabelFormat', 'Unsupported duration label format.');
  end
end
%=============================================================================
function labels = localMonthLabels(monthIndexes)
  shortNames = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
  labels = cell(length(monthIndexes), 1);
  for k = 1:length(monthIndexes)
    yearValue = floor((monthIndexes(k) - 1) / 12);
    monthValue = monthIndexes(k) - yearValue * 12;
    labels{k} = sprintf('%s-%04d', shortNames{monthValue}, yearValue);
  end
end
%=============================================================================
function [outputMode, outputValues, includedEdge] = localParseOptions(varargin, allowAutoCategorical)
  outputMode = 'indices';
  outputValues = [];
  includedEdge = 'left';
  k = 1;
  while k <= length(varargin)
    arg = varargin{k};
    if (ischar(arg) || (isstring(arg) && isscalar(arg))) && strcmpi(char(arg), 'categorical')
      outputMode = 'categorical';
      if k < length(varargin) && ~localIsOptionName(varargin{k + 1})
        outputValues = varargin{k + 1};
        k = k + 2;
      elseif allowAutoCategorical
        k = k + 1;
      else
        error('Nelson:data_analysis:categoryNamesExpected', 'Category names are expected.');
      end
    elseif (ischar(arg) || (isstring(arg) && isscalar(arg))) && strcmpi(char(arg), 'IncludedEdge')
      if k == length(varargin)
        error('Nelson:data_analysis:includedEdgeValueExpected', 'IncludedEdge value is expected.');
      end
      includedEdge = char(varargin{k + 1});
      k = k + 2;
    elseif isempty(outputValues)
      outputMode = 'values';
      outputValues = arg;
      k = k + 1;
    else
      error('Nelson:data_analysis:invalidDiscretizeInputArguments', 'Invalid discretize input arguments.');
    end
  end
  if ~strcmpi(includedEdge, 'left') && ~strcmpi(includedEdge, 'right')
    error('Nelson:data_analysis:includedEdgeLeftRight', 'IncludedEdge must be left or right.');
  end
end
%=============================================================================
function tf = localIsOptionName(value)
  tf = localIsTextScalar(value) && strcmpi(char(value), 'IncludedEdge');
end
%=============================================================================
function tf = localIsTextScalar(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
function bin = localDiscretizeBins(X, edges, includedEdge)
  % Bin assignment via a vectorized searchsorted over the strictly increasing
  % edges, O((n + nEdges) * log) instead of scanning every bin for every point.
  sz = size(X);
  Xv = double(X(:));
  edges = double(edges(:));
  nBins = numel(edges) - 1;
  lastEdge = edges(end);
  firstEdge = edges(1);
  if strcmpi(includedEdge, 'left')
    % bin = number of edges <= x; x on the last edge joins the last bin.
    c = localCountEdges(edges, Xv, true);
    bin = c;
    onLast = (Xv == lastEdge);
    bin(onLast) = nBins;
    bin(c < 1) = NaN;
    bin(c > nBins & ~onLast) = NaN;
  else
    % bin = number of edges < x; x on the first edge joins the first bin.
    c = localCountEdges(edges, Xv, false);
    bin = c;
    onFirst = (Xv == firstEdge);
    bin(onFirst) = 1;
    bin(c < 1 & ~onFirst) = NaN;
    bin(c > nBins) = NaN;
  end
  bin(isnan(Xv)) = NaN;
  bin = reshape(bin, sz);
end
%=============================================================================
function cnt = localCountEdges(edges, X, includeEqual)
  % cnt(i) = #{e in edges : e <= X(i)} when includeEqual, else e < X(i). Merges
  % the two sorted-by-value sets and counts preceding edge entries per point.
  edges = edges(:);
  X = X(:);
  m = numel(edges);
  n = numel(X);
  both = [edges; X];
  if includeEqual
    sec = [zeros(m, 1); ones(n, 1)];
  else
    sec = [ones(m, 1); zeros(n, 1)];
  end
  [~, sidx] = sortrows([both, sec]);
  isEdge = [true(m, 1); false(n, 1)];
  isEdgeSorted = isEdge(sidx);
  cumEdges = cumsum(isEdgeSorted);
  isXSorted = ~isEdgeSorted;
  cntSorted = cumEdges(isXSorted);
  xOrig = sidx(isXSorted) - m;
  cnt = zeros(n, 1);
  cnt(xOrig) = cntSorted;
end
%=============================================================================
function values = localValues(inputValues, nBins)
  if isempty(inputValues)
    values = cellstr(string(1:nBins));
  elseif isstring(inputValues)
    values = cellstr(inputValues(:));
  elseif iscellstr(inputValues)
    values = inputValues(:);
  elseif isnumeric(inputValues)
    if numel(inputValues) ~= nBins
      error('Nelson:data_analysis:numberValuesMatchNumberBins', 'Number of values must match the number of bins.');
    end
    values = inputValues(:);
    return
  else
    error('Nelson:data_analysis:unsupportedDiscretizeOutputValues', 'Unsupported discretize output values.');
  end
  if length(values) ~= nBins
    error('Nelson:data_analysis:numberValuesMatchNumberBins', 'Number of values must match the number of bins.');
  end
end
%=============================================================================
function Y = localCategoricalBins(bin, values)
  labels = repmat({''}, size(bin));
  for k = 1:numel(bin)
    if ~isnan(bin(k))
      labels{k} = values{bin(k)};
    end
  end
  Y = categorical(labels, values, values, 'Protected', true);
end
%=============================================================================
function Y = localValueBins(bin, values)
  if isnumeric(values)
    Y = NaN(size(bin));
    for k = 1:numel(bin)
      if ~isnan(bin(k))
        Y(k) = values(bin(k));
      end
    end
  else
    Y = repmat({''}, size(bin));
    for k = 1:numel(bin)
      if ~isnan(bin(k))
        Y{k} = values{bin(k)};
      end
    end
  end
end
%=============================================================================
