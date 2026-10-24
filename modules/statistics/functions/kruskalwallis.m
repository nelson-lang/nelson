%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, tbl, stats] = kruskalwallis(X, group, displayopt)
  narginchk(1, 3);
  nargoutchk(0, 3);
  hasDisplay = nargin >= 3;
  if nargin < 2
    group = [];
  elseif nargin == 2 && isDisplayOptionLocal(group)
    displayopt = group;
    group = [];
    hasDisplay = true;
  end
  if ~hasDisplay
    displayopt = 'on';
  end
  displayopt = parseDisplayLocal(displayopt);
  [values, codes, labels] = groupedValuesLocal(X, group);
  result = kruskalTableLocal(values, codes, labels);
  p = result.p;
  tbl = result.tbl;
  stats = result.stats;
  if strcmp(displayopt, 'on')
    disp(tbl);
  end
end
%=============================================================================
function [values, codes, labels] = groupedValuesLocal(X, group)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericVectorMatrix', '%s must be a real numeric vector or matrix.', 'X');
  end
  X = double(X);
  if isempty(group)
    [values, codes, labels] = matrixGroupsLocal(X);
  else
    [values, codes, labels] = explicitGroupsLocal(X, group);
  end
end
%=============================================================================
function [values, codes, labels] = matrixGroupsLocal(X)
  ncols = size(X, 2);
  values = X(:);
  codes = reshape(repmat(1:ncols, size(X, 1), 1), [], 1);
  labels = cell(ncols, 1);
  for k = 1:ncols
    labels{k} = int2str(k);
  end
  keep = ~isnan(values);
  [values, codes, labels] = compactGroupsLocal(values(keep), codes(keep), labels);
end
%=============================================================================
function [values, codes, labels] = explicitGroupsLocal(X, group)
  if isvector(X)
    values = X(:);
    [codes, labels, keep] = groupCodesLocal(group, numel(values));
  else
    labelsByColumn = inputLabelsLocal(group, size(X, 2));
    values = X(:);
    columnCodes = reshape(repmat(1:size(X, 2), size(X, 1), 1), [], 1);
    groupValues = labelsByColumn(columnCodes);
    [codes, labels, keep] = groupCodesFromLabelsLocal(groupValues);
  end
  keep = keep & ~isnan(values);
  [values, codes, labels] = compactGroupsLocal(values(keep), codes(keep), labels);
end
%=============================================================================
function [values, codes, labels] = compactGroupsLocal(values, codes, labels)
  counts = zeros(length(labels), 1);
  for k = 1:length(labels)
    counts(k) = sum(codes == k);
  end
  keepGroups = counts > 0;
  map = zeros(length(labels), 1);
  map(keepGroups) = 1:sum(keepGroups);
  labels = labels(keepGroups);
  codes = map(codes);
  values = values(:);
  codes = codes(:);
end
%=============================================================================
function [codes, labels, keep] = groupCodesLocal(group, n)
  rawLabels = inputLabelsLocal(group, n);
  [codes, labels, keep] = groupCodesFromLabelsLocal(rawLabels);
end
%=============================================================================
function [codes, labels, keep] = groupCodesFromLabelsLocal(rawLabels)
  keep = ~strcmp(rawLabels(:), '#<missing>');
  [labels, ~, ic] = unique(rawLabels(keep));
  codes = zeros(length(rawLabels), 1);
  codes(keep) = ic(:);
end
%=============================================================================
function labels = inputLabelsLocal(group, n)
  if iscategorical(group)
    labels = cellstr(group(:));
    labels(strcmp(labels, '<undefined>')) = {'#<missing>'};
  elseif isstring(group)
    labels = cellstr(group(:));
  elseif ischar(group)
    labels = cellstr(group);
  elseif iscellstr(group)
    labels = group(:);
  elseif isnumeric(group) || islogical(group)
    flat = group(:);
    labels = cell(length(flat), 1);
    for k = 1:length(flat)
      if isnumeric(flat) && isnan(flat(k))
        labels{k} = '#<missing>';
      else
        labels{k} = mat2str(flat(k));
      end
    end
  else
    error('Nelson:statistics:GROUPVectorTextArray', '%s must be a vector or text array.', 'GROUP');
  end
  if length(labels) ~= n
    error('Nelson:statistics:GROUPExpectedNumberLabels', 'GROUP must have the expected number of labels.');
  end
end
%=============================================================================
function result = kruskalTableLocal(values, codes, labels)
  ngroups = length(labels);
  n = length(values);
  if ngroups < 2 || n <= ngroups
    error('Nelson:statistics:leastTwoGroupsMoreObservations', 'At least two groups with more observations than groups are required.');
  end
  [ranks, tieCorrection] = tiedRanksLocal(values);
  [counts, rankSums, meanRanks] = rankSummaryLocal(ranks, codes, ngroups);
  [h, p] = kruskalStatisticLocal(rankSums, counts, n, ngroups, tieCorrection);
  [ssGroups, ssError, ssTotal] = rankSumsOfSquaresLocal(rankSums, counts, n);
  tbl = cellTableLocal(ssGroups, ssError, ssTotal, ngroups, n, h, p);
  result.p = p;
  result.tbl = tbl;
  result.stats = struct('gnames', {labels(:)}, 'n', counts(:)', 'source', 'kruskalwallis', ...
    'meanranks', meanRanks(:)', 'sumranks', rankSums(:)', 'df', ngroups - 1);
end
%=============================================================================
function [h, p] = kruskalStatisticLocal(rankSums, counts, n, ngroups, tieCorrection)
  h = 12 / (n * (n + 1)) * sum((rankSums .^ 2) ./ counts) - 3 * (n + 1);
  if tieCorrection == 0
    h = 0;
  else
    h = h / tieCorrection;
  end
  p = chi2cdf(h, ngroups - 1, 'upper');
end
%=============================================================================
function [ssGroups, ssError, ssTotal] = rankSumsOfSquaresLocal(rankSums, counts, n)
  ssGroups = sum((rankSums .^ 2) ./ counts) - n * (n + 1) ^ 2 / 4;
  ssTotal = n * (n + 1) * (n - 1) / 12;
  ssError = ssTotal - ssGroups;
end
%=============================================================================
function tbl = cellTableLocal(ssGroups, ssError, ssTotal, ngroups, n, h, p)
  tbl = {'Source', 'SS', 'df', 'Chi-sq', 'Prob>Chi-sq'; ...
         'Groups', ssGroups, ngroups - 1, h, p; ...
         'Error', ssError, n - ngroups, [], []; ...
         'Total', ssTotal, n - 1, [], []};
end
%=============================================================================
function [ranks, tieCorrection] = tiedRanksLocal(values)
  [sortedValues, order] = sort(values(:));
  n = length(sortedValues);
  ranks = zeros(n, 1);
  tieSum = 0;
  starts = [1; find(diff(sortedValues) ~= 0) + 1];
  stops = [starts(2:end) - 1; n];
  for k = 1:length(starts)
    rows = starts(k):stops(k);
    ranks(order(rows)) = (starts(k) + stops(k)) / 2;
    tieLength = stops(k) - starts(k) + 1;
    tieSum = tieSum + tieLength ^ 3 - tieLength;
  end
  tieCorrection = 1 - tieSum / (n ^ 3 - n);
end
%=============================================================================
function [counts, rankSums, meanRanks] = rankSummaryLocal(ranks, codes, ngroups)
  counts = zeros(ngroups, 1);
  rankSums = zeros(ngroups, 1);
  for k = 1:ngroups
    rows = codes == k;
    counts(k) = sum(rows);
    rankSums(k) = sum(ranks(rows));
  end
  meanRanks = rankSums ./ counts;
end
%=============================================================================
function opt = parseDisplayLocal(value)
  if ~isDisplayOptionLocal(value)
    error('Nelson:statistics:displayOptionOff', 'Display option must be ''on'' or ''off''.');
  end
  opt = lower(char(value));
end
%=============================================================================
function tf = isDisplayOptionLocal(value)
  tf = false;
  if isTextScalarLocal(value)
    text = lower(char(value));
    tf = strcmp(text, 'on') || strcmp(text, 'off');
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
