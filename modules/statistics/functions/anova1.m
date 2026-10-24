%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, tbl, stats] = anova1(X, group, displayopt)
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
  [values, labels] = groupedValuesLocal(X, group);
  result = anovaTableLocal(values, labels);
  p = result.p;
  tbl = result.tbl;
  stats = result.stats;
  if strcmp(displayopt, 'on')
    disp(tbl);
  end
end
%=============================================================================
function [values, labels] = groupedValuesLocal(X, group)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericVectorMatrix', '%s must be a real numeric vector or matrix.', 'X');
  end
  X = double(X);
  if isempty(group)
    [values, labels] = matrixGroupsLocal(X);
  else
    [values, labels] = explicitGroupsLocal(X, group);
  end
end
%=============================================================================
function [values, labels] = matrixGroupsLocal(X)
  ncols = size(X, 2);
  values = X(:);
  groupIndex = reshape(repmat(1:ncols, size(X, 1), 1), [], 1);
  labels = cell(ncols, 1);
  for k = 1:ncols
    labels{k} = int2str(k);
  end
  keep = ~isnan(values);
  values = values(keep);
  groupIndex = groupIndex(keep);
  [values, labels] = compactGroupsLocal(values, groupIndex, labels);
end
%=============================================================================
function [values, labels] = explicitGroupsLocal(X, group)
  if isvector(X)
    values = X(:);
    [codes, labels, keep] = groupCodesLocal(group, numel(values));
  else
    labelsByColumn = inputLabelsLocal(group, size(X, 2));
    values = X(:);
    columnCodes = reshape(repmat((1:size(X, 2)), size(X, 1), 1), [], 1);
    groupValues = labelsByColumn(columnCodes);
    [codes, labels, keep] = groupCodesFromLabelsLocal(groupValues);
  end
  keep = keep & ~isnan(values);
  values = values(keep);
  codes = codes(keep);
  [values, labels] = compactGroupsLocal(values, codes, labels);
end
%=============================================================================
function [values, labels] = compactGroupsLocal(values, codes, labels)
  counts = zeros(length(labels), 1);
  for k = 1:length(labels)
    counts(k) = sum(codes == k);
  end
  keepGroups = counts > 0;
  map = zeros(length(labels), 1);
  map(keepGroups) = 1:sum(keepGroups);
  labels = labels(keepGroups);
  codes = map(codes);
  codes = codes(:);
  values = [values(:), codes];
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
function result = anovaTableLocal(grouped, labels)
  values = grouped(:, 1);
  codes = grouped(:, 2);
  ngroups = length(labels);
  if ngroups < 2 || length(values) <= ngroups
    error('Nelson:statistics:leastTwoGroupsMoreObservations', 'At least two groups with more observations than groups are required.');
  end
  [counts, means, ssw] = groupSummaryLocal(values, codes, ngroups);
  grandMean = sum(values) / length(values);
  ssb = sum(counts .* (means - grandMean) .^ 2);
  sst = sum((values - grandMean) .^ 2);
  dfb = ngroups - 1;
  dfw = length(values) - ngroups;
  msb = ssb / dfb;
  msw = ssw / dfw;
  fstat = msb / msw;
  p = fcdf(fstat, dfb, dfw, 'upper');
  result.tbl = cellTableLocal(ssb, ssw, sst, dfb, dfw, msb, msw, fstat, p);
  result.p = p;
  result.stats = struct('gnames', {labels(:)}, 'n', counts(:)', 'source', 'anova1', ...
    'means', means(:)', 'df', dfw, 's', sqrt(msw));
end
%=============================================================================
function [counts, means, ssw] = groupSummaryLocal(values, codes, ngroups)
  counts = zeros(ngroups, 1);
  sums = zeros(ngroups, 1);
  sumsquares = zeros(ngroups, 1);
  for k = 1:ngroups
    rows = codes == k;
    counts(k) = sum(rows);
    sums(k) = sum(values(rows));
    sumsquares(k) = sum(values(rows) .^ 2);
  end
  means = sums ./ counts;
  ssw = sum(sumsquares - counts .* means .^ 2);
  if ssw < 0 && abs(ssw) < 100 * eps(max(sumsquares))
    ssw = 0;
  end
end
%=============================================================================
function tbl = cellTableLocal(ssb, ssw, sst, dfb, dfw, msb, msw, fstat, p)
  tbl = {'Source', 'SS', 'df', 'MS', 'F', 'Prob>F'; ...
         'Groups', ssb, dfb, msb, fstat, p; ...
         'Error', ssw, dfw, msw, [], []; ...
         'Total', sst, dfb + dfw, [], [], []};
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
