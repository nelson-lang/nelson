%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tbl = tabulate(x)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ~(isnumeric(x) || islogical(x) || ischar(x) || isstring(x) || iscellstr(x) || iscategorical(x))
    error('Nelson:statistics:xVectorTextArray', '%s must be a vector or text array.', 'X');
  end
  if isnumeric(x)
    tbl = tabulateNumericLocal(x);
  elseif islogical(x)
    tbl = tabulateLogicalLocal(x);
  else
    tbl = tabulateLabelTableLocal(tabulateInputLabelsLocal(x));
  end
  if nargout == 0
    disp(tbl);
  end
end
%=============================================================================
function tbl = tabulateLogicalLocal(x)
  if ~isvector(x)
    error('Nelson:statistics:xVectorTextArray', '%s must be a vector or text array.', 'X');
  end
  values = x(:);
  groups = {false; true};
  counts = [sum(values == false); sum(values == true)];
  keep = counts > 0;
  groups = groups(keep);
  counts = counts(keep);
  tbl = cell(length(groups), 3);
  for k = 1:length(groups)
    tbl{k, 1} = groups{k};
    tbl{k, 2} = counts(k);
    tbl{k, 3} = counts(k) * 100 / length(values);
  end
end
%=============================================================================
function tbl = tabulateNumericLocal(x)
  if ~isvector(x)
    error('Nelson:statistics:xVectorTextArray', '%s must be a vector or text array.', 'X');
  end
  values = double(x(:));
  values = values(~isnan(values));
  if isempty(values)
    tbl = zeros(0, 3);
    return
  end
  sortedValues = sort(values);
  starts = [1; find(diff(sortedValues) ~= 0) + 1];
  stops = [starts(2:end) - 1; length(sortedValues)];
  if all(values == fix(values)) && all(values > 0)
    groups = (1:max(values))';
    counts = zeros(length(groups), 1);
    for k = 1:length(starts)
      counts(sortedValues(starts(k))) = stops(k) - starts(k) + 1;
    end
  else
    groups = sortedValues(starts);
    counts = zeros(length(groups), 1);
    for k = 1:length(starts)
      counts(k) = stops(k) - starts(k) + 1;
    end
  end
  percent = counts * 100 / length(values);
  tbl = [groups, counts, percent];
end
%=============================================================================
function labels = tabulateInputLabelsLocal(x)
  if iscategorical(x)
    labels = cellstr(x(:));
  elseif ischar(x)
    labels = cellstr(x);
  elseif isstring(x)
    labels = cellstr(x(:));
  elseif islogical(x)
    labels = num2cell(x(:));
  else
    labels = x(:);
  end
end
%=============================================================================
function tbl = tabulateLabelTableLocal(labels)
  labels = labels(:);
  if isempty(labels)
    tbl = cell(0, 3);
    return
  end
  [groups, ~, ic] = unique(labels, 'stable');
  counts = zeros(length(groups), 1);
  for k = 1:length(groups)
    counts(k) = sum(ic == k);
  end
  tbl = cell(length(groups), 3);
  for k = 1:length(groups)
    tbl{k, 1} = groups{k};
    tbl{k, 2} = counts(k);
    tbl{k, 3} = counts(k) * 100 / length(labels);
  end
end
%=============================================================================
