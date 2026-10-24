%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, h, stats] = ranksum(x, y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 3);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin));
  [x, y] = validateSamplesLocal(x, y);
  [rankSum, ustat, tieSum, hasTies] = rankSumLocal(x, y);
  method = selectMethodLocal(opts.Method, length(x), length(y), hasTies);
  if strcmp(method, 'exact')
    p = exactPValueLocal(rankSum, length(x), length(y), opts.Tail);
    zval = [];
  else
    [p, zval] = approximatePValueLocal(ustat, length(x), length(y), tieSum, opts.Tail);
  end
  h = p <= opts.Alpha;
  stats = struct('ranksum', rankSum);
  if ~isempty(zval)
    stats.zval = zval;
  end
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'Tail', 'both', 'Method', 'auto');
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'alpha'
        opts.Alpha = parseAlphaLocal(args{k + 1});
      case 'tail'
        opts.Tail = parseChoiceLocal(args{k + 1}, {'both', 'right', 'left'}, 'TAIL');
      case 'method'
        opts.Method = parseChoiceLocal(args{k + 1}, {'auto', 'exact', 'approximate'}, 'METHOD');
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function alpha = parseAlphaLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value > 0 && value < 1)
    error('Nelson:statistics:alphaOutOfRange', '%s must be a scalar in the range (0,1).', 'ALPHA');
  end
  alpha = double(value);
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  text = lower(char(value));
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = matches{1};
end
%=============================================================================
function [x, y] = validateSamplesLocal(x, y)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && isvector(x))
    error('Nelson:statistics:xRealNumericVector', '%s must be a real numeric vector.', 'X');
  end
  if ~(isnumeric(y) && isreal(y) && ~issparse(y) && isvector(y))
    error('Nelson:statistics:yRealNumericVector', '%s must be a real numeric vector.', 'Y');
  end
  x = double(x(:));
  y = double(y(:));
  x = x(~isnan(x));
  y = y(~isnan(y));
  if isempty(x) || isempty(y)
    error('Nelson:statistics:xYContainLeastOne', '%s must contain at least one non-NaN observation.', 'X and Y');
  end
end
%=============================================================================
function [rankSum, ustat, tieSum, hasTies] = rankSumLocal(x, y)
  n1 = length(x);
  values = [x; y];
  labels = [ones(n1, 1); zeros(length(y), 1)];
  [ranks, tieSum] = tiedRanksLocal(values);
  rankSum = sum(ranks(labels == 1));
  ustat = rankSum - n1 * (n1 + 1) / 2;
  hasTies = tieSum ~= 0;
end
%=============================================================================
function method = selectMethodLocal(method, n1, n2, hasTies)
  if strcmp(method, 'exact') && hasTies
    error('Nelson:statistics:exactMethodSupportedTiesPresent', 'Exact method is not supported when ties are present.');
  end
  if strcmp(method, 'auto')
    if ~hasTies && n1 + n2 <= 20
      method = 'exact';
    else
      method = 'approximate';
    end
  end
end
%=============================================================================
function p = exactPValueLocal(rankSum, n1, n2, tail)
  counts = exactRankSumCountsLocal(n1, n2);
  minRankSum = n1 * (n1 + 1) / 2;
  idx = round(rankSum - minRankSum) + 1;
  total = sum(counts);
  switch tail
    case 'left'
      p = sum(counts(1:idx)) / total;
    case 'right'
      p = sum(counts(idx:end)) / total;
    otherwise
      lo = sum(counts(1:idx)) / total;
      hi = sum(counts(idx:end)) / total;
      p = min(1, 2 * min(lo, hi));
  end
end
%=============================================================================
function counts = exactRankSumCountsLocal(n1, n2)
  maxExtra = n1 * n2;
  states = zeros(n1 + 1, maxExtra + 1);
  states(1, 1) = 1;
  for r = 1:(n1 + n2)
    upper = min(r, n1);
    for k = upper:-1:1
      offset = r - k;
      if offset >= 0
        states(k + 1, offset + 1:end) = states(k + 1, offset + 1:end) ...
          + states(k, 1:end - offset);
      end
    end
  end
  counts = states(n1 + 1, :);
end
%=============================================================================
function [p, zval] = approximatePValueLocal(ustat, n1, n2, tieSum, tail)
  n = n1 + n2;
  mu = n1 * n2 / 2;
  variance = n1 * n2 / 12 * ((n + 1) - tieSum / (n * (n - 1)));
  sigma = sqrt(variance);
  if sigma == 0
    zval = 0;
    p = 1;
    return
  end
  switch tail
    case 'left'
      zval = (ustat - mu + 0.5) / sigma;
      p = normcdf(zval, 0, 1);
    case 'right'
      zval = (ustat - mu - 0.5) / sigma;
      p = normcdf(zval, 0, 1, 'upper');
    otherwise
      zval = (abs(ustat - mu) - 0.5) / sigma;
      p = 2 * normcdf(-abs(zval), 0, 1);
  end
end
%=============================================================================
function [ranks, tieSum] = tiedRanksLocal(values)
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
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
