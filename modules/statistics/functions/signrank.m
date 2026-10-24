%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, h, stats] = signrank(x, y, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  if nargin < 2
    [diffValues, args] = pairedDifferencesLocal(x, [], varargin, false);
  else
    [diffValues, args] = pairedDifferencesLocal(x, y, varargin, true);
  end
  opts = parseOptionsLocal(convertStringToCharArgs(args));
  [signedRank, tieSum, hasTies, ranks] = signedRankLocal(diffValues);
  method = selectMethodLocal(opts.Method, length(ranks), hasTies);
  if strcmp(method, 'exact')
    p = exactPValueLocal(signedRank, length(ranks), opts.Tail);
    zval = [];
  else
    [p, zval] = approximatePValueLocal(signedRank, ranks, opts.Tail);
  end
  h = p <= opts.Alpha;
  stats = struct('signedrank', signedRank);
  if ~isempty(zval)
    stats.zval = zval;
  end
end
%=============================================================================
function [diffValues, args] = pairedDifferencesLocal(x, y, argsIn, hasY)
  args = argsIn;
  if ~hasY || isTextScalarLocal(y)
    if hasY
      args = [{y}, argsIn];
    end
    diffValues = validateVectorLocal(x, 'X');
  else
    diffValues = pairDifferenceLocal(x, y);
  end
  diffValues = diffValues(:);
  diffValues = diffValues(~isnan(diffValues));
  diffValues = diffValues(diffValues ~= 0);
  if isempty(diffValues)
    error('Nelson:statistics:xYNonzeroDifference', 'X and Y must contain at least one nonzero non-NaN difference.');
  end
end
%=============================================================================
function values = validateVectorLocal(values, name)
  if ~(isnumeric(values) && isreal(values) && ~issparse(values) && isvector(values))
    error('Nelson:statistics:mustBeRealVector', '%s must be a real numeric vector.', name);
  end
  values = double(values(:));
end
%=============================================================================
function diffValues = pairDifferenceLocal(x, y)
  x = validateVectorLocal(x, 'X');
  if ~(isnumeric(y) && isreal(y) && ~issparse(y))
    error('Nelson:statistics:yRealNumericScalarVector', '%s must be a real numeric scalar or vector.', 'Y');
  end
  if isscalar(y)
    diffValues = x - double(y);
  elseif isvector(y)
    y = double(y(:));
    if length(x) ~= length(y)
      error('Nelson:statistics:xYSameNumberObservations', 'X and Y must have %s.', 'the same number of observations');
    end
    diffValues = x - y;
  else
    error('Nelson:statistics:yRealNumericScalarVector', '%s must be a real numeric scalar or vector.', 'Y');
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
function [signedRank, tieSum, hasTies, ranks] = signedRankLocal(diffValues)
  [ranks, tieSum] = tiedRanksLocal(abs(diffValues));
  signedRank = sum(ranks(diffValues > 0));
  hasTies = tieSum ~= 0;
end
%=============================================================================
function method = selectMethodLocal(method, n, hasTies)
  if strcmp(method, 'exact') && hasTies
    error('Nelson:statistics:exactMethodSupportedTiesPresent', 'Exact method is not supported when ties are present.');
  end
  if strcmp(method, 'auto')
    if ~hasTies && n <= 20
      method = 'exact';
    else
      method = 'approximate';
    end
  end
end
%=============================================================================
function p = exactPValueLocal(signedRank, n, tail)
  counts = exactSignedRankCountsLocal(n);
  idx = round(signedRank) + 1;
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
function counts = exactSignedRankCountsLocal(n)
  maxSum = n * (n + 1) / 2;
  counts = zeros(1, maxSum + 1);
  counts(1) = 1;
  for r = 1:n
    counts(r + 1:end) = counts(r + 1:end) + counts(1:end - r);
  end
end
%=============================================================================
function [p, zval] = approximatePValueLocal(signedRank, ranks, tail)
  mu = sum(ranks) / 2;
  sigma = sqrt(sum(ranks .^ 2) / 4);
  if sigma == 0
    zval = 0;
    p = 1;
    return
  end
  switch tail
    case 'left'
      zval = (signedRank - mu + 0.5) / sigma;
      p = normcdf(zval, 0, 1);
    case 'right'
      zval = (signedRank - mu - 0.5) / sigma;
      p = normcdf(zval, 0, 1, 'upper');
    otherwise
      zval = (abs(signedRank - mu) - 0.5) / sigma;
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
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
