%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, stats] = fishertest(x, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin));
  tableCounts = validateTableLocal(x);
  p = fisherPValueLocal(tableCounts, opts.Tail);
  h = p <= opts.Alpha;
  oddsRatio = oddsRatioLocal(tableCounts);
  ci = confidenceIntervalLocal(tableCounts, opts.Alpha, oddsRatio);
  stats = struct('OddsRatio', oddsRatio, 'ConfidenceInterval', ci);
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'Tail', 'both');
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
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function counts = validateTableLocal(x)
  if istable(x)
    x = table2array(x);
  end
  if ~(isnumeric(x) && isreal(x) && isequal(size(x), [2 2]))
    error('Nelson:statistics:x22NumericMatrix', 'X must be a 2-by-2 numeric matrix or table.');
  end
  counts = double(x);
  if any(counts(:) < 0) || any(counts(:) ~= fix(counts(:))) || any(~isfinite(counts(:)))
    error('Nelson:statistics:xContainNonnegativeIntegerCounts', 'X must contain nonnegative integer counts.');
  end
  if sum(counts(:)) == 0
    error('Nelson:statistics:xContainLeastOneObservation', 'X must contain at least one observation.');
  end
end
%=============================================================================
function p = fisherPValueLocal(counts, tail)
  a = counts(1, 1);
  row1 = sum(counts(1, :));
  row2 = sum(counts(2, :));
  col1 = sum(counts(:, 1));
  lo = max(0, col1 - row2);
  hi = min(row1, col1);
  values = lo:hi;
  logp = arrayfun(@(v) hypergeometricLogProbabilityLocal(v, row1, col1, row1 + row2), values);
  probs = exp(logp - max(logp));
  probs = probs / sum(probs);
  observed = probs(values == a);
  switch tail
    case 'right'
      p = sum(probs(values >= a));
    case 'left'
      p = sum(probs(values <= a));
    otherwise
      p = sum(probs(probs <= observed + 100 * eps));
  end
  p = min(max(p, 0), 1);
end
%=============================================================================
function value = hypergeometricLogProbabilityLocal(a, row1, col1, total)
  value = gammaln(col1 + 1) - gammaln(a + 1) - gammaln(col1 - a + 1) ...
    + gammaln(total - col1 + 1) - gammaln(row1 - a + 1) ...
    - gammaln(total - col1 - row1 + a + 1) ...
    - gammaln(total + 1) + gammaln(row1 + 1) + gammaln(total - row1 + 1);
end
%=============================================================================
function value = oddsRatioLocal(counts)
  if counts(1, 2) == 0 || counts(2, 1) == 0
    if counts(1, 1) == 0 || counts(2, 2) == 0
      value = NaN;
    else
      value = Inf;
    end
  else
    value = counts(1, 1) * counts(2, 2) / (counts(1, 2) * counts(2, 1));
  end
end
%=============================================================================
function ci = confidenceIntervalLocal(counts, alpha, oddsRatio)
  if any(counts(:) == 0) || isnan(oddsRatio)
    ci = [-Inf Inf];
    return
  end
  logOdds = log(oddsRatio);
  se = sqrt(sum(1 ./ counts(:)));
  z = norminv(1 - alpha / 2, 0, 1);
  ci = exp([logOdds - z * se, logOdds + z * se]);
end
%=============================================================================
function value = parseAlphaLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value > 0 && value < 1)
    error('Nelson:statistics:alphaOutOfRange', '%s must be a scalar in the range (0,1).', 'ALPHA');
  end
  value = double(value);
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
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
