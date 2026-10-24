%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, stats] = runstest(x, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  [mode, reference, args] = parseLeadingInputsLocal(x, convertStringToCharArgs(varargin));
  opts = parseOptionsLocal(args);
  signs = signsLocal(x, mode, reference);
  [nruns, n1, n0] = runCountsLocal(signs);
  method = selectMethodLocal(opts.Method, mode, length(signs));
  z = zStatisticLocal(nruns, n1, n0);
  if strcmp(method, 'exact') && strcmp(mode, 'ab')
    p = exactPValueLocal(nruns, n1, n0, opts.Tail);
  else
    p = approximatePValueLocal(z, opts.Tail);
  end
  h = p <= opts.Alpha;
  stats = struct('nruns', nruns, 'n1', n1, 'n0', n0, 'z', z);
end
%=============================================================================
function [mode, reference, args] = parseLeadingInputsLocal(x, argsIn)
  mode = 'ab';
  reference = [];
  args = argsIn;
  if isempty(argsIn)
    reference = meanOmitNaNLocal(x);
    return
  end
  first = argsIn{1};
  if isTextScalarLocal(first)
    if strcmpi(char(first), 'ud')
      mode = 'ud';
      args = argsIn(2:end);
    else
      reference = meanOmitNaNLocal(x);
    end
  elseif isnumeric(first) && isreal(first) && isscalar(first)
    reference = double(first);
    args = argsIn(2:end);
  else
    error('Nelson:statistics:referenceValueRealNumericScalar', 'Reference value must be a real numeric scalar or ''ud''.');
  end
  if isempty(reference) && strcmp(mode, 'ab')
    reference = meanOmitNaNLocal(x);
  end
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'Method', 'auto', 'Tail', 'both');
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  k = 1;
  while k <= length(args)
    name = lower(char(args{k}));
    switch name
      case 'alpha'
        opts.Alpha = parseAlphaLocal(args{k + 1});
      case 'method'
        opts.Method = parseChoiceLocal(args{k + 1}, {'auto', 'exact', 'approximate'}, 'METHOD');
      case 'tail'
        opts.Tail = parseChoiceLocal(args{k + 1}, {'both', 'right', 'left'}, 'TAIL');
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
    k = k + 2;
  end
end
%=============================================================================
function signs = signsLocal(x, mode, reference)
  values = validateVectorLocal(x);
  if strcmp(mode, 'ud')
    d = diff(values);
    d = d(~isnan(d) & d ~= 0);
    signs = d > 0;
  else
    values = values(~isnan(values));
    values = values(values ~= reference);
    signs = values > reference;
  end
  if length(signs) < 2 || all(signs) || ~any(signs)
    error('Nelson:statistics:xContainLeastOneValue', 'X must contain at least one value in each run category.');
  end
end
%=============================================================================
function values = validateVectorLocal(x)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && isvector(x))
    error('Nelson:statistics:xRealNumericVector', '%s must be a real numeric vector.', 'X');
  end
  values = double(x(:));
end
%=============================================================================
function [nruns, n1, n0] = runCountsLocal(signs)
  signs = logical(signs(:));
  n1 = sum(signs);
  n0 = length(signs) - n1;
  nruns = 1 + sum(signs(2:end) ~= signs(1:end - 1));
end
%=============================================================================
function method = selectMethodLocal(method, mode, n)
  if strcmp(method, 'auto')
    if strcmp(mode, 'ud') && n > 50
      method = 'approximate';
    elseif strcmp(mode, 'ab')
      method = 'exact';
    else
      method = 'approximate';
    end
  end
  if strcmp(mode, 'ud') && strcmp(method, 'exact')
    method = 'approximate';
  end
end
%=============================================================================
function z = zStatisticLocal(nruns, n1, n0)
  n = n1 + n0;
  mu = 2 * n1 * n0 / n + 1;
  variance = 2 * n1 * n0 * (2 * n1 * n0 - n) / (n ^ 2 * (n - 1));
  if variance <= 0
    z = NaN;
  else
    z = (nruns - mu) / sqrt(variance);
  end
end
%=============================================================================
function p = approximatePValueLocal(z, tail)
  switch tail
    case 'right'
      p = normcdf(z, 0, 1, 'upper');
    case 'left'
      p = normcdf(z, 0, 1);
    otherwise
      p = 2 * normcdf(-abs(z), 0, 1);
  end
end
%=============================================================================
function p = exactPValueLocal(nruns, n1, n0, tail)
  lo = 2;
  hi = 2 * min(n1, n0) + double(n1 ~= n0);
  runs = lo:hi;
  probs = zeros(size(runs));
  for k = 1:length(runs)
    probs(k) = exactRunProbabilityLocal(runs(k), n1, n0);
  end
  switch tail
    case 'right'
      p = sum(probs(runs >= nruns));
    case 'left'
      p = sum(probs(runs <= nruns));
    otherwise
      left = sum(probs(runs <= nruns));
      right = sum(probs(runs >= nruns));
      p = min(1, 2 * min(left, right));
  end
end
%=============================================================================
function p = exactRunProbabilityLocal(runs, n1, n0)
  totalLog = gammaln(n1 + n0 + 1) - gammaln(n1 + 1) - gammaln(n0 + 1);
  if rem(runs, 2) == 0
    k = runs / 2;
    value = 2 * combinationLocal(n1 - 1, k - 1) * combinationLocal(n0 - 1, k - 1);
  else
    k = (runs - 1) / 2;
    value = combinationLocal(n1 - 1, k) * combinationLocal(n0 - 1, k - 1) ...
      + combinationLocal(n1 - 1, k - 1) * combinationLocal(n0 - 1, k);
  end
  p = value / exp(totalLog);
end
%=============================================================================
function c = combinationLocal(n, k)
  if k < 0 || k > n
    c = 0;
  else
    c = exp(gammaln(n + 1) - gammaln(k + 1) - gammaln(n - k + 1));
  end
end
%=============================================================================
function value = meanOmitNaNLocal(x)
  values = validateVectorLocal(x);
  values = values(~isnan(values));
  if isempty(values)
    error('Nelson:statistics:xAllNaN', 'X must contain at least one non-NaN value.');
  end
  value = mean(values);
end
%=============================================================================
function value = parseAlphaLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > 0 && value < 1)
    error('Nelson:statistics:alphaOutOfRange', '%s must be a scalar in the range (0,1).', 'ALPHA');
  end
  value = double(value);
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
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
