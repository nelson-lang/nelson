%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, adstat, cv] = adtest(x, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 4);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin));
  values = validateSampleLocal(x);
  probabilities = fittedProbabilitiesLocal(values, opts.Distribution);
  adstat = andersonDarlingStatisticLocal(probabilities);
  adjusted = adjustedStatisticLocal(adstat, length(probabilities), opts.Asymptotic);
  p = adPValueLocal(adjusted);
  cv = criticalValueLocal(opts.Alpha, length(probabilities), opts.Asymptotic);
  h = p <= opts.Alpha;
  opts.MCTol;
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'Distribution', 'norm', 'MCTol', [], 'Asymptotic', false);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  names = {'alpha', 'distribution', 'mctol', 'asymptotic'};
  k = 1;
  while k <= length(args)
    name = matchedNameLocal(args{k}, names);
    switch name
      case 'alpha'
        opts.Alpha = scalarInRangeLocal(args{k + 1}, 'ALPHA');
      case 'distribution'
        opts.Distribution = distributionNameLocal(args{k + 1});
      case 'mctol'
        opts.MCTol = positiveScalarLocal(args{k + 1}, 'MCTOL');
      case 'asymptotic'
        opts.Asymptotic = logicalScalarLocal(args{k + 1}, 'ASYMPTOTIC');
    end
    k = k + 2;
  end
  if opts.Asymptotic && ~isempty(opts.MCTol)
    error('Nelson:statistics:ASYMPTOTICFalseMCTOLSpecified', 'ASYMPTOTIC must be false when MCTOL is specified.');
  end
end
%=============================================================================
function name = matchedNameLocal(value, names)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:statistics:nameValueArgumentNameText', 'Name-value argument name must be text.');
  end
  text = lower(char(value));
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function name = distributionNameLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:statistics:DISTRIBUTIONText', 'DISTRIBUTION must be text.');
  end
  text = lower(char(value));
  switch text
    case {'normal', 'norm'}
      name = 'norm';
    case {'exponential', 'exp'}
      name = 'exp';
    case {'extreme value', 'ev', 'extremevalue'}
      name = 'ev';
    case {'lognormal', 'logn'}
      name = 'logn';
    case {'weibull', 'wbl'}
      name = 'weibull';
    otherwise
      error('Nelson:statistics:DISTRIBUTIONUnsupportedValue', '%s has an unsupported value.', 'DISTRIBUTION');
  end
end
%=============================================================================
function values = validateSampleLocal(x)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && isvector(x))
    error('Nelson:statistics:xRealNumericVector', '%s must be a real numeric vector.', 'X');
  end
  values = double(x(:));
  values = values(~isnan(values));
  if length(values) < 3
    error('Nelson:statistics:xContainLeastThreeNon', 'X must contain at least three non-NaN observations.');
  end
end
%=============================================================================
function probabilities = fittedProbabilitiesLocal(values, distribution)
  values = sort(values(:));
  switch distribution
    case 'norm'
      probabilities = normalProbabilitiesLocal(values);
    case 'exp'
      if any(values < 0)
        error('Nelson:statistics:xNonnegativeExponentialDistribution', 'X must be nonnegative for the exponential distribution.');
      end
      probabilities = 1 - exp(-values / mean(values));
    case 'ev'
      probabilities = extremeValueProbabilitiesLocal(values);
    case 'logn'
      if any(values <= 0)
        error('Nelson:statistics:xPositiveLognormalDistribution', 'X must be positive for the %s distribution.', 'lognormal');
      end
      probabilities = normalProbabilitiesLocal(log(values));
    otherwise
      if any(values <= 0)
        error('Nelson:statistics:xPositiveWeibullDistribution', 'X must be positive for the %s distribution.', 'Weibull');
      end
      probabilities = extremeValueProbabilitiesLocal(log(values));
  end
  probabilities = min(max(probabilities, realmin), 1 - eps);
end
%=============================================================================
function probabilities = normalProbabilitiesLocal(values)
  sigma = std(values, 0);
  if sigma <= 0
    probabilities = 0.5 * ones(size(values));
  else
    probabilities = normcdf((values - mean(values)) / sigma);
  end
end
%=============================================================================
function probabilities = extremeValueProbabilitiesLocal(values)
  sigma = std(values, 0) * sqrt(6) / pi;
  if sigma <= 0
    probabilities = 0.5 * ones(size(values));
  else
    mu = mean(values) - 0.577215664901533 * sigma;
    probabilities = 1 - exp(-exp((values - mu) / sigma));
  end
end
%=============================================================================
function adstat = andersonDarlingStatisticLocal(probabilities)
  n = length(probabilities);
  weights = (2 * (1:n)' - 1) / n;
  adstat = -n - sum(weights .* (log(probabilities) + log(1 - probabilities(end:-1:1))));
end
%=============================================================================
function adjusted = adjustedStatisticLocal(adstat, n, asymptotic)
  if asymptotic
    adjusted = adstat;
  else
    adjusted = adstat * (1 + 0.75 / n + 2.25 / (n ^ 2));
  end
end
%=============================================================================
function p = adPValueLocal(a)
  if a < 0.2
    p = 1 - exp(-13.436 + 101.14 * a - 223.73 * a ^ 2);
  elseif a < 0.34
    p = 1 - exp(-8.318 + 42.796 * a - 59.938 * a ^ 2);
  elseif a < 0.6
    p = exp(0.9177 - 4.279 * a - 1.38 * a ^ 2);
  else
    p = exp(1.2937 - 5.709 * a + 0.0186 * a ^ 2);
  end
  p = min(max(p, 0), 1);
end
%=============================================================================
function cv = criticalValueLocal(alpha, n, asymptotic)
  lo = 0;
  hi = 10;
  for k = 1:80
    mid = (lo + hi) / 2;
    adjusted = adjustedStatisticLocal(mid, n, asymptotic);
    if adPValueLocal(adjusted) > alpha
      lo = mid;
    else
      hi = mid;
    end
  end
  cv = hi;
end
%=============================================================================
function value = scalarInRangeLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > 0 && value < 1)
    error('Nelson:statistics:mustBeInRange', '%s must be a scalar in the range (0,1).', name);
  end
  value = double(value);
end
%=============================================================================
function value = positiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > 0)
    error('Nelson:statistics:mustBePositive', '%s must be a positive scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = logicalScalarLocal(value, name)
  if ~(islogical(value) && isscalar(value))
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
  value = logical(value);
end
%=============================================================================
