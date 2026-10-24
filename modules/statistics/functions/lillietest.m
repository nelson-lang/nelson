%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, kstat, critval] = lillietest(x, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 4);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin));
  values = validateSampleLocal(x);
  [probabilities, n] = fittedProbabilitiesLocal(values, opts.Distribution);
  kstat = kolmogorovStatisticLocal(probabilities);
  p = lilliePValueLocal(kstat, n);
  critval = criticalValueLocal(opts.Alpha, n);
  h = kstat > critval;
  opts.MCTol;
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Alpha', 0.05, 'Distribution', 'normal', 'MCTol', []);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  names = {'alpha', 'distribution', 'mctol'};
  k = 1;
  while k <= length(args)
    name = matchedNameLocal(args{k}, names);
    switch name
      case 'alpha'
        opts.Alpha = scalarInRangeLocal(args{k + 1}, 'ALPHA');
      case 'distribution'
        opts.Distribution = distributionNameLocal(args{k + 1});
      case 'mctol'
        opts.MCTol = scalarInRangeLocal(args{k + 1}, 'MCTOL');
    end
    k = k + 2;
  end
  if isempty(opts.MCTol) && (opts.Alpha < 0.001 || opts.Alpha > 0.5)
    error('Nelson:statistics:ALPHARange00010', 'ALPHA must be in the range [0.001,0.5] when MCTOL is not specified.');
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
      name = 'normal';
    case {'exponential', 'exp'}
      name = 'exponential';
    case {'extreme value', 'ev', 'extremevalue'}
      name = 'extreme value';
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
  if length(values) < 4
    error('Nelson:statistics:xContainLeastFourNon', 'X must contain at least four non-NaN observations.');
  end
end
%=============================================================================
function [probabilities, n] = fittedProbabilitiesLocal(values, distribution)
  values = sort(values(:));
  n = length(values);
  switch distribution
    case 'normal'
      sigma = std(values, 0);
      if sigma <= 0
        probabilities = zeros(size(values));
      else
        probabilities = normcdf((values - mean(values)) / sigma);
      end
    case 'exponential'
      if any(values < 0)
        error('Nelson:statistics:xNonnegativeExponentialDistribution', 'X must be nonnegative for the exponential distribution.');
      end
      mu = mean(values);
      probabilities = 1 - exp(-values / mu);
    otherwise
      sigma = std(values, 0) * sqrt(6) / pi;
      if sigma <= 0
        probabilities = zeros(size(values));
      else
        mu = mean(values) - 0.577215664901533 * sigma;
        probabilities = 1 - exp(-exp((values - mu) / sigma));
      end
  end
  probabilities = min(max(probabilities, 0), 1);
end
%=============================================================================
function kstat = kolmogorovStatisticLocal(probabilities)
  n = length(probabilities);
  upper = (1:n)' / n - probabilities;
  lower = probabilities - (0:n - 1)' / n;
  kstat = max(max(upper), max(lower));
end
%=============================================================================
function p = lilliePValueLocal(kstat, n)
  z = kstat * (sqrt(n) - 0.01 + 0.85 / sqrt(n));
  if z <= 0.302
    p = 1;
  elseif z <= 0.5
    p = polyval([81.218052 -138.55152 80.709644 -19.828315 2.76773], z);
  elseif z <= 0.9
    p = polyval([-32.355711 94.029866 -97.490286 40.662806 -4.901232], z);
  elseif z <= 1.31
    p = polyval([2.423045 -12.234627 23.186922 -19.558097 6.198765], z);
  else
    p = 0;
  end
  p = min(max(p, 0.001), 0.5);
end
%=============================================================================
function critval = criticalValueLocal(alpha, n)
  lo = 0;
  hi = 1;
  for k = 1:60
    mid = (lo + hi) / 2;
    if lilliePValueLocal(mid, n) > alpha
      lo = mid;
    else
      hi = mid;
    end
  end
  critval = hi;
end
%=============================================================================
function value = scalarInRangeLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > 0 && value < 1)
    error('Nelson:statistics:mustBeInRange', '%s must be a scalar in the range (0,1).', name);
  end
  value = double(value);
end
%=============================================================================
