%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, h, stats] = signtest(x, y, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  if nargin < 2
    [diffValues, args] = differencesLocal(x, [], varargin, false);
  else
    [diffValues, args] = differencesLocal(x, y, varargin, true);
  end
  opts = parseOptionsLocal(convertStringToCharArgs(args));
  nPositive = sum(diffValues > 0);
  n = length(diffValues);
  method = selectMethodLocal(opts.Method, n);
  if strcmp(method, 'exact')
    p = exactPValueLocal(nPositive, n, opts.Tail);
    zval = NaN;
  else
    [p, zval] = approximatePValueLocal(nPositive, n, opts.Tail);
  end
  h = p <= opts.Alpha;
  stats = struct('sign', nPositive, 'zval', zval);
end
%=============================================================================
function [diffValues, args] = differencesLocal(x, y, argsIn, hasY)
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
  k = 1;
  while k <= length(args)
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
    k = k + 2;
  end
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
function method = selectMethodLocal(method, n)
  if strcmp(method, 'auto')
    if n < 100
      method = 'exact';
    else
      method = 'approximate';
    end
  end
end
%=============================================================================
function p = exactPValueLocal(nPositive, n, tail)
  switch tail
    case 'right'
      p = binocdf(nPositive - 1, n, 0.5, 'upper');
    case 'left'
      p = binocdf(nPositive, n, 0.5);
    otherwise
      lower = binocdf(nPositive, n, 0.5);
      upper = binocdf(nPositive - 1, n, 0.5, 'upper');
      p = min(1, 2 * min(lower, upper));
  end
end
%=============================================================================
function [p, zval] = approximatePValueLocal(nPositive, n, tail)
  mu = n / 2;
  sigma = sqrt(n / 4);
  switch tail
    case 'right'
      zval = (nPositive - mu - 0.5) / sigma;
      p = normcdf(zval, 0, 1, 'upper');
    case 'left'
      zval = (nPositive - mu + 0.5) / sigma;
      p = normcdf(zval, 0, 1);
    otherwise
      correction = 0.5 * sign(nPositive - mu);
      zval = (nPositive - mu - correction) / sigma;
      p = 2 * normcdf(-abs(zval), 0, 1);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
