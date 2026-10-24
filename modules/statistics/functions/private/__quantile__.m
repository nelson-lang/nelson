%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Q = __quantile__(A, probs, args, allowCount, percentInput)
  if ~(isnumeric(A) && isreal(A) && ~issparse(A))
    error('Nelson:statistics:aMustBeRealArray', '%s must be a real numeric array.', 'A');
  end
  [p, method] = parseProbabilitiesLocal(probs, allowCount, percentInput);
  [dims, allValues, method] = parseArgsLocal(A, args, method);
  if allValues
    Q = quantileVectorLocal(double(A(:)), p, method);
    Q = Q(:);
    return
  end
  Q = quantileAlongDimensionsLocal(double(A), p, dims, method);
end
%=============================================================================
function [p, method] = parseProbabilitiesLocal(value, allowCount, percentInput)
  method = 'midpoint';
  if ~(isnumeric(value) && isreal(value) && (isscalar(value) || isvector(value)))
    error('Nelson:statistics:probabilitiesRealNumericScalarVector', '%s must be a real numeric scalar or vector.', 'Probabilities');
  end
  value = double(value(:).');
  if allowCount && isscalar(value) && value > 1 && isfinite(value) && value == fix(value)
    if value <= 1
      error('Nelson:statistics:numberQuantilesIntegerGreaterOne', 'Number of quantiles must be an integer greater than one.');
    end
    p = (1:value) ./ (value + 1);
    return
  end
  if percentInput
    if any(~isfinite(value)) || any(value < 0) || any(value > 100)
      error('Nelson:statistics:percentageOutOfRange', 'Percentages must be in the range [0,100].');
    end
    p = value ./ 100;
  else
    if any(~isfinite(value)) || any(value < 0) || any(value > 1)
      error('Nelson:statistics:probabilityOutOfRange', '%s must be in the range [0,1].', 'Probabilities');
    end
    p = value;
  end
end
%=============================================================================
function [dims, allValues, method] = parseArgsLocal(A, args, method)
  dims = firstNonsingletonLocal(size(A));
  allValues = false;
  k = 1;
  while k <= length(args)
    arg = args{k};
    if isTextScalarLocal(arg) && strcmpi(char(arg), 'method')
      if k == length(args)
        error('Nelson:statistics:methodRequiresValue', 'Method option requires a value.');
      end
      method = parseMethodLocal(args{k + 1});
      k = k + 2;
    elseif isTextScalarLocal(arg)
      if strcmpi(char(arg), 'all')
        dims = [];
        allValues = true;
        k = k + 1;
      else
        error('Nelson:statistics:invalidQuantileOption', 'Invalid quantile option.');
      end
    elseif isnumeric(arg)
      [dims, allValues] = parseDimensionLocal(arg);
      k = k + 1;
    else
      error('Nelson:statistics:invalidQuantileOption', 'Invalid quantile option.');
    end
  end
end
%=============================================================================
function method = parseMethodLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Method');
  end
  method = lower(char(value));
  switch method
    case {'midpoint', 'exact', 'inclusive', 'exclusive', 'approximate'}
    otherwise
      error('Nelson:statistics:invalidQuantileMethod', 'Invalid quantile method.');
  end
end
%=============================================================================
function [dims, allValues] = parseDimensionLocal(value)
  allValues = false;
  if ~(isnumeric(value) && isreal(value) && isvector(value) && all(value == fix(value)) ...
      && all(value >= 1))
    error('Nelson:statistics:invalidDimensionArg:dimensionArgumentAPositiveIntegerScalar', 'Dimension argument must be a positive integer scalar, vector, or ''all''.');
  end
  dims = unique(double(value(:).'), 'stable');
end
%=============================================================================
function Q = quantileAlongDimensionsLocal(A, p, dims, method)
  originalRank = max(2, length(size(A)));
  sz = size(A);
  nd = max([length(sz), max(dims)]);
  sz(end + 1:nd) = 1;
  keepDims = setdiff(1:nd, dims, 'stable');
  perm = [dims, keepDims];
  Ap = permute(reshape(A, sz), perm);
  psize = size(Ap);
  psize(end + 1:nd) = 1;
  n = prod(psize(1:length(dims)));
  cols = prod(psize(length(dims) + 1:end));
  Ar = reshape(Ap, n, cols);
  values = NaN(length(p), cols);
  for k = 1:cols
    values(:, k) = quantileVectorLocal(Ar(:, k), p, method);
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize(dims(1)) = length(p);
  outSize = outSize(1:max(originalRank, dims(1)));
  permutedOutSize = [length(p), ones(1, length(dims) - 1), psize(length(dims) + 1:end)];
  Q = ipermute(reshape(values, permutedOutSize), perm);
  Q = reshape(Q, outSize);
end
%=============================================================================
function q = quantileVectorLocal(values, p, method)
  values = sort(values(~isnan(values)));
  n = length(values);
  q = NaN(length(p), 1);
  if n == 0
    return
  end
  if n == 1
    q(:) = values(1);
    return
  end
  pos = positionsLocal(p, n, method);
  for j = 1:length(p)
    q(j) = interpolateLocal(values, pos(j));
  end
end
%=============================================================================
function pos = positionsLocal(p, n, method)
  switch lower(method)
    case {'midpoint', 'exact', 'approximate'}
      pos = n .* p + 0.5;
    case 'inclusive'
      pos = (n - 1) .* p + 1;
    case 'exclusive'
      pos = (n + 1) .* p;
    otherwise
      error('Nelson:statistics:invalidQuantileMethod', 'Invalid quantile method.');
  end
end
%=============================================================================
function value = interpolateLocal(values, pos)
  n = length(values);
  if pos <= 1
    value = values(1);
  elseif pos >= n
    value = values(n);
  else
    lo = floor(pos);
    hi = ceil(pos);
    if lo == hi
      value = values(lo);
    else
      value = values(lo) + (pos - lo) .* (values(hi) - values(lo));
    end
  end
end
%=============================================================================
function dim = firstNonsingletonLocal(sz)
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
