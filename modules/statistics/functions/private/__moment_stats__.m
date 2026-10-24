%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = __moment_stats__(x, order, varargin)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  if isempty(x)
    y = NaN;
    return
  end
  [flag, dims, allValues] = parseInputsLocal(x, varargin);
  if allValues
    y = momentVectorLocal(double(x(:)), flag, order);
  else
    y = momentAlongDimensionsLocal(double(x), flag, dims, order);
  end
  if isa(x, 'single')
    y = single(y);
  end
end
%=============================================================================
function [flag, dims, allValues] = parseInputsLocal(x, args)
  flag = 1;
  dims = firstNonsingletonLocal(size(x));
  allValues = false;
  if isempty(args)
    return
  end
  flag = parseFlagLocal(args{1});
  if length(args) >= 2
    [dims, allValues] = parseDimensionLocal(args{2});
  end
end
%=============================================================================
function flag = parseFlagLocal(value)
  if isempty(value)
    flag = 1;
    return
  end
  if ~(isnumeric(value) || islogical(value)) || ~isscalar(value) || ~(value == 0 || value == 1)
    error('Nelson:statistics:invalidFlag', '%s must be 0 or 1.', 'Flag');
  end
  flag = double(value);
end
%=============================================================================
function [dims, allValues] = parseDimensionLocal(value)
  allValues = false;
  if isTextScalarLocal(value)
    if strcmpi(char(value), 'all')
      dims = [];
      allValues = true;
      return
    end
    error('Nelson:statistics:dimensionPositiveIntegerScalarVector', 'Dimension must be a positive integer scalar, vector, or ''all''.');
  end
  if ~(isnumeric(value) && isreal(value) && isvector(value) && ~isempty(value) ...
      && all(value == fix(value)) && all(value >= 1))
    error('Nelson:statistics:dimensionPositiveIntegerScalarVector', 'Dimension must be a positive integer scalar, vector, or ''all''.');
  end
  dims = unique(double(value(:).'), 'stable');
end
%=============================================================================
function y = momentAlongDimensionsLocal(x, flag, dims, order)
  originalRank = max(2, length(size(x)));
  sz = size(x);
  nd = max([length(sz), max(dims)]);
  sz(end + 1:nd) = 1;
  keepDims = setdiff(1:nd, dims, 'stable');
  perm = [dims, keepDims];
  xp = permute(reshape(x, sz), perm);
  psize = size(xp);
  psize(end + 1:nd) = 1;
  n = prod(psize(1:length(dims)));
  cols = prod(psize(length(dims) + 1:end));
  xr = reshape(xp, n, cols);
  values = NaN(1, cols);
  for k = 1:cols
    values(k) = momentVectorLocal(xr(:, k), flag, order);
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  y = ipermute(reshape(values, permutedOutSize), perm);
  y = reshape(y, outSize);
end
%=============================================================================
function value = momentVectorLocal(values, flag, order)
  values = values(~isnan(values));
  n = length(values);
  if n == 0
    value = NaN;
    return
  end
  centered = values - mean(values);
  m2 = mean(centered .^ 2);
  if ~(m2 > 0)
    value = NaN;
    return
  end
  if order == 3
    value = mean(centered .^ 3) / (m2 ^ 1.5);
    if flag == 0
      value = correctedSkewnessLocal(value, n);
    end
  else
    value = mean(centered .^ 4) / (m2 ^ 2);
    if flag == 0
      value = correctedKurtosisLocal(value, n);
    end
  end
end
%=============================================================================
function value = correctedSkewnessLocal(value, n)
  if n < 3
    value = NaN;
  else
    value = sqrt(n * (n - 1)) / (n - 2) * value;
  end
end
%=============================================================================
function value = correctedKurtosisLocal(value, n)
  if n < 4
    value = NaN;
  else
    value = ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * value - 3 * (n - 1)) + 3;
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
