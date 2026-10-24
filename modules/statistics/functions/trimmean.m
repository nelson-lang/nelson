%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = trimmean(x, percent, varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  percent = parsePercentLocal(percent);
  if isempty(x)
    m = cast(NaN, class(x));
    return
  end
  [flag, dims, allValues, returnInput] = parseInputsLocal(x, varargin);
  if returnInput
    m = x;
  elseif allValues
    m = trimVectorLocal(double(x(:)), percent, flag);
  else
    m = trimAlongDimensionsLocal(double(x), percent, flag, dims);
  end
  if isa(x, 'single') && ~isa(m, 'single')
    m = single(m);
  end
end
%=============================================================================
function percent = parsePercentLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value)) ...
      || ~isfinite(value) || value < 0 || value > 100
    error('Nelson:statistics:PERCENTScalarBetween0100', 'PERCENT must be a scalar between 0 and 100.');
  end
  percent = double(value);
end
%=============================================================================
function [flag, dims, allValues, returnInput] = parseInputsLocal(x, args)
  flag = 'round';
  dims = firstNonsingletonLocal(size(x));
  allValues = false;
  returnInput = false;
  hasDimension = false;
  hasFlag = false;
  for index = 1:length(args)
    value = args{index};
    if isTextScalarLocal(value) && isTrimFlagLocal(char(value))
      if hasFlag
        error('Nelson:statistics:invalidTrimmingFlag', 'Invalid trimming flag.');
      end
      flag = lower(char(value));
      hasFlag = true;
    else
      if hasDimension
        error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
      end
      [dims, allValues, returnInput] = parseDimensionLocal(value, size(x));
      hasDimension = true;
    end
  end
end
%=============================================================================
function [dims, allValues, returnInput] = parseDimensionLocal(value, sz)
  allValues = false;
  returnInput = false;
  if isTextScalarLocal(value)
    text = lower(char(value));
    if strcmp(text, 'all')
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
  rank = max(2, length(sz));
  if isscalar(dims) && (dims > rank || dimensionLengthLocal(sz, dims) == 1)
    returnInput = true;
  elseif all(dims > rank)
    returnInput = true;
  else
    dims = dims(dims <= rank);
  end
end
%=============================================================================
function m = trimAlongDimensionsLocal(x, percent, flag, dims)
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
    values(k) = trimVectorLocal(xr(:, k), percent, flag);
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  m = ipermute(reshape(values, permutedOutSize), perm);
  m = reshape(m, outSize);
end
%=============================================================================
function value = trimVectorLocal(values, percent, flag)
  values = sort(values(~isnan(values)));
  n = length(values);
  if n == 0
    value = NaN;
    return
  end
  k = n * percent / 200;
  if strcmp(flag, 'weighted')
    value = weightedTrimLocal(values, k);
  else
    trimCount = trimCountLocal(k, flag);
    if n - 2 * trimCount <= 0
      value = NaN;
    else
      value = mean(values((trimCount + 1):(n - trimCount)));
    end
  end
end
%=============================================================================
function count = trimCountLocal(k, flag)
  if strcmp(flag, 'floor')
    count = floor(k);
  else
    count = ceil(k - 0.5);
    count = max(count, 0);
  end
end
%=============================================================================
function value = weightedTrimLocal(values, k)
  n = length(values);
  i = floor(k);
  f = k - i;
  denominator = n - 2 * k;
  if denominator <= 0
    value = NaN;
    return
  end
  first = i + 1;
  last = n - i;
  if f == 0
    value = mean(values(first:last));
  else
    value = (sum(values((first + 1):(last - 1))) + (1 - f) * (values(first) + values(last))) ...
        / denominator;
  end
end
%=============================================================================
function tf = isTrimFlagLocal(text)
  text = lower(text);
  tf = strcmp(text, 'round') || strcmp(text, 'floor') || strcmp(text, 'weighted');
end
%=============================================================================
function value = dimensionLengthLocal(sz, dim)
  if dim <= length(sz)
    value = sz(dim);
  else
    value = 1;
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
