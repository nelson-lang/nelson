%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = __nan_statistics__(x, method, weights, varargin)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  if isempty(x)
    y = cast(NaN, class(x));
    return
  end
  [dims, allValues, returnInput] = parseDimensionsLocal(x, varargin);
  if returnInput
    y = returnInputLocal(x, method);
  elseif strcmp(method, 'median')
    y = medianLocal(x, dims, allValues);
  elseif strcmp(method, 'mean')
    y = meanLocal(x, dims, allValues);
  else
    y = varianceFamilyLocal(x, method, weights, dims, allValues);
  end
  if isa(x, 'single') && ~isa(y, 'single')
    y = single(y);
  end
end
%=============================================================================
function y = meanLocal(x, dims, allValues)
  x = double(x);
  mask = ~isnan(x);
  x(~mask) = 0;
  n = reduceSumLocal(double(mask), dims, allValues);
  y = reduceSumLocal(x, dims, allValues) ./ n;
  y(n == 0) = NaN;
end
%=============================================================================
function y = varianceFamilyLocal(x, method, weights, dims, allValues)
  if isscalar(weights)
    y = scalarWeightedVarianceLocal(double(x), weights, dims, allValues);
  else
    if allValues || ~isscalar(dims)
      error('Nelson:statistics:vectorWeightsRequireScalarOperating', 'Vector weights require a scalar operating dimension.');
    end
    y = vectorWeightedVarianceLocal(double(x), double(weights(:)), dims);
  end
  if strcmp(method, 'std')
    y = sqrt(y);
  end
end
%=============================================================================
function y = scalarWeightedVarianceLocal(x, flag, dims, allValues)
  mask = ~isnan(x);
  x(~mask) = 0;
  n = reduceSumLocal(double(mask), dims, allValues);
  s = reduceSumLocal(x, dims, allValues);
  ss = reduceSumLocal(x .* x, dims, allValues);
  y = (ss - (s .* s) ./ n) ./ denominatorLocal(n, flag);
  y(n == 0) = NaN;
end
%=============================================================================
function y = vectorWeightedVarianceLocal(x, weights, dim)
  if length(weights) ~= size(x, dim)
    error('Nelson:statistics:weightsLengthMatchOperatingDimension', 'Weights length must match the operating dimension.');
  end
  nd = max(ndims(x), dim);
  shape = ones(1, nd);
  shape(dim) = length(weights);
  w = reshape(weights, shape);
  reps = size(x);
  reps(end + 1:nd) = 1;
  reps(dim) = 1;
  w = repmat(w, reps);
  valid = ~isnan(x) & ~isnan(w);
  x(~valid) = 0;
  w(~valid) = 0;
  if any(w(:) < 0)
    error('Nelson:statistics:weights01NonnegativeVector', 'Weights must be 0, 1, or a nonnegative vector.');
  end
  sw = sum(w, dim);
  sx = sum(w .* x, dim);
  sxx = sum(w .* x .* x, dim);
  y = (sxx - (sx .* sx) ./ sw) ./ sw;
  y(sw == 0) = NaN;
end
%=============================================================================
function d = denominatorLocal(n, flag)
  if flag == 0
    d = n - 1;
    idx = n <= 1;
    if any(idx(:))
      d(idx) = n(idx);
    end
  else
    d = n;
  end
end
%=============================================================================
function y = medianLocal(x, dims, allValues)
  if allValues
    values = double(x(:));
    values = values(~isnan(values));
    y = medianVectorLocal(values);
  else
    y = medianAlongDimensionsLocal(double(x), dims);
  end
end
%=============================================================================
function y = medianAlongDimensionsLocal(x, dims)
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
    column = xr(:, k);
    values(k) = medianVectorLocal(column(~isnan(column)));
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  y = ipermute(reshape(values, permutedOutSize), perm);
  y = reshape(y, outSize);
end
%=============================================================================
function value = medianVectorLocal(values)
  if isempty(values)
    value = NaN;
  else
    value = median(values);
  end
end
%=============================================================================
function y = reduceSumLocal(x, dims, allValues)
  if allValues
    y = sum(x, 'all');
    return
  end
  if isscalar(dims)
    y = sum(x, dims);
    return
  end
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
  values = sum(reshape(xp, n, cols), 1);
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  y = ipermute(reshape(values, permutedOutSize), perm);
  y = reshape(y, outSize);
end
%=============================================================================
function [dims, allValues, returnInput] = parseDimensionsLocal(x, args)
  dims = firstNonsingletonLocal(size(x));
  allValues = false;
  returnInput = false;
  if isempty(args)
    return
  end
  if length(args) > 1
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  value = args{1};
  if isTextScalarLocal(value)
    text = lower(char(value));
    if strcmp(text, 'all')
      dims = [];
      allValues = true;
    else
      error('Nelson:statistics:invalidOption', 'Invalid option.');
    end
  else
    [dims, returnInput] = parseDimensionLocal(value, size(x));
  end
end
%=============================================================================
function [dims, returnInput] = parseDimensionLocal(value, sz)
  returnInput = false;
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
function y = returnInputLocal(x, method)
  if strcmp(method, 'mean') || strcmp(method, 'median')
    y = x;
  else
    y = zeros(size(x), class(x));
    y(isnan(x)) = NaN;
  end
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
