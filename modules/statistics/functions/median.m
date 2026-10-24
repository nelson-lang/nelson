%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = median(x, dim, nanflag, varargin)
  narginchk(1, 3);
  args = {};
  if nargin >= 2
    args{end + 1} = dim;
  end
  if nargin >= 3
    args{end + 1} = nanflag;
  end
  [dim, allValues, omitNaN, hasDim] = medianParseInputs(x, args);
  % a sparse input is processed densely; the result stays sparse
  wasSparse = issparse(x);
  if wasSparse
    x = full(x);
  end
  % char input is processed as double; the result is char when the median is
  % an element of the data (odd number of values), double otherwise
  wasChar = ischar(x);
  if wasChar
    x = double(x);
  end
  if isequal(size(x), [0 0]) && ~hasDim
    % median([]) is NaN (with a dimension, [] follows the reduction rule);
    % the class of the input is preserved
    % (integer classes give 0, since NaN cannot be represented).
    r = medianCastResult(NaN, x);
  elseif allValues
    r = medianCastResult(medianVector(x(:), omitNaN), x);
  elseif isscalar(dim)
    r = medianAlongDimension(x, dim, omitNaN);
  else
    r = medianAlongDimensions(x, dim, omitNaN);
  end
  if wasChar && mod(medianReducedCount(x, dim, allValues), 2) == 1
    r = char(r);
  end
  if wasSparse
    r = sparse(r);
  end
end
%=============================================================================
function n = medianReducedCount(x, dim, allValues)
  % number of values each median is computed from
  if allValues
    n = numel(x);
  else
    sz = size(x);
    sz(end + 1:max(dim)) = 1;
    n = prod(sz(dim));
  end
end
%=============================================================================
function r = medianCastResult(r, x)
  % preserve the class of the input: integer and logical classes are
  % rounded (half away from zero), single stays single.
  if isinteger(x) || islogical(x)
    r = cast(round(r), class(x));
  elseif isa(x, 'single')
    r = single(r);
  end
end
%=============================================================================
function [dim, allValues, omitNaN, hasDim] = medianParseInputs(x, args)
  dim = medianFirstNonsingleton(size(x));
  hasDim = false;
  allValues = false;
  omitNaN = false;
  for k = 1:length(args)
    arg = args{k};
    if ischar(arg) || isstring(arg)
      value = char(arg);
      if strcmp(value, 'all')
        allValues = true;
      elseif strcmp(value, 'omitnan') || strcmp(value, 'omitmissing')
        omitNaN = true;
      elseif strcmp(value, 'includenan') || strcmp(value, 'includemissing')
        omitNaN = false;
      else
        error('Nelson:statistics:unsupportedMedianOption', 'Unsupported %s.', 'median option');
      end
    elseif isnumeric(arg) && isscalar(arg)
      dim = arg;
      hasDim = true;
    elseif isnumeric(arg) && isvector(arg) && ~isempty(arg)
      if any(arg < 1) || any(arg ~= floor(arg))
        error('Nelson:statistics:dimensionArgumentPositiveIntegerScalar', 'Dimension argument must be %s.', 'a positive integer scalar or vector of positive integers');
      end
      dim = arg;
      hasDim = true;
    else
      error('Nelson:statistics:unsupportedMedianOption', 'Unsupported %s.', 'median option');
    end
  end
end
%=============================================================================
function dim = medianFirstNonsingleton(sz)
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
function r = medianAlongDimension(x, dim, omitNaN)
  sz = size(x);
  if dim > length(sz)
    r = x;
    return
  end
  perm = 1:length(sz);
  perm([1 dim]) = [dim 1];
  xp = permute(x, perm);
  psz = size(xp);
  n = psz(1);
  cols = prod(psz(2:end));
  xr = reshape(xp, n, cols);
  values = zeros(1, cols);
  for k = 1:cols
    values(k) = medianVector(xr(:, k), omitNaN);
  end
  outSize = psz;
  outSize(1) = 1;
  r = ipermute(reshape(values, outSize), perm);
  r = medianCastResult(r, x);
end
%=============================================================================
function r = medianAlongDimensions(x, dims, omitNaN)
  % median over a vector of dimensions: gather every element spanning the
  % listed dimensions, then take a single median over them. This is not the
  % same as successive single-dimension medians, so the elements are collected
  % by permuting the reduced dimensions to the front and collapsing them.
  sz = size(x);
  nd = numel(sz);
  isReduced = false(1, nd);
  for d = dims(:)'
    if d >= 1 && d <= nd
      isReduced(d) = true;
    end
  end
  reduced = find(isReduced);
  kept = find(~isReduced);
  if numel(reduced) <= 1
    if isempty(reduced)
      dsel = dims(1);
    else
      dsel = reduced(1);
    end
    r = medianAlongDimension(x, dsel, omitNaN);
    return
  end
  perm = [reduced, kept];
  xp = permute(x, perm);
  reducedCount = prod(sz(reduced));
  keptCount = prod(sz(kept));
  xr = reshape(xp, reducedCount, keptCount);
  values = zeros(1, keptCount);
  for k = 1:keptCount
    values(k) = medianVector(xr(:, k), omitNaN);
  end
  outSize = sz;
  outSize(reduced) = 1;
  r = reshape(values, outSize);
  r = medianCastResult(r, x);
end
%=============================================================================
function value = medianVector(values, omitNaN)
  values = double(values(:));
  if omitNaN
    values = values(~isnan(values));
  elseif any(isnan(values))
    value = NaN;
    return
  end
  n = length(values);
  if n == 0
    value = NaN;
    return
  end
  values = sort(values);
  if mod(n, 2) == 1
    value = values((n + 1) / 2);
  else
    low = values(n / 2);
    high = values(n / 2 + 1);
    value = (low + high) / 2;
    if isinf(value) && isfinite(low) && isfinite(high)
      % the sum overflowed: average the halves instead
      value = low / 2 + high / 2;
    end
  end
end
%=============================================================================
