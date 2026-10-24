%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = moment(x, order, varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  order = parseOrderLocal(order);
  if isempty(x)
    m = cast(NaN, class(x));
    return
  end
  [dims, allValues] = parseDimensionLocal(x, varargin);
  if allValues
    m = momentVectorLocal(double(x(:)), order);
  else
    m = momentAlongDimensionsLocal(double(x), order, dims);
  end
  if isa(x, 'single')
    m = single(m);
  end
end
%=============================================================================
function order = parseOrderLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value)) ...
      || ~isfinite(value) || value < 1 || value ~= fix(value)
    error('Nelson:statistics:ORDERPositiveIntegerScalar', '%s must be a positive integer scalar.', 'ORDER');
  end
  order = double(value);
end
%=============================================================================
function [dims, allValues] = parseDimensionLocal(x, args)
  allValues = false;
  dims = firstNonsingletonLocal(size(x));
  if isempty(args)
    return
  end
  value = args{1};
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
function m = momentAlongDimensionsLocal(x, order, dims)
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
    values(k) = momentVectorLocal(xr(:, k), order);
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  m = ipermute(reshape(values, permutedOutSize), perm);
  m = reshape(m, outSize);
end
%=============================================================================
function value = momentVectorLocal(values, order)
  center = mean(values);
  value = mean((values - center) .^ order);
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
