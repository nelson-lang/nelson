%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = mad(x, varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if isempty(x)
    y = NaN;
    return
  end
  [flag, dims, allValues] = parseInputsLocal(x, varargin);
  if allValues
    y = madVectorLocal(double(x(:)), flag);
  else
    y = madAlongDimensionsLocal(double(x), flag, dims);
  end
end
%=============================================================================
function [flag, dims, allValues] = parseInputsLocal(x, args)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  flag = 0;
  dims = firstNonsingletonLocal(size(x));
  allValues = false;
  if isempty(args)
    return
  end
  first = args{1};
  if isTextScalarLocal(first) || (isnumeric(first) && ~isscalar(first))
    [dims, allValues] = parseDimensionLocal(first);
  else
    flag = parseFlagLocal(first);
  end
  if length(args) >= 2
    [dims, allValues] = parseDimensionLocal(args{2});
  end
end
%=============================================================================
function flag = parseFlagLocal(value)
  if ~(isnumeric(value) || islogical(value)) || ~isscalar(value) || ~(value == 0 || value == 1)
    error('Nelson:statistics:invalidFlag', '%s must be 0 or 1.', 'FLAG');
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
    error('Nelson:statistics:invalidDimensionArg:dimensionArgumentAPositiveIntegerScalar', 'Dimension argument must be a positive integer scalar, vector, or ''all''.');
  end
  if ~(isnumeric(value) && isreal(value) && isvector(value) && all(value == fix(value)) ...
      && all(value >= 1))
    error('Nelson:statistics:invalidDimensionArg:dimensionArgumentAPositiveIntegerScalar', 'Dimension argument must be a positive integer scalar, vector, or ''all''.');
  end
  dims = unique(double(value(:).'), 'stable');
end
%=============================================================================
function y = madAlongDimensionsLocal(x, flag, dims)
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
    values(k) = madVectorLocal(xr(:, k), flag);
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  y = ipermute(reshape(values, permutedOutSize), perm);
  y = reshape(y, outSize);
end
%=============================================================================
function value = madVectorLocal(values, flag)
  values = values(~isnan(values));
  if isempty(values)
    value = NaN;
    return
  end
  if flag == 0
    center = mean(values);
    value = mean(abs(values - center));
  else
    center = median(values);
    value = median(abs(values - center));
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
