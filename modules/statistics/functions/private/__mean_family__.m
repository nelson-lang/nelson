%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = __mean_family__(x, method, varargin)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  if isempty(x)
    y = cast(NaN, class(x));
    return
  end
  [dims, allValues, omitnan, returnInput] = parseInputsLocal(x, varargin);
  if returnInput
    y = x;
  elseif allValues
    y = meanVectorLocal(double(x(:)), method, omitnan);
  else
    y = meanAlongDimensionsLocal(double(x), method, dims, omitnan);
  end
  if isa(x, 'single') && ~isa(y, 'single')
    y = single(y);
  end
end
%=============================================================================
function [dims, allValues, omitnan, returnInput] = parseInputsLocal(x, args)
  dims = firstNonsingletonLocal(size(x));
  allValues = false;
  omitnan = false;
  returnInput = false;
  if isempty(args)
    return
  end
  first = args{1};
  consumedDimension = false;
  if isTextScalarLocal(first)
    text = lower(char(first));
    if strcmp(text, 'all')
      dims = [];
      allValues = true;
      consumedDimension = true;
    elseif isNanFlagLocal(text)
      omitnan = strcmp(text, 'omitnan');
    else
      error('Nelson:statistics:invalidOption', 'Invalid option.');
    end
  else
    [dims, returnInput] = parseDimensionLocal(first, size(x));
    consumedDimension = true;
  end
  if length(args) >= 2
    text = textArgumentLocal(args{2});
    if ~isNanFlagLocal(text)
      error('Nelson:statistics:invalidOption', 'Invalid option.');
    end
    omitnan = strcmp(text, 'omitnan');
  end
  if length(args) > 2 || (~consumedDimension && length(args) > 1)
    error('Nelson:tooManyInputs', 'Too many input arguments.');
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
function y = meanAlongDimensionsLocal(x, method, dims, omitnan)
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
    values(k) = meanVectorLocal(xr(:, k), method, omitnan);
  end
  outSize = sz;
  outSize(dims) = 1;
  outSize = outSize(1:originalRank);
  permutedOutSize = [ones(1, length(dims)), psize(length(dims) + 1:end)];
  y = ipermute(reshape(values, permutedOutSize), perm);
  y = reshape(y, outSize);
end
%=============================================================================
function value = meanVectorLocal(values, method, omitnan)
  if omitnan
    values = values(~isnan(values));
  end
  if isempty(values)
    value = NaN;
  elseif strcmp(method, 'geomean')
    value = exp(mean(log(values)));
  else
    value = length(values) / sum(1 ./ values);
  end
end
%=============================================================================
function text = textArgumentLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:statistics:invalidOption', 'Invalid option.');
  end
  text = lower(char(value));
end
%=============================================================================
function tf = isNanFlagLocal(text)
  tf = strcmp(text, 'includenan') || strcmp(text, 'omitnan');
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
