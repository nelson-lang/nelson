%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [M, F, C] = mode(A, dim)
  narginchk(1, 2);
  nargoutchk(0, 3);
  % a sparse input is processed densely; M and the values in C stay sparse
  wasSparse = issparse(A);
  if wasSparse
    A = full(A);
  end
  hasDim = nargin == 2;
  allValues = false;
  if ~hasDim
    dim = firstNonsingletonLocal(size(A));
  elseif ischar(dim) || isstring(dim)
    if ~strcmp(char(dim), 'all')
      error('Nelson:statistics:unsupportedModeOption', 'Unsupported %s.', 'mode option');
    end
    allValues = true;
  elseif ~isnumeric(dim) || isempty(dim) || ~isvector(dim) || any(dim < 1) || any(dim ~= floor(dim)) || numel(unique(dim)) ~= numel(dim)
    error('Nelson:statistics:dimensionArgumentPositiveIntegerScalar', 'Dimension argument must be %s.', 'a positive integer scalar or vector of positive integers');
  end
  if allValues
    [M, F, C] = modeVectorLocal(A(:), A);
    C = {C};
  elseif isequal(size(A), [0 0]) && ~hasDim
    % mode([]) is NaN (with a dimension, [] follows the reduction rule)
    [M, F, C] = modeVectorLocal(A(:), A);
    C = {reshape(C, 0, 1)};
  elseif isscalar(dim)
    [M, F, C] = modeAlongDimensionLocal(A, dim);
  else
    [M, F, C] = modeAlongDimensionsLocal(A, dim);
  end
  if wasSparse
    M = sparse(M);
    C = cellfun(@sparse, C, 'UniformOutput', false);
  end
end
%=============================================================================
function [M, F, C] = modeAlongDimensionLocal(A, dim)
  perm = 1:max(ndims(A), dim);
  perm([1 dim]) = [dim 1];
  Ap = permute(A, perm);
  sz = size(Ap);
  n = sz(1);
  cols = prod(sz(2:end));
  Ar = reshape(Ap, n, cols);
  [Mv, Fv, Cv] = modeColumnsLocal(Ar, A);
  outSize = sz;
  outSize(1) = 1;
  M = ipermute(reshape(Mv, outSize), perm);
  F = ipermute(reshape(Fv, outSize), perm);
  C = ipermute(reshape(Cv, outSize), perm);
end
%=============================================================================
function [M, F, C] = modeAlongDimensionsLocal(A, dims)
  % mode over a vector of dimensions: the elements spanning the listed
  % dimensions are gathered together before taking a single mode.
  sz = size(A);
  nd = numel(sz);
  isReduced = false(1, nd);
  isReduced(dims(dims <= nd)) = true;
  reduced = find(isReduced);
  kept = find(~isReduced);
  perm = [reduced, kept];
  Ap = permute(A, perm);
  Ar = reshape(Ap, prod(sz(reduced)), prod(sz(kept)));
  [Mv, Fv, Cv] = modeColumnsLocal(Ar, A);
  outSize = sz;
  outSize(reduced) = 1;
  M = reshape(Mv, outSize);
  F = reshape(Fv, outSize);
  C = reshape(Cv, outSize);
end
%=============================================================================
function [Mv, Fv, Cv] = modeColumnsLocal(Ar, A)
  cols = size(Ar, 2);
  Mv = modeClassLocal(zeros(1, cols), A);
  Fv = zeros(1, cols);
  Cv = cell(1, cols);
  for k = 1:cols
    [Mv(k), Fv(k), Cv{k}] = modeVectorLocal(Ar(:, k), A);
  end
end
%=============================================================================
function [m, f, c] = modeVectorLocal(values, A)
  values = values(:);
  if isempty(values)
    m = modeClassLocal(NaN, A);
    f = 0;
    c = reshape(values, 1, 0);
    return
  end
  sortedValues = sort(values);
  % a run of equal values (NaN never equals NaN: each NaN is its own run)
  isNewRun = sortedValues(1:end - 1) ~= sortedValues(2:end);
  runStarts = [1; find(isNewRun) + 1];
  runLengths = diff([runStarts; length(sortedValues) + 1]);
  [f, idx] = max(runLengths);
  m = sortedValues(runStarts(idx));
  % C contains all values with the maximum frequency.
  c = sortedValues(runStarts(runLengths == f));
  c = c(:);
end
%=============================================================================
function M = modeClassLocal(M, A)
  % M keeps the class of the input (integer classes give 0 and logical
  % gives false for an empty set, since NaN cannot be represented).
  if islogical(A) || ischar(A)
    M(isnan(M)) = 0;
    M = cast(M, class(A));
  elseif isinteger(A) || isa(A, 'single')
    M = cast(M, class(A));
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
