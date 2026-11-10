%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Y, ok] = __gpuArray_quantile__(X, p, dim)
  % quantiles at the plotting positions (k - 0.5) / n of the sorted values along dim, on the
  % device; ok is false for what the host must handle (NaN data, invalid or device probabilities)
  Y = [];
  ok = false;
  if ~(isfloat(X) && isreal(X) && ~isempty(X) && isnumeric(p) && ~isgpuarray(p) && isreal(p) ...
      && isvector(p) && all(p >= 0 & p <= 1) && ~any(isnan(p)))
    return
  end
  if ~isgpuarray(X)
    X = gpuArray(X);
  end
  if gather(any(isnan(X(:))))
    return
  end
  sz = size(X);
  if isempty(dim)
    dim = find(sz ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  sz(end + 1:dim) = 1;
  n = sz(dim);
  order = [dim, 1:dim - 1, dim + 1:numel(sz)];
  S = sort(reshape(permute(X, order), n, []), 1);
  r = n * double(p(:)) + 0.5;
  lo = min(max(floor(r), 1), n);
  hi = min(lo + 1, n);
  f = single(min(max(r - lo, 0), 1));
  f(r < 1) = 0;
  Y = S(lo, :) .* (1 - f) + S(hi, :) .* f;
  rest = sz(order(2:end));
  Y = ipermute(reshape(Y, [numel(p), rest]), order);
  ok = true;
end
%=============================================================================
