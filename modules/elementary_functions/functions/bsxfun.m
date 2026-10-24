%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = bsxfun(fun, A, B)
  narginchk(3, 3);
  nargoutchk(0, 1);
  if ischar(fun) || (isstring(fun) && isscalar(fun))
    fun = str2func(fun);
  end
  if ~isa(fun, 'function_handle')
    error('Nelson:bsxfun:invalidFunction', '%s must be a function handle.', 'First argument');
  end
  sa = size(A);
  sb = size(B);
  nd = max(numel(sa), numel(sb));
  sa(end + 1:nd) = 1;
  sb(end + 1:nd) = 1;
  compatible = (sa == sb) | (sa == 1) | (sb == 1);
  if ~all(compatible)
    error('Nelson:bsxfun:arrayDimensionsMustMatch', 'Non-singleton dimensions of the two input arrays must match each other.');
  end
  % Fast path: when the callback body is a provably element-wise expression
  % whose operators broadcast singleton dimensions, Nelson evaluates it directly
  % on the original operands with implicit broadcasting. This skips the repmat
  % materialization of both inputs to the full output size. Any body that is not
  % provably broadcast-safe (matrix ops, reductions, indexing, min/max/mod/rem
  % which do not broadcast, non-scalar captures, ...) falls through unchanged.
  if (isnumeric(A) || islogical(A)) && (isnumeric(B) || islogical(B)) ...
      && ~issparse(A) && ~issparse(B) && __is_elementwise_broadcastable__(fun)
    C = fun(A, B);
    return
  end
  repA = ones(1, nd);
  repB = ones(1, nd);
  needA = (sa == 1) & (sb > 1);
  needB = (sb == 1) & (sa > 1);
  repA(needA) = sb(needA);
  repB(needB) = sa(needB);
  if any(repA > 1)
    A = repmat(A, repA);
  end
  if any(repB > 1)
    B = repmat(B, repB);
  end
  C = fun(A, B);
end
%=============================================================================
