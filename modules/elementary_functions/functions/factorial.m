%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = factorial(X)
  if ~isnumeric(X)
    error('Nelson:elementary_functions:nonNumericInput', 'Input must be numeric.');
  end
  if ~isreal(X) || any(fix(X(:)) ~= X(:)) || any(X(:) < 0)
    error('Nelson:elementary_functions:e1InputArgumentRealPositive', '#%d argument: must be real positive integer values.', 1);
  end
  cl = class(X);
  % a sparse input is processed densely; the result stays sparse
  wasSparse = issparse(X);
  if wasSparse
    X = full(X);
  end
  if isa(X, 'int64') || isa(X, 'uint64')
    % 64 bit integers: exact saturated iterative product in the input class
    Y = X;
    mx = intmax(cl);
    for k = 1:numel(X)
      p = ones(1, 1, cl);
      for j = 2:double(X(k))
        p = p * cast(j, cl);
        if p == mx
          break
        end
      end
      Y(k) = p;
    end
    return
  end
  % cumulative product in the working precision (single stays single all along,
  % so single results match a single-precision running product)
  if isa(X, 'single')
    t = [1, cumprod(single(1:34))];
    maxValue = 34;
  else
    t = [1, cumprod(1:170)];
    maxValue = 170;
  end
  Xd = double(X);
  Y = zeros(size(X), class(t));
  big = Xd > maxValue;
  Y(big) = Inf;
  Y(~big) = t(Xd(~big) + 1);
  if isinteger(X)
    Y = cast(Y, cl);
  end
  if wasSparse && isa(Y, 'double')
    Y = sparse(Y);
  end
end
%=============================================================================
