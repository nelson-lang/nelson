%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, S, mu] = polyfit(x, y, n)
  if (numel(x) ~= numel(y))
    error('Nelson:polyfit:XYSizeMismatch', 'The first two inputs must have the same number of elements.');
  end
  x = x(:);
  y = y(:);
  if nargout > 2
    mu = [mean(x); std(x)];
    if mu(2) == 0
      mu(2) = 1;
    end
    x = (x - mu(1)) ./ mu(2);
  end
  A = ones(numel(x), n + 1);
  for i = n:-1:1
    A(:, i) = x .* A(:, i + 1);
  end
  [Q, R] = qr(A, 0);
  p = (R \ (Q' * y)).';
  if nargout > 1
    r = y - A * p.';
    S.R = R;
    S.df = max(0, numel(y) - (n + 1));
    S.normr = norm(r);
  end
end
%=============================================================================
