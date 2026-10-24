%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Size 40 exercises matrix-vector kernels beyond the small-matrix path.
% References use only element-wise products and the three known diagonals.
n = 40;
for precision = {'double', 'single'}
  for complex_values = [false, true]
    diagonal = feval(precision{1}, (1:n)' + 2);
    lower = feval(precision{1}, -ones(n - 1, 1));
    upper = feval(precision{1}, 2 .* ones(n - 1, 1));
    x = feval(precision{1}, (1:n)');
    if complex_values
      diagonal = diagonal + feval(precision{1}, 1i);
      lower = lower + feval(precision{1}, 2i);
      upper = upper - feval(precision{1}, 1i);
      x = x + feval(precision{1}, 1i .* (n:-1:1)');
    end
    D = sparse(1:n, 1:n, diagonal, n, n);
    X = [x, 2 .* x, flipud(x)];
    assert_isequal(D * x, diagonal .* x);
    assert_isequal(x.' * D, (diagonal .* x).');
    assert_isequal(D * X, repmat(diagonal, 1, 3) .* X);

    A = D + sparse(2:n, 1:n - 1, lower, n, n) ...
      + sparse(1:n - 1, 2:n, upper, n, n);
    result = A * x;
    assert_isequal(class(result), precision{1});
    assert_isequal(result, banded_reference(diagonal, lower, upper, x));
    assert_isequal(x.' * A, banded_reference(diagonal, upper, lower, x).');
    assert_isequal(A * X, banded_reference(diagonal, lower, upper, X));
  end
end
%=============================================================================
function result = banded_reference(diagonal, lower, upper, values)
  n = size(values, 1);
  columns = size(values, 2);
  result = repmat(diagonal, 1, columns) .* values;
  result(2:n, :) = result(2:n, :) ...
    + repmat(lower, 1, columns) .* values(1:n - 1, :);
  result(1:n - 1, :) = result(1:n - 1, :) ...
    + repmat(upper, 1, columns) .* values(2:n, :);
end
%=============================================================================
