%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = are(A, B, C)
  arguments
    A {mustBeNumeric}
    B {mustBeNumeric}
    C {mustBeNumeric}
  end
  % Algebraic Riccati equation.
  % X = are(A, B, C)

  nargoutchk(0, 1);

  [rA, cA] = size(A);
  if rA ~= cA
    error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 1, 'square matrix');
  end
  if ~isequal(size(B), size(A))
    error('Nelson:control_system:wrongSize:wrongSizeArgumentNSizeArgument', 'Wrong size for argument #%d: same size as argument #%d expected.', 2, 1);
  end
  if ~isequal(size(C), size(A))
    error('Nelson:control_system:wrongSize:wrongSizeArgumentNSizeArgument', 'Wrong size for argument #%d: same size as argument #%d expected.', 3, 1);
  end
  if ~ishermitian(B)
    error('Nelson:control_system:wrongValue', 'Wrong value for argument #%d: %s expected.', 2, 'symmetric matrix');
  end
  if ~ishermitian(C)
    error('Nelson:control_system:wrongValue', 'Wrong value for argument #%d: %s expected.', 3, 'symmetric matrix');
  end

  G = localSymmetricSquareRoot(B);
  X = care(A, G, C);
end
%=============================================================================
function G = localSymmetricSquareRoot(B)
  B = (B + B') / 2;
  [V, D] = eig(B);
  values = real(diag(D));
  tolerance = 100 * eps() * max(1, max(abs(values)));
  if any(values < -tolerance)
    error('Nelson:control_system:wrongValue', 'Wrong value for argument #%d: %s expected.', 2, 'nonnegative matrix');
  end
  values(values < 0) = 0;
  G = V * diag(sqrt(values));
end
%=============================================================================
