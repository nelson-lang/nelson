%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Small Hermitian systems are solved by LU, not Cholesky.
function test_mldivide_small_hermitian()
  % nearly singular: the LU RCOND is reported
  A = [2, 1 + 1i; 1 - 1i, 1 + 1e-15];
  b = [1; 2];
  lastwarn('', '');
  x = A \ b;
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:nearlySingularMatrix');
  R = str2double(regexp(msg, 'RCOND = (\S+)\.$', 'tokens', 'once'));
  asserts.istrue(abs(R / 1.904842e-16 - 1) < 1e-5);
  asserts.isequal(x, luSolve(A, b));
  lastwarn('', '');
  y = b.' / A;
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:nearlySingularMatrix');
  % well conditioned: same bits as the LU solution, real and complex
  for n = [2, 3, 4, 8, 16]
    M = reshape(mod((1:n^2) * 7, 11), n, n) - 5;
    cases = {M' * M + n * eye(n), single(M' * M + n * eye(n))};
    if n <= 8
      Z = M + 1i * M.';
      cases{end + 1} = Z' * Z + n * eye(n);
      cases{end + 1} = single(Z' * Z + n * eye(n));
    end
    for k = 1:numel(cases)
      H = cases{k};
      H = (H + H') / 2;
      c = ones(n, 1, 'like', real(H));
      lastwarn('', '');
      asserts.isequal(H \ c, luSolve(H, c));
      asserts.isequal(lastwarn(), '');
    end
  end
end
%=============================================================================
function x = luSolve(A, b)
  [L, U, P] = lu(A);
  x = U \ (L \ (P * b));
end
%=============================================================================
