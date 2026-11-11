%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Large triangular systems with one right-hand side, solved by substitution.
rng(3);
for n = [700 1200]
  A = rand(n) + n * eye(n);
  U = triu(A);
  L = tril(A);
  b = rand(n, 1);
  % column by column back substitution as a reference
  y = b;
  for j = n:-1:1
    y(j) = y(j) / U(j, j);
    y(1:j-1) = y(1:j-1) - y(j) * U(1:j-1, j);
  end
  x = U \ b;
  asserts.isapprox(x, y, 1e-13);
  lastwarn('', '');
  x = L \ b;
  [msg, id] = lastwarn();
  asserts.isequal(id, '');
  asserts.istrue(norm(L * x - b, 1) <= 1e-12 * norm(b, 1));
  % row vector over a triangular matrix
  r = b';
  asserts.istrue(norm(r / U * U - r, 1) <= 1e-12 * norm(r, 1));
  asserts.istrue(norm(r / L * L - r, 1) <= 1e-12 * norm(r, 1));
  % complex and single
  Uc = complex(U, triu(rand(n)));
  bc = complex(b, rand(n, 1));
  xc = Uc \ bc;
  asserts.istrue(norm(Uc * xc - bc, 1) <= 1e-12 * norm(bc, 1));
  yc = bc' / Uc;
  asserts.istrue(norm(yc * Uc - bc', 1) <= 1e-12 * norm(bc, 1));
  yc = bc.' / triu(Uc).';
  asserts.istrue(norm(yc * triu(Uc).' - bc.', 1) <= 1e-12 * norm(bc, 1));
  xs = single(U) \ single(b);
  asserts.isequal(class(xs), 'single');
  asserts.istrue(norm(double(xs) - y, 1) <= 1e-4 * norm(y, 1));
  % zero and tiny diagonal entries keep their warnings and Inf/NaN results
  Uz = U;
  Uz(5, 5) = 0;
  lastwarn('', '');
  x = Uz \ b;
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:singularMatrix');
  asserts.istrue(any(~isfinite(x)));
  Ut = U;
  Ut(2, 2) = 1e-30;
  lastwarn('', '');
  x = b' / Ut;
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:nearlySingularMatrix');
end
%=============================================================================
