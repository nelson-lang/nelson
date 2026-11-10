%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Ill-conditioned matrices with equal LU pivots still report RCOND < eps.
n = 60;
K = eye(n) - triu(ones(n), 1);
K(n, 1) = 1e-30;
b = ones(n, 1);
cases = {K, single(K), K + 1i * 1e-30 * (K ~= 0), K' * K, single(K' * K)};
for k = 1:numel(cases)
  A = cases{k};
  B = ones(n, 1, 'like', A);
  lastwarn('', '');
  X = A \ B;
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:nearlySingularMatrix');
  asserts.isequal(class(X), class(A));
  lastwarn('', '');
  Y = B' / A;
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:nearlySingularMatrix');
  asserts.isequal(class(Y), class(A));
end
%=============================================================================
lastwarn('', '');
X = K \ b;
msg = lastwarn();
asserts.istrue(startsWith(msg, _('Matrix is nearly singular or badly scaled')));
R = str2double(regexp(msg, 'RCOND = (\S+)\.$', 'tokens', 'once'));
asserts.istrue(abs(R / 2.891206e-20 - 1) < 1e-4);
%=============================================================================
% Symmetric positive definite but ill-conditioned: Cholesky gives way to LU.
lastwarn('', '');
X = (K' * K) \ b;
msg = lastwarn();
asserts.istrue(startsWith(msg, _('Matrix is nearly singular or badly scaled')));
R = str2double(regexp(msg, 'RCOND = (\S+)\.$', 'tokens', 'once'));
asserts.istrue(abs(R / 2.633546e-39 - 1) < 1e-4);
%=============================================================================
% A right-hand side built as A * x does not hide the bad condition.
lastwarn('', '');
X = K \ (K * b);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
lastwarn('', '');
X = (K' * K) \ ((K' * K) * b);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
lastwarn('', '');
X = zeros(1, n) / K;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
%=============================================================================
% Triangular matrices keep the diagonal ratio: no warning here.
lastwarn('', '');
X = single(K(1:30, 1:30)) \ ones(30, 1, 'single');
[msg, id] = lastwarn();
asserts.isequal(id, '');
%=============================================================================
% Well-conditioned systems stay silent.
lastwarn('', '');
X = [4 1; 1 3] \ [1; 2];
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isapprox(X, [1; 7] / 11, 1e-15);
lastwarn('', '');
X = [1 2; 3 4] \ [1; 2];
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isapprox(X, [0; 0.5], 1e-15);
%=============================================================================
