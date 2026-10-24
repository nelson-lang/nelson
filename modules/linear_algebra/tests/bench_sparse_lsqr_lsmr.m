%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
m = 900;
n = 300;
d = ones(n, 1);
A = sparse(1:n, 1:n, 4 * d, m, n) ...
  + sparse(2:n, 1:n - 1, d(2:n), m, n) ...
  + sparse(1:n - 1, 2:n, 0.5 * d(1:n - 1), m, n) ...
  + sparse(n + 1:2 * n, 1:n, 0.25 * d, m, n);
xref = ones(n, 1);
b = A * xref;
%=============================================================================
tic();
[x1, flag1, relres1, iter1] = lsqr(A, b, 1e-8, 200);
t1 = toc();
asserts.isequal(flag1, 0);
asserts.istrue(relres1 < 1e-7);
asserts.istrue(norm(A * x1 - b) / norm(b) < 1e-7);
%=============================================================================
tic();
[x2, flag2, relres2, iter2] = lsmr(A, b, 1e-8, 200);
t2 = toc();
asserts.isequal(flag2, 0);
asserts.istrue(relres2 < 1e-7);
asserts.istrue(norm(A * x2 - b) / norm(b) < 1e-7);
%=============================================================================
disp(['lsqr rectangular sparse: ', num2str(t1), ' s, iter ', num2str(iter1)]);
disp(['lsmr rectangular sparse: ', num2str(t2), ' s, iter ', num2str(iter2)]);
%=============================================================================
