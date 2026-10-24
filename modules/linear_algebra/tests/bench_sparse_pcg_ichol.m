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
N = 1500;
e = ones(N, 1);
A = sparse(1:N, 1:N, 4 * e, N, N) + sparse(2:N, 1:N - 1, -e(2:N), N, N) + sparse(1:N - 1, 2:N, -e(1:N - 1), N, N);
b = ones(N, 1);
%=============================================================================
tic();
[x1, flag1, relres1, iter1] = pcg(A, b, 1e-8, 500);
t1 = toc();
asserts.isequal(flag1, 0);
asserts.istrue(relres1 < 1e-8);
%=============================================================================
L = ichol(A);
tic();
[x2, flag2, relres2, iter2] = pcg(A, b, 1e-8, 500, L, L');
t2 = toc();
asserts.isequal(flag2, 0);
asserts.istrue(relres2 < 1e-8);
asserts.istrue(norm(A * x2 - b) <= norm(A * x1 - b) * 10);
asserts.istrue(iter2 <= iter1);
%=============================================================================
disp(['pcg tridiagonal: ', num2str(t1), ' s, iter ', num2str(iter1)]);
disp(['pcg tridiagonal + ichol: ', num2str(t2), ' s, iter ', num2str(iter2)]);
%=============================================================================
