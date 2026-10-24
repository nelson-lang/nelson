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
M = 25;
e = ones(M, 1);
T = sparse(1:M, 1:M, 4 * e, M, M) ...
  + sparse(2:M, 1:M - 1, -e(2:M), M, M) ...
  + sparse(1:M - 1, 2:M, -0.5 * e(1:M - 1), M, M);
E = sparse(2:M, 1:M - 1, -e(2:M), M, M);
A = kron(speye(M), T) + kron(E, speye(M));
b = ones(M * M, 1);
restart = 20;
%=============================================================================
tic();
[x1, flag1, relres1, iter1] = gmres(A, b, restart, 1e-8, 100);
t1 = toc();
asserts.isequal(flag1, 0);
asserts.istrue(relres1 < 1e-8);
%=============================================================================
[L, U] = ilu(A);
tic();
[x2, flag2, relres2, iter2] = gmres(A, b, restart, 1e-8, 100, L, U);
t2 = toc();
asserts.isequal(flag2, 0);
asserts.istrue(relres2 < 1e-8);
asserts.istrue(norm(A * x2 - b) <= norm(A * x1 - b) * 10);
asserts.istrue(((iter2(1) - 1) * restart + iter2(2)) <= ((iter1(1) - 1) * restart + iter1(2)));
%=============================================================================
disp(['gmres nonsymmetric poisson2d: ', num2str(t1), ' s, iter ', mat2str(iter1)]);
disp(['gmres nonsymmetric poisson2d + ilu: ', num2str(t2), ' s, iter ', mat2str(iter2)]);
%=============================================================================
