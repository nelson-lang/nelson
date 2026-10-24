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
M = 35;
e = ones(M, 1);
T = sparse(1:M, 1:M, 4 * e, M, M) + sparse(2:M, 1:M - 1, -e(2:M), M, M) + sparse(1:M - 1, 2:M, -e(1:M - 1), M, M);
E = sparse(2:M, 1:M - 1, -e(2:M), M, M) + sparse(1:M - 1, 2:M, -e(1:M - 1), M, M);
A = kron(speye(M), T) + kron(E, speye(M));
b = ones(M * M, 1);
asserts.istrue(nnz(A) > 4 * M * M);
%=============================================================================
tic();
[x1, flag1, relres1, iter1] = bicgstab(A, b, 1e-8, 500);
t1 = toc();
asserts.isequal(flag1, 0);
asserts.istrue(relres1 < 1e-8);
%=============================================================================
[L, U] = ilu(A);
tic();
[x2, flag2, relres2, iter2] = bicgstab(A, b, 1e-8, 500, L, U);
t2 = toc();
asserts.isequal(flag2, 0);
asserts.istrue(relres2 < 1e-8);
asserts.istrue(norm(A * x2 - b) <= norm(A * x1 - b) * 10);
asserts.istrue(iter2 <= iter1);
%=============================================================================
disp(['bicgstab poisson2d: ', num2str(t1), ' s, iter ', num2str(iter1)]);
disp(['bicgstab poisson2d + ilu: ', num2str(t2), ' s, iter ', num2str(iter2)]);
%=============================================================================
