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
N = 800;
e = ones(N, 1);
d = ones(N, 1);
d(1:2:N) = -2;
d(2:2:N) = 2;
A = sparse(1:N, 1:N, d, N, N) ...
  + sparse(2:N, 1:N - 1, 0.25 * e(2:N), N, N) ...
  + sparse(1:N - 1, 2:N, 0.25 * e(1:N - 1), N, N);
b = ones(N, 1);
%=============================================================================
tic();
[x, flag, relres, iter] = minres(A, b, 1e-8, 200);
t = toc();
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-8);
asserts.istrue(norm(A * x - b) / norm(b) < 1e-8);
asserts.istrue(iter > 1);
%=============================================================================
disp(['minres symmetric indefinite: ', num2str(t), ' s, iter ', num2str(iter)]);
%=============================================================================
