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
N = 3000;
REPEATS = 25;
e = single(ones(N, 1));
main = single(4) * e;
lower = single(-1) * e(2:N);
upper = single(-0.75) * e(1:N - 1);
A = sparse(1:N, 1:N, main, N, N) ...
  + sparse(2:N, 1:N - 1, lower, N, N) ...
  + sparse(1:N - 1, 2:N, upper, N, N);
b = single(ones(N, 1));
M = main;
%=============================================================================
t1 = 0;
for k = 1:REPEATS
  tic();
  [x1, flag1, relres1, iter1] = pcg(A, b, 1e-6, 500);
  t1 = t1 + toc();
end
t1 = t1 / REPEATS;
asserts.isequal(flag1, 0);
asserts.istrue(relres1 < 1e-6);
asserts.istrue(norm(A * x1 - b) / norm(b) < 1e-5);
%=============================================================================
t2 = 0;
for k = 1:REPEATS
  tic();
  [x2, flag2, relres2, iter2] = pcg(A, b, 1e-6, 500, M);
  t2 = t2 + toc();
end
t2 = t2 / REPEATS;
asserts.isequal(flag2, 0);
asserts.istrue(relres2 < 1e-6);
asserts.istrue(norm(A * x2 - b) / norm(b) < 1e-5);
asserts.istrue(iter2 <= iter1);
%=============================================================================
disp(['pcg sparse single no preconditioner: ', num2str(t1), ' s, iter ', num2str(iter1)]);
disp(['pcg sparse single diagonal preconditioner: ', num2str(t2), ' s, iter ', num2str(iter2)]);
%=============================================================================
