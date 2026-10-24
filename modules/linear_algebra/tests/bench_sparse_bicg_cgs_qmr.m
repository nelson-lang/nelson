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
n = 250;
d = ones(n, 1);
A = sparse(1:n, 1:n, 4 * d, n, n) ...
  + sparse(2:n, 1:n - 1, -d(2:n), n, n) ...
  + sparse(1:n - 1, 2:n, 0.5 * d(1:n - 1), n, n);
b = A * ones(n, 1);
%=============================================================================
tic();
[xb, flagb, relresb, iterb] = bicg(A, b, 1e-8, 500);
tb = toc();
asserts.isequal(flagb, 0);
asserts.istrue(relresb < 1e-8);
asserts.istrue(norm(A * xb - b) / norm(b) < 1e-8);
%=============================================================================
tic();
[xc, flagc, relresc, iterc] = cgs(A, b, 1e-8, 500);
tc = toc();
asserts.isequal(flagc, 0);
asserts.istrue(relresc < 1e-8);
asserts.istrue(norm(A * xc - b) / norm(b) < 1e-8);
%=============================================================================
tic();
[xq, flagq, relresq, iterq] = qmr(A, b, 1e-8, 500);
tq = toc();
asserts.isequal(flagq, 0);
asserts.istrue(relresq < 1e-8);
asserts.istrue(norm(A * xq - b) / norm(b) < 1e-8);
%=============================================================================
disp(['bicg sparse nonsymmetric: ', num2str(tb), ' s, iter ', num2str(iterb)]);
disp(['cgs sparse nonsymmetric: ', num2str(tc), ' s, iter ', num2str(iterc)]);
disp(['qmr sparse nonsymmetric: ', num2str(tq), ' s, iter ', num2str(iterq)]);
%=============================================================================
