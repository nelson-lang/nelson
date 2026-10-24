%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
N = 2500;
REPEATS = 50;
e = ones(N, 1);
A = sparse(1:N, 1:N, 4 * e, N, N) ...
  + sparse(2:N, 1:N - 1, -1.2 * e(2:N), N, N) ...
  + sparse(1:N - 1, 2:N, -0.35 * e(1:N - 1), N, N);
B = [ones(N, 1), (1:N)'];
%=============================================================================
t = 0;
for k = 1:REPEATS
  tic();
  X = A \ B;
  t = t + toc();
end
t = t / REPEATS;
asserts.istrue(norm(A * X - B, 'fro') / norm(B, 'fro') < 1e-10);
disp(['sparse nonsymmetric square mldivide, two RHS: ', num2str(t), ' s']);
%=============================================================================
