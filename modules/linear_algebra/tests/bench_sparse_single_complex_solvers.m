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
N = 900;
REPEATS = 20;
e = single(ones(N, 1));
A = sparse(1:N, 1:N, single(4) * e + single(0.05i) * e, N, N) ...
  + sparse(2:N, 1:N - 1, single(-1) * e(2:N), N, N) ...
  + sparse(1:N - 1, 2:N, single(-0.3 + 0.05i) * e(1:N - 1), N, N);
xref = single(ones(N, 1) + 0.25i * ones(N, 1));
b = A * xref;
restart = 20;
%=============================================================================
tg = 0;
for k = 1:REPEATS
  tic();
  [xg, flag, relresg, iterg] = gmres(A, b, restart, 1e-6, 80);
  tg = tg + toc();
end
tg = tg / REPEATS;
asserts.isequal(flag, 0);
asserts.istrue(relresg < 1e-6);
asserts.istrue(norm(A * xg - b) / norm(b) < 1e-5);
%=============================================================================
tb = 0;
for k = 1:REPEATS
  tic();
  [xb, flagb, relresb, iterb] = bicgstab(A, b, 1e-6, 120);
  tb = tb + toc();
end
tb = tb / REPEATS;
asserts.isequal(flagb, 0);
asserts.istrue(relresb < 1e-6);
asserts.istrue(norm(A * xb - b) / norm(b) < 1e-5);
%=============================================================================
m = 1200;
n = 400;
d = single(ones(n, 1));
R = sparse(1:n, 1:n, single(3 + 0.1i) * d, m, n) ...
  + sparse(2:n, 1:n - 1, single(0.75 - 0.05i) * d(2:n), m, n) ...
  + sparse(1:n - 1, 2:n, single(0.25 + 0.1i) * d(1:n - 1), m, n) ...
  + sparse(n + 1:2 * n, 1:n, single(0.2) * d, m, n);
yref = single(ones(n, 1) - 0.5i * ones(n, 1));
c = R * yref;
%=============================================================================
tlq = 0;
for k = 1:REPEATS
  tic();
  [xlq, flaglq, relreslq, iterlq] = lsqr(R, c, 1e-6, 200);
  tlq = tlq + toc();
end
tlq = tlq / REPEATS;
asserts.isequal(flaglq, 0);
asserts.istrue(relreslq < 1e-5);
asserts.istrue(norm(R * xlq - c) / norm(c) < 1e-5);
%=============================================================================
tlm = 0;
for k = 1:REPEATS
  tic();
  [xlm, flaglm, relreslm, iterlm] = lsmr(R, c, 1e-6, 200);
  tlm = tlm + toc();
end
tlm = tlm / REPEATS;
asserts.isequal(flaglm, 0);
asserts.istrue(relreslm < 1e-5);
asserts.istrue(norm(R * xlm - c) / norm(c) < 1e-5);
%=============================================================================
disp(['gmres sparse single complex: ', num2str(tg), ' s, iter ', mat2str(iterg)]);
disp(['bicgstab sparse single complex: ', num2str(tb), ' s, iter ', num2str(iterb)]);
disp(['lsqr sparse single complex rectangular: ', num2str(tlq), ' s, iter ', num2str(iterlq)]);
disp(['lsmr sparse single complex rectangular: ', num2str(tlm), ' s, iter ', num2str(iterlm)]);
%=============================================================================
