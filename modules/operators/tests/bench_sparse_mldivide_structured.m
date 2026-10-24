%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
N = 3000;
REPEATS = 50;
e = ones(N, 1);
Aspd = sparse(1:N, 1:N, 4 * e, N, N) ...
  + sparse(2:N, 1:N - 1, -e(2:N), N, N) ...
  + sparse(1:N - 1, 2:N, -e(1:N - 1), N, N);
bspd = ones(N, 1);
tspd = 0;
for k = 1:REPEATS
  tic();
  xspd = Aspd \ bspd;
  tspd = tspd + toc();
end
tspd = tspd / REPEATS;
asserts.istrue(norm(Aspd * xspd - bspd) / norm(bspd) < 1e-10);
%=============================================================================
Hn = 1200;
h = single(ones(Hn, 1));
diagH = single(2) * h;
diagH(2:2:Hn) = single(-3);
H = sparse(1:Hn, 1:Hn, diagH, Hn, Hn) ...
  + sparse(2:Hn, 1:Hn - 1, single(0.1 - 0.2i) * h(2:Hn), Hn, Hn) ...
  + sparse(1:Hn - 1, 2:Hn, single(0.1 + 0.2i) * h(1:Hn - 1), Hn, Hn);
bh = single(ones(Hn, 1) + 0.25i * ones(Hn, 1));
th = 0;
for k = 1:REPEATS
  tic();
  xh = H \ bh;
  th = th + toc();
end
th = th / REPEATS;
asserts.istrue(norm(H * xh - bh) / norm(bh) < 1e-4);
%=============================================================================
m = 1800;
n = 600;
d = ones(n, 1);
Aover = sparse(1:n, 1:n, 4 * d, m, n) ...
  + sparse(2:n, 1:n - 1, d(2:n), m, n) ...
  + sparse(1:n - 1, 2:n, 0.5 * d(1:n - 1), m, n) ...
  + sparse(n + 1:2 * n, 1:n, 0.25 * d, m, n);
xrefOver = ones(n, 1);
bover = Aover * xrefOver;
tover = 0;
for k = 1:REPEATS
  tic();
  xover = Aover \ bover;
  tover = tover + toc();
end
tover = tover / REPEATS;
asserts.istrue(norm(Aover * xover - bover) / norm(bover) < 1e-6);
%=============================================================================
m = 600;
n = 1800;
d = ones(m, 1);
Aunder = sparse(1:m, 1:m, 4 * d, m, n) ...
  + sparse(2:m, 1:m - 1, d(2:m), m, n) ...
  + sparse(1:m - 1, 2:m, 0.5 * d(1:m - 1), m, n) ...
  + sparse(1:m, m + 1:2 * m, 0.25 * d, m, n);
xrefUnder = zeros(n, 1);
xrefUnder(1:m) = 1;
dunder = Aunder * xrefUnder;
tunder = 0;
for k = 1:REPEATS
  tic();
  xunder = Aunder \ dunder;
  tunder = tunder + toc();
end
tunder = tunder / REPEATS;
asserts.istrue(norm(Aunder * xunder - dunder) / norm(dunder) < 1e-6);
%=============================================================================
disp(['sparse SPD mldivide: ', num2str(tspd), ' s']);
disp(['sparse Hermitian indefinite single complex mldivide: ', num2str(th), ' s']);
disp(['sparse overdetermined mldivide: ', num2str(tover), ' s']);
disp(['sparse underdetermined mldivide: ', num2str(tunder), ' s']);
%=============================================================================
