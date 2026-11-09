%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: the fixed cost of a call or operator in a loop body. A builtin call
% must not rebuild caches per call (caller context walk, overload probe, GUI
% dispatch query), and scalar power, logical operators, && conditions and
% small colon ranges must not take the generic operator path.
N = 200000;
v = 1:10;
%=============================================================================
acc = 0;
tic();
for k = 1:N
  acc = acc + numel(v);
end
t = toc();
asserts.isequal(acc, 10 * N);
fprintf('  builtin call         numel(v)    : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
acc = 0;
tic();
for k = 1:N
  z = zeros(1, 3);
  acc = acc + z(1);
end
t = toc();
asserts.isequal(acc, 0);
fprintf('  evaluator builtin    zeros(1, 3) : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
acc = 0;
tic();
for k = 1:N
  acc = acc + k ^ 2;
end
t = toc();
asserts.isequal(acc, N * (N + 1) * (2 * N + 1) / 6);
fprintf('  scalar power         k ^ 2       : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
x = false;
tic();
for k = 1:N
  x = ~x;
end
t = toc();
asserts.isequal(x, false);
fprintf('  logical not          ~x          : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
acc = 0;
tic();
for k = 1:N
  if k > 3 && k < N
    acc = acc + 1;
  end
end
t = toc();
asserts.isequal(acc, N - 4);
fprintf('  && condition         if a && b   : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
acc = 0;
tic();
for k = 1:N
  r = 1:5;
  acc = acc + r(2);
end
t = toc();
asserts.isequal(acc, 2 * N);
fprintf('  colon range          1:5         : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
f = @(y) y + 1;
acc = 0;
tic();
for k = 1:N
  acc = acc + f(k);
end
t = toc();
asserts.isequal(acc, N * (N + 1) / 2 + N);
fprintf('  anonymous call       f(k)        : %8.1f ns/op\n', t / N * 1e9);
%=============================================================================
