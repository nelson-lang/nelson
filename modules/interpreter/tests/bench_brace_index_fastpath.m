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
% Bench: brace extraction of a local through a local index used directly as
% an index argument, e.g. m(ks{i}) = v. This compiles to a single
% LOAD_BRACES_LOCAL instead of the generic LOAD/END_CTX/SUBIDX_BRACES
% comma-list sequence; the inline form must stay close to the exploded
% x = ks{i}; m(x) = v form (it was ~27x slower before the fast path).
%=============================================================================
N = 20000;
ks = cell(1, N);
for i = 1:N
  ks{i} = i;
end
%=============================================================================
m = zeros(1, N);
tic();
for i = 1:N
  m(ks{i}) = i;
end
t_inline = toc();
asserts.isequal(m, 1:N);
fprintf('  assign inline   m(ks{i}) = v    : %8.1f ns/op\n', t_inline / N * 1e9);
%=============================================================================
m2 = zeros(1, N);
tic();
for i = 1:N
  x = ks{i};
  m2(x) = i;
end
t_exploded = toc();
asserts.isequal(m2, 1:N);
fprintf('  assign exploded x = ks{i}; m(x) : %8.1f ns/op\n', t_exploded / N * 1e9);
%=============================================================================
acc = 0;
tic();
for i = 1:N
  acc = acc + m(ks{i});
end
t_read = toc();
asserts.isequal(acc, sum(1:N));
fprintf('  read inline     y = m(ks{i})    : %8.1f ns/op\n', t_read / N * 1e9);
%=============================================================================
% regression guard: inline must stay in the same league as the exploded form
asserts.istrue(t_inline < 5 * max(t_exploded, 1e-4));
%=============================================================================
% behavior checks: the fast path must keep comma-list semantics
c = {1, 3};
iv = [1 2];
A = zeros(3, 3);
A(c{iv}) = 9;
asserts.isequal(A(1, 3), 9);
asserts.isequal(A(c{iv}), 9);
%=============================================================================
% empty comma-list index must still error
ce = {1};
ie = [];
ok = false;
try
  A(ce{ie}) = 1;
catch
  ok = true;
end
asserts.istrue(ok);
%=============================================================================
% char key extracted from a cell, applied to a keyed container
keys = {'alpha', 'beta'};
mp = containers.Map();
k = 2;
mp(keys{k}) = 42;
asserts.isequal(mp(keys{k}), 42);
%=============================================================================
