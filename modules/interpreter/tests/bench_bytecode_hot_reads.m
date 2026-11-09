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
% Bench: the element reads a loop body does most often. Each one has a direct
% path in its opcode (scalar struct field, scalar cell content, m(i, j) on a
% double matrix, scalar math builtin) that must not build temporary ArrayOf
% values: a default ArrayOf allocates, and these paths used to pay two to five
% of them per read.
N = 200000;
%=============================================================================
s = struct('a', 3);
acc = 0;
tic();
for k = 1:N
  acc = acc + s.a;
end
t_field = toc();
asserts.isequal(acc, 3 * N);
fprintf('  scalar struct field  x = s.a     : %8.1f ns/op\n', t_field / N * 1e9);
%=============================================================================
c = num2cell(1:N);
acc = 0;
tic();
for k = 1:N
  acc = acc + c{k};
end
t_cell = toc();
asserts.isequal(acc, sum(1:N));
fprintf('  scalar cell content  x = c{k}    : %8.1f ns/op\n', t_cell / N * 1e9);
%=============================================================================
n = 400;
m = reshape(1:n * n, n, n);
acc = 0;
tic();
for j = 1:n
  for i = 1:n
    acc = acc + m(i, j);
  end
end
t_matrix = toc();
asserts.isequal(acc, sum(1:n * n));
fprintf('  matrix element       x = m(i, j) : %8.1f ns/op\n', t_matrix / (n * n) * 1e9);
%=============================================================================
acc = 0;
tic();
for k = 1:N
  acc = acc + abs(-k);
end
t_builtin = toc();
asserts.isequal(acc, sum(1:N));
fprintf('  scalar builtin       x = abs(k)  : %8.1f ns/op\n', t_builtin / N * 1e9);
%=============================================================================
w = zeros(n, n);
tic();
for j = 1:n
  for i = 1:n
    w(i, j) = i + j;
  end
end
t_write = toc();
asserts.isequal(w(n, n), 2 * n);
fprintf('  matrix element write m(i, j) = v : %8.1f ns/op\n', t_write / (n * n) * 1e9);
%=============================================================================
sv = struct('a', num2cell(1:N));
acc = 0;
tic();
for k = 1:N
  acc = acc + sv(k).a;
end
t_struct = toc();
asserts.isequal(acc, sum(1:N));
fprintf('  struct array field   x = s(k).a  : %8.1f ns/op\n', t_struct / N * 1e9);
%=============================================================================
names = {};
tic();
for k = 1:N
  names{end + 1} = 'item'; %#ok<NLS0039>
end
t_append = toc();
asserts.isequal(numel(names), N);
fprintf('  cell append          c{end + 1}  : %8.1f ns/op\n', t_append / N * 1e9);
%=============================================================================
