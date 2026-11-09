%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% m(i, :) and m(:, j) give the values, class and size of the same subset
% written with index vectors, and keep their errors.
%=============================================================================
m = reshape(1:12, 3, 4);
values = {m, single(m), int8(m), uint16(m), m > 6, char(m + 64), m(:, 1:3)'};
for c = 1:numel(values)
  a = values{c};
  for i = 1:size(a, 1)
    assert_isequal(a(i, :), a(i, 1:end));
    assert_isequal(class(a(i, :)), class(a));
  end
  for j = 1:size(a, 2)
    assert_isequal(a(:, j), a(1:end, j));
  end
end
assert_isequal(size(m(2, :)), [1 4]);
assert_isequal(size(m(:, 3)), [3 1]);
%=============================================================================
% the same subsets read in a loop
acc = 0;
expected = 0;
for k = 1:30
  r = m(mod(k, 3) + 1, :);
  col = m(:, mod(k, 4) + 1);
  acc = acc + r(1) + col(end);
  expected = expected + m(mod(k, 3) + 1, 1) + m(end, mod(k, 4) + 1);
end
assert_isequal(acc, expected);
assert_isequal(acc, 285);
%=============================================================================
% complex, sparse, empty and N-D arrays keep the generic subset
z = [1 + 2i, 3; 4, 5];
assert_isequal(z(2, :), [4, 5]);
assert_isfalse(isreal(z(1, :)));
assert_istrue(isreal(z(2, :)));
s = sparse([1 0; 0 2]);
assert_istrue(issparse(s(2, :)));
assert_isequal(full(s(2, :)), [0 2]);
e = zeros(0, 3);
assert_isequal(size(e(:, 2)), [0 1]);
n = reshape(1:24, 2, 3, 4);
assert_isequal(n(2, :), 2:2:24);
assert_isequal(m(int32(2), :), m(2, 1:end));
assert_isequal(m(true, :), m(1, :));
%=============================================================================
% index errors are unchanged
assert_checkerror('m(4, :)', 'Index exceeds array bounds.');
assert_checkerror('m(:, 5)', 'Index exceeds array bounds.');
assert_checkerror('m(1.5, :)', 'index must either be real positive integers or logicals.');
assert_checkerror('m(0, :)', 'Zero or negative index encountered.');
%=============================================================================
