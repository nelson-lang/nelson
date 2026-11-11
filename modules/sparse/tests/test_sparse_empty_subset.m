%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%=============================================================================
% empty subsets of sparse arrays are sparse with real empty storage
a = sparse([1 2 3]);
b = sparse([1 0; 0 2]);
l = sparse([true false true]);
c = sparse([1i 2]);
cases = {a(:, zeros(1, 0)), [1 0], 'double'; a(false), [0 0], 'double'; a([]), [0 0], 'double'; ...
  a(zeros(1, 0)), [1 0], 'double'; b(:, []), [2 0], 'double'; b([], :), [0 2], 'double'; ...
  b(false(2, 2)), [0 1], 'double'; l(:, []), [1 0], 'logical'; c(:, []), [1 0], 'double'; ...
  b(zeros(0, 1)), [0 1], 'double'; b(1, []), [1 0], 'double'; l(false), [0 0], 'logical'; ...
  a(:, logical([0 0 0])), [1 0], 'double'};
for k = 1:size(cases, 1)
  r = cases{k, 1};
  asserts.isequal(size(r), cases{k, 2});
  asserts.isequal(class(r), cases{k, 3});
  asserts.istrue(issparse(r));
  asserts.istrue(isreal(r));
  asserts.isequal(nnz(r), 0);
end
%=============================================================================
% operations on an empty subset (sparse minus used to crash)
x = a(:, zeros(1, 0));
r = sparse(zeros(1, 0)) - x;
asserts.isequal(size(r), [1 0]);
asserts.istrue(issparse(r));
asserts.istrue(issparse(x + x));
asserts.istrue(issparse(x .* x));
asserts.isequal(size(x'), [0 1]);
asserts.isequal(full(x), zeros(1, 0));
asserts.isequal(size([x, sparse(1)]), [1 1]);
asserts.isequal(full(x * sparse(zeros(0, 2))), zeros(1, 2));
asserts.isequal(full(sum(x)), 0);
asserts.isequal(size(sparse(zeros(2, 1)) * x), [2 0]);
asserts.isequal(size(x(:)), [0 1]);
%=============================================================================
