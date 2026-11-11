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
% concatenation of empty sparse arrays follows the dense rule and stays sparse
x = sparse(zeros(1, 0));
y = sparse(zeros(0, 2));
z = sparse(zeros(0, 0));
w = sparse(zeros(2, 0));
l = sparse(false(1, 0));
cases = {[x; x], [2 0]; [x, x], [1 0]; [y; y], [0 2]; [y, y], [0 4]; [w; x], [3 0]; [x; w], [3 0]; ...
  [z; x], [1 0]; [x; z], [1 0]; [x; zeros(1, 0)], [2 0]; [zeros(1, 0); x], [2 0]; [w, w], [2 0]; ...
  [y; y; y], [0 2]; [x; y], [0 2]; [x, w], [2 0]; [x, y], [0 2]; [y; w], [0 0]; ...
  [w; sparse(zeros(3, 0))], [5 0]};
for k = 1:size(cases, 1)
  asserts.isequal(size(cases{k, 1}), cases{k, 2});
  asserts.istrue(issparse(cases{k, 1}));
  asserts.isequal(class(cases{k, 1}), 'double');
end
asserts.isequal(class([l; l]), 'logical');
asserts.isequal(size([l; l]), [2 0]);
asserts.istrue(issparse([l, l]));
% a non-empty operand gives the shape
asserts.isequal(full([x; sparse(1)]), 1);
asserts.isequal(full([y; sparse([1 2])]), [1 2]);
asserts.isequal(full([w, sparse([1; 2])]), [1; 2]);
asserts.isequal(size([y; sparse([1 2 3])]), [1 3]);
%=============================================================================
% comparisons with an empty sparse operand are sparse logical
results = {x == x, x ~= x, x < x, x <= x, x > x, x >= x, x == 1, 1 == x, x == zeros(1, 0), ...
  zeros(1, 0) == x, l == l, x == sparse(1)};
for k = 1:numel(results)
  asserts.isequal(size(results{k}), [1 0]);
  asserts.isequal(class(results{k}), 'logical');
  asserts.istrue(issparse(results{k}));
end
asserts.istrue(issparse(z == z));
asserts.isequal(size(y == y), [0 2]);
asserts.istrue(issparse(y == y));
asserts.isfalse(issparse(zeros(1, 0) == zeros(1, 0)));
%=============================================================================
