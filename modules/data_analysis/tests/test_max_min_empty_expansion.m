%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Two-argument max and min with an empty operand follow implicit expansion:
% the result is an empty of the expanded size, never the other operand.
%=============================================================================
fns = {@max, @min};
for k = 1:numel(fns)
  f = fns{k};
  assert_isequal(f(zeros(0, 3), 0), zeros(0, 3));
  assert_isequal(f(0, zeros(0, 3)), zeros(0, 3));
  assert_isequal(f(zeros(0, 3), zeros(1, 3)), zeros(0, 3));
  assert_isequal(f(zeros(1, 3), zeros(0, 3)), zeros(0, 3));
  assert_isequal(f(zeros(3, 0), zeros(3, 1)), zeros(3, 0));
  assert_isequal(f(zeros(0, 3), zeros(0, 3)), zeros(0, 3));
  assert_isequal(f([], 5), []);
  assert_isequal(f(5, []), []);
  assert_isequal(f(zeros(0, 3, 2), 1), zeros(0, 3, 2));
  % classes
  assert_isequal(f(int8(zeros(0, 3)), 0), int8(zeros(0, 3)));
  assert_isequal(f(0, uint16(zeros(3, 0))), uint16(zeros(3, 0)));
  assert_isequal(f(zeros(0, 3, 'single'), 1), zeros(0, 3, 'single'));
  assert_isequal(f(single(1), zeros(0, 3)), zeros(0, 3, 'single'));
  assert_isequal(f(false(0, 2), true), false(0, 2));
  assert_isequal(f(char(zeros(0, 2)), 'a'), zeros(0, 2));
  R = f(zeros(0, 3), 1i);
  assert_isequal(size(R), [0 3]);
  assert_isequal(class(R), 'double');
  % sparse
  R = f(sparse(zeros(0, 3)), 0);
  assert_istrue(issparse(R));
  assert_isequal(size(R), [0 3]);
  % nanflag
  assert_isequal(f(zeros(0, 3), 5, 'omitnan'), zeros(0, 3));
  assert_isequal(f(zeros(0, 3), 5, 'includenan'), zeros(0, 3));
  % incompatible sizes still error
  assert_checkerror('f([], [1 2 3])', [_('Size mismatch on arguments to arithmetic operator'), ' ', func2str(f)]);
end
%=============================================================================
% nanflag with implicit expansion (non-empty)
assert_isequal(max([1 NaN 3], 2, 'omitnan'), [2 2 3]);
assert_isequal(max([1 NaN 3], 2, 'includenan'), [2 NaN 3]);
assert_isequal(min([1 NaN 3], 2, "omitnan"), [1 2 2]);
assert_isequal(min([1; NaN], [0 5], 'includenan'), [0 1; NaN NaN]);
assert_isequal(max(2, [1 NaN 3], 'omitnan'), [2 2 3]);
% [] (0x0) as second argument keeps its placeholder meaning
assert_isequal(max([1 NaN 3], [], 'omitnan'), 3);
assert_isequal(min([1 NaN 3], [], 'includenan'), NaN);
%=============================================================================
% sparse complex follows the same sparsity rule as sparse real
S = sparse([0, 1 + 2i; 0, 0]);
Sr = sparse([0, 1; 0, 0]);
E = sparse(complex(zeros(0, 3)));
assert_istrue(issparse(max(S, 0)));
assert_istrue(issparse(min(S, 5)));
assert_istrue(issparse(max(S, sparse(0))));
assert_istrue(issparse(max(S, S)));
assert_isfalse(issparse(max(S, 5)));
assert_isfalse(issparse(max(S, full(S))));
assert_isequal(full(max(S, 0)), full(S));
assert_isequal(full(min(S, 5)), full(S));
assert_isequal(issparse(max(E, 0)), issparse(max(sparse(zeros(0, 3)), 0)));
assert_isequal(issparse(min(E, 5)), issparse(min(sparse(zeros(0, 3)), 5)));
assert_istrue(issparse(max(E, E)));
assert_isequal(size(max(E, 0)), [0 3]);
assert_isequal(issparse(max(Sr, 0)), issparse(max(S, 0)));
%=============================================================================
