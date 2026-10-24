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
%=============================================================================
% max/min of an empty array along a dimension: a dimension of length 0 keeps
% the size of A, any other operating dimension is reduced to 1. The index
% output has the size of M.
%=============================================================================
for f = {@max, @min}
  fn = f{1};
  asserts.isequal(fn(zeros(0, 3)), zeros(0, 3));
  asserts.isequal(fn(zeros(0, 3), [], 1), zeros(0, 3));
  asserts.isequal(fn(zeros(0, 3), [], 2), zeros(0, 1));
  asserts.isequal(fn(zeros(0, 3), [], 3), zeros(0, 3));
  asserts.isequal(fn(zeros(3, 0)), zeros(1, 0));
  asserts.isequal(fn(zeros(3, 0), [], 1), zeros(1, 0));
  asserts.isequal(fn(zeros(3, 0), [], 2), zeros(3, 0));
  asserts.isequal(fn(zeros(1, 0)), zeros(1, 0));
  asserts.isequal(fn(zeros(0, 1)), zeros(0, 1));
  asserts.isequal(fn(zeros(0, 3, 2)), zeros(0, 3, 2));
  asserts.isequal(fn(zeros(2, 0, 3)), zeros(1, 0, 3));
  asserts.isequal(fn(zeros(2, 0, 3), [], 3), zeros(2, 0));
  asserts.isequal(fn(zeros(0, 3), [], 2, 'omitnan'), zeros(0, 1));
  asserts.isequal(fn(zeros(0, 3), [], 2, 'includenan'), zeros(0, 1));
  asserts.isequal(fn([]), []);
  asserts.isequal(fn([], [], 1), []);
  asserts.isequal(fn([], [], 2), []);
  asserts.isequal(fn([], [], 3), []);
  asserts.isequal(fn([], [], 1, 'omitnan'), []);
  % class is kept
  asserts.isequal(fn(int8(zeros(3, 0))), int8(zeros(1, 0)));
  asserts.isequal(fn(single(zeros(0, 3)), [], 2), single(zeros(0, 1)));
  asserts.isequal(fn(false(3, 0)), false(1, 0));
  asserts.isequal(fn(char(zeros(0, 3)), [], 2), char(zeros(0, 1)));
  % index output
  [m, i] = fn(zeros(0, 3));
  asserts.isequal(m, zeros(0, 3));
  asserts.isequal(i, zeros(0, 3));
  [m, i] = fn(zeros(3, 0));
  asserts.isequal(m, zeros(1, 0));
  asserts.isequal(i, zeros(1, 0));
  [m, i] = fn(zeros(0, 3), [], 2);
  asserts.isequal(m, zeros(0, 1));
  asserts.isequal(i, zeros(0, 1));
  [m, i] = fn([]);
  asserts.isequal(m, []);
  asserts.isequal(i, []);
  [m, i] = fn(int8(zeros(2, 0, 3)));
  asserts.isequal(m, int8(zeros(1, 0, 3)));
  asserts.isequal(i, zeros(1, 0, 3));
  % sparse follows the same rule and stays sparse
  asserts.isequal(fn(sparse(zeros(0, 3)), [], 1), sparse(zeros(0, 3)));
  asserts.isequal(fn(sparse(zeros(0, 3)), [], 2), sparse(zeros(0, 1)));
  asserts.isequal(fn(sparse(zeros(3, 0))), sparse(zeros(1, 0)));
  asserts.isequal(fn(sparse(zeros(3, 0)), [], 2), sparse(zeros(3, 0)));
  asserts.isequal(fn(sparse([]), [], 2), sparse([]));
  asserts.istrue(issparse(fn(sparse(zeros(0, 3)), [], 2)));
end
%=============================================================================
