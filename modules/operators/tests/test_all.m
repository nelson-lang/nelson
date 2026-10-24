%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('all'), 2);
asserts.isequal(nargout('all'), 1);
%=============================================================================
asserts.isequal(all([true true false]), false);
asserts.isequal(all([true true false], 2), false);
asserts.isequal(all([1 1; 1 0]), logical([true false]));
%=============================================================================
S = sparse(single([1 0; 2 + 1i 3]));
R = all(S);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'logical');
asserts.isequal(full(R), logical([true false]));
R = all(S, 2);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'logical');
asserts.isequal(full(R), logical([false; true]));
asserts.isequal(all(sparse(single([1 + 1i 2]))), true);
%=============================================================================
% Reducing along a singleton dimension is a no-op (does not collapse to a scalar).
asserts.isequal(all([1; 1; 0], 2), [true; true; false]);
asserts.isequal(all([1 1 0], 1), [true true false]);
asserts.isequal(all([1; 1; 0], 1), false);
%=============================================================================
% N-D reduction along a dimension
A = reshape(1:24, 2, 3, 4);
assert_isequal(size(all(A > 0, 1)), [1 3 4]);
assert_isequal(all(A > 20, 3), false(2, 3));
%=============================================================================
% vector of dimensions (vecdim)
A = reshape(1:24, 2, 3, 4);
assert_isequal(all(A > 0, [1 3]), true(1, 3));
assert_isequal(all(A > 10, [1 2]), reshape([false false true true], 1, 1, 4));
%=============================================================================
