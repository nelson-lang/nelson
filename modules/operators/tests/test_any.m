%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('any'), 2);
asserts.isequal(nargout('any'), 1);
%=============================================================================
asserts.isequal(any([false false true]), true);
asserts.isequal(any([false false true], 2), true);
asserts.isequal(any([0 0; 1 0]), logical([true false]));
%=============================================================================
asserts.isequal(any(zeros(0, 3), 1), [false false false]);
%=============================================================================
S = sparse(single([0 0; 2 + 1i 3]));
R = any(S);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'logical');
asserts.isequal(full(R), logical([true true]));
R = any(S, 2);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'logical');
asserts.isequal(full(R), logical([false; true]));
asserts.isequal(any(sparse(single([0 1 + 1i]))), true);
asserts.isequal(any(sparse(single([0 0]))), false);
%=============================================================================
% Reducing along a singleton dimension is a no-op (does not collapse to a scalar).
asserts.isequal(any([1; 0; 1], 2), [true; false; true]);
asserts.isequal(any([1 0 1], 1), [true false true]);
asserts.isequal(any([1; 0; 1], 1), true);
asserts.isequal(any([1 0 1], 2), true);
%=============================================================================
% N-D reduction along a dimension
L = reshape(1:24, 2, 3, 4) > 10;
assert_isequal(size(any(L, 1)), [1 3 4]);
assert_isequal(any(L, 3), true(2, 3));
R = any(L, 2);
assert_isequal(size(R), [2 1 4]);
%=============================================================================
% vector of dimensions (vecdim)
L = reshape(1:24, 2, 3, 4) > 10;
assert_isequal(any(L, [1 2]), reshape([false true true true], 1, 1, 4));
assert_isequal(any(L, [1 2 3]), true);
%=============================================================================
% NaN counts as false in 'any' (a NaN element does not make the result true),
% while a finite nonzero and Inf remain true.
assert_isequal(any(NaN), false);
assert_isequal(any([NaN 0]), false);
assert_isequal(any([NaN 1]), true);
assert_isequal(any([Inf 0]), true);
assert_isequal(any([NaN; 0]), false);
assert_isequal(any([NaN 0 NaN]), false);
assert_isequal(any(single([NaN 0])), false);
assert_isequal(any(single([NaN 1])), true);
%=============================================================================
% NaN in 'any' along matrix dimensions and 'all' form.
M = [1 0 NaN; 0 0 2];
assert_isequal(any(M), logical([true false true]));
assert_isequal(any(M, 2), logical([true; true]));
assert_isequal(any(M, 'all'), true);
%=============================================================================
% NaN in 'any' for an N-D array reduced along a dimension.
A(:, :, 1) = [NaN 0; 1 0];
A(:, :, 2) = [0 0; NaN NaN];
assert_isequal(any(A, 3), logical([false false; true false]));
%=============================================================================
% A complex element is false in 'any' when either part is NaN.
assert_isequal(any(complex(NaN, 0)), false);
assert_isequal(any(complex(NaN, 1)), false);
assert_isequal(any(complex(1, NaN)), false);
assert_isequal(any(complex(NaN, NaN)), false);
assert_isequal(any(complex(3, 4)), true);
assert_isequal(any([complex(NaN, 1) complex(0, 0)]), false);
assert_isequal(any([complex(NaN, 1) complex(2, 3)]), true);
%=============================================================================
% 'all' is unchanged: NaN is treated as true (nonzero).
assert_isequal(all(NaN), true);
assert_isequal(all([NaN 1]), true);
assert_isequal(all([NaN 0]), false);
assert_isequal(all(complex(1, NaN)), true);
%=============================================================================
