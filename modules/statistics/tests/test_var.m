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
asserts.isequal(nargin('var'), 4);
asserts.isequal(nargout('var'), 2);
%=============================================================================
R = var([1, 2, 3], 0, 1);
REF = [0 0 0];
asserts.isequal(R, REF);
%=============================================================================
A = [4 -7 3; 1 4 -2; 10 7 9];
R = var(A);
REF = [21.0000 54.3333 30.3333];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
A = [4 -2 1; 9 5 7];
R = var(A, 0, 1);
REF = [12.5000 24.5000 18.0000];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
A = [4 -2 1; 9 5 7];
R = var(A, 0, 2);
REF = [9; 4];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
asserts.checkerror('var()', _('Not enough input arguments.'))
%=============================================================================
asserts.isequal(var(36), 0);
%=============================================================================
asserts.isequal(var([1 2 3]), 1);
%=============================================================================
% 'all' option
assert_isequal(var([1 2 3; 4 5 6], 0, 'all'), 3.5);
assert_isapprox(var([1 2 3; 4 5 6], 1, 'all'), 35 / 12, 1e-10);
assert_isapprox(var([1 NaN; 3 4], 0, 'all', 'omitnan'), 7 / 3, 1e-10);
%=============================================================================
% var of a matrix with no rows must not crash and returns NaN per column
assert_isequal(var(zeros(0, 3)), [NaN NaN NaN]);
assert_isequal(size(var(zeros(0, 3))), [1 3]);
R = var(single(zeros(0, 2)));
assert_isequal(class(R), 'single');
assert_isequal(R, single([NaN NaN]));
assert_isequal(std(zeros(0, 3)), [NaN NaN NaN]);
%=============================================================================
% vector of dimensions (vecdim)
A = reshape(1:24, [2 3 4]);
assert_isequal(size(var(A, 0, [1 2])), [1 1 4]);
assert_isapprox(var(A, 0, [1 2]), reshape([3.5 3.5 3.5 3.5], [1 1 4]), 1e-10);
assert_isequal(size(var(A, 0, [1 3])), [1 3]);
assert_isapprox(var(A, 0, [1 3]), [362 / 7 362 / 7 362 / 7], 1e-10);
assert_isequal(size(var(A, 0, [2 3])), [2 1]);
assert_isapprox(var(A, 0, [2 3]), [52; 52], 1e-10);
assert_isapprox(var(A, 1, [1 2]), reshape([35 / 12 35 / 12 35 / 12 35 / 12], [1 1 4]), 1e-10);
assert_isapprox(var(A, 0, [1 2 3]), 50, 1e-10);
assert_isequal(var(A, 0, [1 2 3]), var(A, 0, 'all'));
% omitnan combined with a vector of dimensions
C = [1 NaN; 3 4];
assert_isapprox(var(C, 0, [1 2], 'omitnan'), 7 / 3, 1e-10);
% a vector that lists a single effective dimension behaves like a scalar dim
assert_isequal(var(A, 0, [2]), var(A, 0, 2));
% a weight vector cannot be combined with a vector of dimensions
asserts.checkerror('var(reshape(1:24, [2 3 4]), [1 1], [1 2])', _('A weight vector is not supported with a vector of dimensions.'));
%=============================================================================
